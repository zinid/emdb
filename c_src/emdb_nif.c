#include "emdb.h"
#include <erl_nif.h>
#include <errno.h>
#include <lmdb.h>
#include <stdatomic.h>
#include <string.h>

struct env_state {
  atomic_flag is_closed;
  MDB_env *mdb_env;
};

struct txn_state {
  atomic_flag is_closed;
  MDB_txn *mdb_txn;
  struct env_state *env_state;
};

struct cursor_state {
  atomic_flag is_closed;
  MDB_cursor *mdb_cursor;
  struct txn_state *txn_state;
};

static ErlNifResourceType *env_rtype = NULL;
static ErlNifResourceType *txn_rtype = NULL;
static ErlNifResourceType *cursor_rtype = NULL;

static struct env_state *alloc_env_state(ErlNifEnv *env) {
  struct env_state *env_state =
      enif_alloc_resource(env_rtype, sizeof(*env_state));
  if (env_state) {
    memset(env_state, 0, sizeof(*env_state));
    atomic_flag_clear(&env_state->is_closed);
  }

  return env_state;
}

static struct txn_state *alloc_txn_state(ErlNifEnv *env) {
  struct txn_state *txn_state =
      enif_alloc_resource(txn_rtype, sizeof(*txn_state));
  if (txn_state) {
    memset(txn_state, 0, sizeof(*txn_state));
    atomic_flag_clear(&txn_state->is_closed);
  }

  return txn_state;
}

static struct cursor_state *alloc_cursor_state(ErlNifEnv *env) {
  struct cursor_state *cursor_state =
      enif_alloc_resource(cursor_rtype, sizeof(*cursor_state));
  if (cursor_state) {
    memset(cursor_state, 0, sizeof(*cursor_state));
    atomic_flag_clear(&cursor_state->is_closed);
  }

  return cursor_state;
}

static void destroy_env_state(ErlNifEnv *env, void *data) {
  struct env_state *env_state = data;

  if (atomic_flag_test_and_set(&env_state->is_closed) == 0)
    mdb_env_close(env_state->mdb_env);
}

static void destroy_txn_state(ErlNifEnv *env, void *data) {
  struct txn_state *txn_state = data;

  if (atomic_flag_test_and_set(&txn_state->is_closed) == 0) {
    mdb_txn_abort(txn_state->mdb_txn);
    if (txn_state->env_state)
      enif_release_resource(txn_state->env_state);
  }
}

static void destroy_cursor_state(ErlNifEnv *env, void *data) {
  struct cursor_state *cursor_state = data;

  if (atomic_flag_test_and_set(&cursor_state->is_closed) == 0) {
    mdb_cursor_close(cursor_state->mdb_cursor);
    if (cursor_state->txn_state)
      enif_release_resource(cursor_state->txn_state);
  }
}

static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info) {
  init_atoms(env);

  ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  env_rtype = enif_open_resource_type(env, NULL, "env_state", destroy_env_state,
                                      flags, NULL);
  txn_rtype = enif_open_resource_type(env, NULL, "txn_state", destroy_txn_state,
                                      flags, NULL);
  cursor_rtype = enif_open_resource_type(env, NULL, "cursor_state",
                                         destroy_cursor_state, flags, NULL);
  return 0;
}

static ERL_NIF_TERM make_errno(ErlNifEnv *env, int err) {
  return enif_make_tuple2(env, atom_error, errtag(env, err));
}

static ERL_NIF_TERM make_ok(ErlNifEnv *env, ERL_NIF_TERM term) {
  return enif_make_tuple2(env, atom_ok, term);
}

static ERL_NIF_TERM encode_flag_nif(ErlNifEnv *env, int argc,
                                    const ERL_NIF_TERM argv[]) {
  unsigned int flag = encode_flag(argv[0]);
  return flag ? enif_make_uint(env, flag) : enif_make_badarg(env);
}

static ERL_NIF_TERM encode_errtag_nif(ErlNifEnv *env, int argc,
                                      const ERL_NIF_TERM argv[]) {
  int err = encode_errtag(argv[0]);
  return err ? enif_make_int(env, err) : enif_make_badarg(env);
}

static ERL_NIF_TERM encode_cursor_op_nif(ErlNifEnv *env, int argc,
                                         const ERL_NIF_TERM argv[]) {
  MDB_cursor_op op = encode_cursor_op(argv[0]);
  return op == (MDB_cursor_op)-1 ? enif_make_badarg(env)
                                 : enif_make_int(env, op);
}

static ERL_NIF_TERM strerror_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  int err;
  if (enif_get_int(env, argv[0], &err) == 0)
    return enif_make_badarg(env);

  return enif_make_string(env, mdb_strerror(err), ERL_NIF_LATIN1);
}

static ERL_NIF_TERM version_nif(ErlNifEnv *env, int argc,
                                const ERL_NIF_TERM argv[]) {
  int major, minor, patch;
  mdb_version(&major, &minor, &patch);

  return enif_make_tuple3(env, enif_make_int(env, major),
                          enif_make_int(env, minor), enif_make_int(env, patch));
}

static ERL_NIF_TERM env_create_nif(ErlNifEnv *env, int argc,
                                   const ERL_NIF_TERM argv[]) {
  struct env_state *env_state = alloc_env_state(env);
  if (env_state == NULL)
    return make_errno(env, ENOMEM);

  ERL_NIF_TERM ref = enif_make_resource(env, env_state);
  enif_release_resource(env_state);

  int err = mdb_env_create(&env_state->mdb_env);
  if (err)
    return make_errno(env, err);

  return make_ok(env, ref);
}

static ERL_NIF_TERM env_open_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  struct env_state *env_state;
  ErlNifBinary path_bin;
  unsigned int flags;
  mdb_mode_t mode;

  if (enif_get_resource(env, argv[0], env_rtype, (void **)&env_state) == 0)
    return enif_make_badarg(env);

  if (enif_inspect_iolist_as_binary(env, argv[1], &path_bin) == 0)
    return enif_make_badarg(env);

  if (enif_get_uint(env, argv[2], &flags) == 0)
    return enif_make_badarg(env);

  if (enif_get_uint(env, argv[3], &mode) == 0)
    return enif_make_badarg(env);

  char path[path_bin.size + 1];
  memcpy(path, path_bin.data, path_bin.size);
  path[path_bin.size] = 0;

  int err = mdb_env_open(env_state->mdb_env, path, flags, mode);
  if (err)
    return make_errno(env, err);

  return atom_ok;
}

static ERL_NIF_TERM env_close_nif(ErlNifEnv *env, int argc,
                                  const ERL_NIF_TERM argv[]) {
  struct env_state *env_state;
  if (enif_get_resource(env, argv[0], env_rtype, (void **)&env_state) == 0)
    return enif_make_badarg(env);

  if (atomic_flag_test_and_set(&env_state->is_closed) == 0) {
    mdb_env_close(env_state->mdb_env);
    return atom_true;
  }

  return atom_false;
}

static ERL_NIF_TERM env_set_mapsize_nif(ErlNifEnv *env, int argc,
                                        const ERL_NIF_TERM argv[]) {
  struct env_state *env_state;
  size_t size;

  if (enif_get_resource(env, argv[0], env_rtype, (void **)&env_state) == 0)
    return enif_make_badarg(env);

  if (enif_get_uint64(env, argv[1], &size) == 0)
    return enif_make_badarg(env);

  int err = mdb_env_set_mapsize(env_state->mdb_env, size);

  if (err)
    return make_errno(env, err);

  return atom_ok;
}

static ERL_NIF_TERM env_set_maxdbs_nif(ErlNifEnv *env, int argc,
                                       const ERL_NIF_TERM argv[]) {
  struct env_state *env_state;
  MDB_dbi maxdbs;

  if (enif_get_resource(env, argv[0], env_rtype, (void **)&env_state) == 0)
    return enif_make_badarg(env);

  if (enif_get_uint(env, argv[1], &maxdbs) == 0)
    return enif_make_badarg(env);

  int err = mdb_env_set_maxdbs(env_state->mdb_env, maxdbs);

  if (err)
    return make_errno(env, err);

  return atom_ok;
}

static ERL_NIF_TERM env_set_maxreaders_nif(ErlNifEnv *env, int argc,
                                           const ERL_NIF_TERM argv[]) {
  struct env_state *env_state;
  unsigned int maxreaders;

  if (enif_get_resource(env, argv[0], env_rtype, (void **)&env_state) == 0)
    return enif_make_badarg(env);

  if (enif_get_uint(env, argv[1], &maxreaders) == 0)
    return enif_make_badarg(env);

  int err = mdb_env_set_maxreaders(env_state->mdb_env, maxreaders);

  if (err)
    return make_errno(env, err);

  return atom_ok;
}

static ERL_NIF_TERM txn_begin_nif(ErlNifEnv *env, int argc,
                                  const ERL_NIF_TERM argv[]) {
  struct env_state *env_state;
  unsigned int flags;

  if (enif_get_resource(env, argv[0], env_rtype, (void **)&env_state) == 0)
    return enif_make_badarg(env);

  if (enif_get_uint(env, argv[1], &flags) == 0)
    return enif_make_badarg(env);

  struct txn_state *txn_state = alloc_txn_state(env);
  if (txn_state == NULL)
    return make_errno(env, ENOMEM);

  ERL_NIF_TERM ref = enif_make_resource(env, txn_state);
  enif_release_resource(txn_state);

  int err = mdb_txn_begin(env_state->mdb_env, NULL, flags, &txn_state->mdb_txn);
  if (err)
    return make_errno(env, err);

  txn_state->env_state = env_state;
  enif_keep_resource(env_state);

  return make_ok(env, ref);
}

static ERL_NIF_TERM txn_commit_nif(ErlNifEnv *env, int argc,
                                   const ERL_NIF_TERM argv[]) {
  struct txn_state *txn_state;

  if (enif_get_resource(env, argv[0], txn_rtype, (void **)&txn_state) == 0)
    return enif_make_badarg(env);

  if (atomic_flag_test_and_set(&txn_state->is_closed) == 0) {
    int err = mdb_txn_commit(txn_state->mdb_txn);
    if (txn_state->env_state)
      enif_release_resource(txn_state->env_state);
    if (err)
      return make_errno(env, err);
  }

  return atom_ok;
}

static ERL_NIF_TERM txn_abort_nif(ErlNifEnv *env, int argc,
                                  const ERL_NIF_TERM argv[]) {
  struct txn_state *txn_state;

  if (enif_get_resource(env, argv[0], txn_rtype, (void **)&txn_state) == 0)
    return enif_make_badarg(env);

  destroy_txn_state(env, txn_state);

  return atom_ok;
}

static ERL_NIF_TERM dbi_open_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  struct txn_state *txn_state;
  unsigned int flags;
  MDB_dbi dbi;
  char name[256];

  if (enif_get_resource(env, argv[0], txn_rtype, (void **)&txn_state) == 0)
    return enif_make_badarg(env);

  if (enif_get_atom(env, argv[1], name, sizeof(name), ERL_NIF_LATIN1) == 0)
    return enif_make_badarg(env);

  if (enif_get_uint(env, argv[2], &flags) == 0)
    return enif_make_badarg(env);

  int err = mdb_dbi_open(txn_state->mdb_txn, name, flags, &dbi);
  if (err)
    return make_errno(env, err);

  return make_ok(env, enif_make_uint(env, dbi));
}

static ERL_NIF_TERM dbi_close_nif(ErlNifEnv *env, int argc,
                                  const ERL_NIF_TERM argv[]) {
  struct env_state *env_state;
  MDB_dbi dbi;

  if (enif_get_resource(env, argv[0], env_rtype, (void **)&env_state) == 0)
    return enif_make_badarg(env);

  if (enif_get_uint(env, argv[1], &dbi) == 0)
    return enif_make_badarg(env);

  mdb_dbi_close(env_state->mdb_env, dbi);

  return atom_ok;
}

static ERL_NIF_TERM put_nif(ErlNifEnv *env, int argc,
                            const ERL_NIF_TERM argv[]) {
  struct txn_state *txn_state;
  MDB_dbi dbi;
  MDB_val key, val;
  unsigned int flags;
  ErlNifBinary key_bin, val_bin;

  if (enif_get_resource(env, argv[0], txn_rtype, (void **)&txn_state) == 0)
    return enif_make_badarg(env);

  if (enif_get_uint(env, argv[1], &dbi) == 0)
    return enif_make_badarg(env);

  if (enif_inspect_binary(env, argv[2], &key_bin) == 0)
    return enif_make_badarg(env);

  if (enif_inspect_binary(env, argv[3], &val_bin) == 0)
    return enif_make_badarg(env);

  if (enif_get_uint(env, argv[4], &flags) == 0)
    return enif_make_badarg(env);

  key.mv_size = key_bin.size;
  key.mv_data = key_bin.data;

  val.mv_size = val_bin.size;
  val.mv_data = val_bin.data;

  int err = mdb_put(txn_state->mdb_txn, dbi, &key, &val, flags);
  if (err)
    return make_errno(env, err);

  return atom_ok;
}

static ERL_NIF_TERM get_nif(ErlNifEnv *env, int argc,
                            const ERL_NIF_TERM argv[]) {
  struct txn_state *txn_state;
  MDB_dbi dbi;
  MDB_val key, val;
  ErlNifBinary key_bin;

  if (enif_get_resource(env, argv[0], txn_rtype, (void **)&txn_state) == 0)
    return enif_make_badarg(env);

  if (enif_get_uint(env, argv[1], &dbi) == 0)
    return enif_make_badarg(env);

  if (enif_inspect_binary(env, argv[2], &key_bin) == 0)
    return enif_make_badarg(env);

  key.mv_size = key_bin.size;
  key.mv_data = key_bin.data;

  int err = mdb_get(txn_state->mdb_txn, dbi, &key, &val);
  if (err)
    return make_errno(env, err);

  ERL_NIF_TERM val_term;
  unsigned char *buf = enif_make_new_binary(env, val.mv_size, &val_term);
  memcpy(buf, val.mv_data, val.mv_size);

  return make_ok(env, val_term);
}

static ERL_NIF_TERM del_nif(ErlNifEnv *env, int argc,
                            const ERL_NIF_TERM argv[]) {
  struct txn_state *txn_state;
  MDB_dbi dbi;
  MDB_val key, val;
  ErlNifBinary key_bin, val_bin;

  if (enif_get_resource(env, argv[0], txn_rtype, (void **)&txn_state) == 0)
    return enif_make_badarg(env);

  if (enif_get_uint(env, argv[1], &dbi) == 0)
    return enif_make_badarg(env);

  if (enif_inspect_binary(env, argv[2], &key_bin) == 0)
    return enif_make_badarg(env);

  if (enif_inspect_binary(env, argv[3], &val_bin) == 0)
    return enif_make_badarg(env);

  key.mv_size = key_bin.size;
  key.mv_data = key_bin.data;

  val.mv_size = val_bin.size;
  val.mv_data = val_bin.data;

  int err = mdb_del(txn_state->mdb_txn, dbi, &key, &val);
  if (err)
    return make_errno(env, err);

  return atom_ok;
}

static ERL_NIF_TERM cursor_open_nif(ErlNifEnv *env, int argc,
                                    const ERL_NIF_TERM argv[]) {
  struct txn_state *txn_state;
  MDB_dbi dbi;

  if (enif_get_resource(env, argv[0], txn_rtype, (void **)&txn_state) == 0)
    return enif_make_badarg(env);

  if (enif_get_uint(env, argv[1], &dbi) == 0)
    return enif_make_badarg(env);

  struct cursor_state *cursor_state = alloc_cursor_state(env);
  if (cursor_state == NULL)
    return make_errno(env, ENOMEM);

  ERL_NIF_TERM ref = enif_make_resource(env, cursor_state);
  enif_release_resource(cursor_state);

  int err = mdb_cursor_open(txn_state->mdb_txn, dbi, &cursor_state->mdb_cursor);
  if (err)
    return make_errno(env, err);

  cursor_state->txn_state = txn_state;
  enif_keep_resource(txn_state);

  return make_ok(env, ref);
}

static ERL_NIF_TERM cursor_close_nif(ErlNifEnv *env, int argc,
                                     const ERL_NIF_TERM argv[]) {
  struct cursor_state *cursor_state;

  if (enif_get_resource(env, argv[0], cursor_rtype, (void **)&cursor_state) ==
      0)
    return enif_make_badarg(env);

  destroy_cursor_state(env, cursor_state);

  return atom_ok;
}

static ERL_NIF_TERM cursor_get_nif(ErlNifEnv *env, int argc,
                                   const ERL_NIF_TERM argv[]) {
  struct cursor_state *cursor_state;
  MDB_cursor_op op;
  MDB_val key, val;
  ERL_NIF_TERM key_term, val_term;

  if (enif_get_resource(env, argv[0], cursor_rtype, (void **)&cursor_state) ==
      0)
    return enif_make_badarg(env);

  if (enif_get_uint(env, argv[1], &op) == 0)
    return enif_make_badarg(env);

  int err = mdb_cursor_get(cursor_state->mdb_cursor, &key, &val, op);
  if (err)
    return make_errno(env, err);

  unsigned char *key_buf = enif_make_new_binary(env, key.mv_size, &key_term);
  memcpy(key_buf, key.mv_data, key.mv_size);

  unsigned char *val_buf = enif_make_new_binary(env, val.mv_size, &val_term);
  memcpy(val_buf, val.mv_data, val.mv_size);

  return make_ok(env, enif_make_tuple2(env, key_term, val_term));
}

static ErlNifFunc nif_funcs[] = {
    {"encode_flag", 1, encode_flag_nif, 0},
    {"encode_errtag", 1, encode_errtag_nif, 0},
    {"encode_cursor_op", 1, encode_cursor_op_nif, 0},
    {"strerror", 1, strerror_nif, 0},
    {"version", 0, version_nif, 0},
    {"env_create", 0, env_create_nif, 0},
    {"env_open", 4, env_open_nif, 0},
    {"env_close", 1, env_close_nif, 0},
    {"env_set_mapsize", 2, env_set_mapsize_nif, 0},
    {"env_set_maxdbs", 2, env_set_maxdbs_nif, 0},
    {"env_set_maxreaders", 2, env_set_maxreaders_nif, 0},
    {"txn_begin", 2, txn_begin_nif, 0},
    {"txn_commit", 1, txn_commit_nif, 0},
    {"txn_abort", 1, txn_abort_nif, 0},
    {"dbi_open", 3, dbi_open_nif, 0},
    {"dbi_close", 2, dbi_close_nif, 0},
    {"put", 5, put_nif, 0},
    {"get", 3, get_nif, 0},
    {"del", 4, del_nif, 0},
    {"cursor_open", 2, cursor_open_nif, 0},
    {"cursor_close", 1, cursor_close_nif, 0},
    {"cursor_get", 2, cursor_get_nif, 0}};

ERL_NIF_INIT(emdb_nif, nif_funcs, load, NULL, NULL, NULL)
