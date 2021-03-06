#ifndef EMDB_NIF_H
#define EMDB_NIF_H

#include <erl_nif.h>
#include <lmdb.h>

// Common
ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_errno;
ERL_NIF_TERM atom_true;
ERL_NIF_TERM atom_false;

// Flags
ERL_NIF_TERM atom_fixedmap;
ERL_NIF_TERM atom_nosubdir;
ERL_NIF_TERM atom_rdonly;
ERL_NIF_TERM atom_writemap;
ERL_NIF_TERM atom_nometasync;
ERL_NIF_TERM atom_nosync;
ERL_NIF_TERM atom_mapasync;
ERL_NIF_TERM atom_notls;
ERL_NIF_TERM atom_nolock;
ERL_NIF_TERM atom_nordahead;
ERL_NIF_TERM atom_nomeminit;
ERL_NIF_TERM atom_reversekey;
ERL_NIF_TERM atom_dupsort;
ERL_NIF_TERM atom_integerkey;
ERL_NIF_TERM atom_dupfixed;
ERL_NIF_TERM atom_get_multiple;
ERL_NIF_TERM atom_next_multiple;
ERL_NIF_TERM atom_integerdup;
ERL_NIF_TERM atom_integerkey;
ERL_NIF_TERM atom_reversedup;
ERL_NIF_TERM atom_create;

// errno
ERL_NIF_TERM atom_e2big;
ERL_NIF_TERM atom_eacces;
ERL_NIF_TERM atom_eaddrinuse;
ERL_NIF_TERM atom_eaddrnotavail;
ERL_NIF_TERM atom_eadv;
ERL_NIF_TERM atom_eafnosupport;
ERL_NIF_TERM atom_eagain;
ERL_NIF_TERM atom_ealready;
ERL_NIF_TERM atom_ebade;
ERL_NIF_TERM atom_ebadf;
ERL_NIF_TERM atom_ebadfd;
ERL_NIF_TERM atom_ebadmsg;
ERL_NIF_TERM atom_ebadr;
ERL_NIF_TERM atom_ebadrqc;
ERL_NIF_TERM atom_ebadslt;
ERL_NIF_TERM atom_ebfont;
ERL_NIF_TERM atom_ebusy;
ERL_NIF_TERM atom_ecanceled;
ERL_NIF_TERM atom_echild;
ERL_NIF_TERM atom_echrng;
ERL_NIF_TERM atom_ecomm;
ERL_NIF_TERM atom_econnaborted;
ERL_NIF_TERM atom_econnrefused;
ERL_NIF_TERM atom_econnreset;
ERL_NIF_TERM atom_edeadlock;
ERL_NIF_TERM atom_edestaddrreq;
ERL_NIF_TERM atom_edom;
ERL_NIF_TERM atom_edotdot;
ERL_NIF_TERM atom_edquot;
ERL_NIF_TERM atom_eexist;
ERL_NIF_TERM atom_efault;
ERL_NIF_TERM atom_efbig;
ERL_NIF_TERM atom_ehostdown;
ERL_NIF_TERM atom_ehostunreach;
ERL_NIF_TERM atom_ehwpoison;
ERL_NIF_TERM atom_eidrm;
ERL_NIF_TERM atom_eilseq;
ERL_NIF_TERM atom_einprogress;
ERL_NIF_TERM atom_eintr;
ERL_NIF_TERM atom_einval;
ERL_NIF_TERM atom_eio;
ERL_NIF_TERM atom_eisconn;
ERL_NIF_TERM atom_eisdir;
ERL_NIF_TERM atom_eisnam;
ERL_NIF_TERM atom_ekeyexpired;
ERL_NIF_TERM atom_ekeyrejected;
ERL_NIF_TERM atom_ekeyrevoked;
ERL_NIF_TERM atom_el2hlt;
ERL_NIF_TERM atom_el2nsync;
ERL_NIF_TERM atom_el3hlt;
ERL_NIF_TERM atom_el3rst;
ERL_NIF_TERM atom_elibacc;
ERL_NIF_TERM atom_elibbad;
ERL_NIF_TERM atom_elibexec;
ERL_NIF_TERM atom_elibmax;
ERL_NIF_TERM atom_elibscn;
ERL_NIF_TERM atom_elnrng;
ERL_NIF_TERM atom_eloop;
ERL_NIF_TERM atom_emediumtype;
ERL_NIF_TERM atom_emfile;
ERL_NIF_TERM atom_emlink;
ERL_NIF_TERM atom_emsgsize;
ERL_NIF_TERM atom_emultihop;
ERL_NIF_TERM atom_enametoolong;
ERL_NIF_TERM atom_enavail;
ERL_NIF_TERM atom_enetdown;
ERL_NIF_TERM atom_enetreset;
ERL_NIF_TERM atom_enetunreach;
ERL_NIF_TERM atom_enfile;
ERL_NIF_TERM atom_enoano;
ERL_NIF_TERM atom_enobufs;
ERL_NIF_TERM atom_enocsi;
ERL_NIF_TERM atom_enodata;
ERL_NIF_TERM atom_enodev;
ERL_NIF_TERM atom_enoent;
ERL_NIF_TERM atom_enoexec;
ERL_NIF_TERM atom_enokey;
ERL_NIF_TERM atom_enolck;
ERL_NIF_TERM atom_enolink;
ERL_NIF_TERM atom_enomedium;
ERL_NIF_TERM atom_enomem;
ERL_NIF_TERM atom_enomsg;
ERL_NIF_TERM atom_enonet;
ERL_NIF_TERM atom_enopkg;
ERL_NIF_TERM atom_enoprotoopt;
ERL_NIF_TERM atom_enospc;
ERL_NIF_TERM atom_enosr;
ERL_NIF_TERM atom_enostr;
ERL_NIF_TERM atom_enosys;
ERL_NIF_TERM atom_enotblk;
ERL_NIF_TERM atom_enotconn;
ERL_NIF_TERM atom_enotdir;
ERL_NIF_TERM atom_enotempty;
ERL_NIF_TERM atom_enotnam;
ERL_NIF_TERM atom_enotrecoverable;
ERL_NIF_TERM atom_enotsock;
ERL_NIF_TERM atom_enotsup;
ERL_NIF_TERM atom_enotty;
ERL_NIF_TERM atom_enotuniq;
ERL_NIF_TERM atom_enxio;
ERL_NIF_TERM atom_eoverflow;
ERL_NIF_TERM atom_eownerdead;
ERL_NIF_TERM atom_eperm;
ERL_NIF_TERM atom_epfnosupport;
ERL_NIF_TERM atom_epipe;
ERL_NIF_TERM atom_eproto;
ERL_NIF_TERM atom_eprotonosupport;
ERL_NIF_TERM atom_eprototype;
ERL_NIF_TERM atom_erange;
ERL_NIF_TERM atom_eremchg;
ERL_NIF_TERM atom_eremote;
ERL_NIF_TERM atom_eremoteio;
ERL_NIF_TERM atom_erestart;
ERL_NIF_TERM atom_erfkill;
ERL_NIF_TERM atom_erofs;
ERL_NIF_TERM atom_eshutdown;
ERL_NIF_TERM atom_esocktnosupport;
ERL_NIF_TERM atom_espipe;
ERL_NIF_TERM atom_esrch;
ERL_NIF_TERM atom_esrmnt;
ERL_NIF_TERM atom_estale;
ERL_NIF_TERM atom_estrpipe;
ERL_NIF_TERM atom_etime;
ERL_NIF_TERM atom_etimedout;
ERL_NIF_TERM atom_etoomanyrefs;
ERL_NIF_TERM atom_etxtbsy;
ERL_NIF_TERM atom_euclean;
ERL_NIF_TERM atom_eunatch;
ERL_NIF_TERM atom_eusers;
ERL_NIF_TERM atom_exdev;
ERL_NIF_TERM atom_exfull;

// LMDB errors
ERL_NIF_TERM atom_keyexist;
ERL_NIF_TERM atom_notfound;
ERL_NIF_TERM atom_page_notfound;
ERL_NIF_TERM atom_corrupted;
ERL_NIF_TERM atom_panic;
ERL_NIF_TERM atom_version_mismatch;
ERL_NIF_TERM atom_invalid;
ERL_NIF_TERM atom_map_full;
ERL_NIF_TERM atom_dbs_full;
ERL_NIF_TERM atom_readers_full;
ERL_NIF_TERM atom_tls_full;
ERL_NIF_TERM atom_txn_full;
ERL_NIF_TERM atom_cursor_full;
ERL_NIF_TERM atom_page_full;
ERL_NIF_TERM atom_map_resized;
ERL_NIF_TERM atom_incompatible;
ERL_NIF_TERM atom_bad_rslot;
ERL_NIF_TERM atom_bad_txn;
ERL_NIF_TERM atom_bad_valsize;
ERL_NIF_TERM atom_bad_dbi;

// LMDB cursor op
ERL_NIF_TERM atom_first;
ERL_NIF_TERM atom_first_dup;
ERL_NIF_TERM atom_get_both;
ERL_NIF_TERM atom_get_both_range;
ERL_NIF_TERM atom_get_current;
ERL_NIF_TERM atom_get_multiple;
ERL_NIF_TERM atom_last;
ERL_NIF_TERM atom_last_dup;
ERL_NIF_TERM atom_next;
ERL_NIF_TERM atom_next_dup;
ERL_NIF_TERM atom_next_multiple;
ERL_NIF_TERM atom_next_nodup;
ERL_NIF_TERM atom_prev;
ERL_NIF_TERM atom_prev_dup;
ERL_NIF_TERM atom_prev_nodup;
ERL_NIF_TERM atom_set;
ERL_NIF_TERM atom_set_key;
ERL_NIF_TERM atom_set_range;

extern void init_atoms(ErlNifEnv *);
extern ERL_NIF_TERM errtag(ErlNifEnv *, int);
extern int encode_errtag(ERL_NIF_TERM);
extern unsigned int encode_flag(ERL_NIF_TERM);
extern MDB_cursor_op encode_cursor_op(ERL_NIF_TERM);

#endif // EMDB_NIF_H
