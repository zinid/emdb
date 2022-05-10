#include "emdb.h"
#include <erl_nif.h>
#include <errno.h>
#include <lmdb.h>

void init_atoms(ErlNifEnv *env) {
  // Common
  atom_ok = enif_make_atom(env, "ok");
  atom_error = enif_make_atom(env, "error");
  atom_errno = enif_make_atom(env, "errno");
  atom_true = enif_make_atom(env, "true");
  atom_false = enif_make_atom(env, "false");

  // Flags
  atom_fixedmap = enif_make_atom(env, "fixedmap");
  atom_nosubdir = enif_make_atom(env, "nosubdir");
  atom_rdonly = enif_make_atom(env, "rdonly");
  atom_writemap = enif_make_atom(env, "writemap");
  atom_nometasync = enif_make_atom(env, "nometasync");
  atom_nosync = enif_make_atom(env, "nosync");
  atom_mapasync = enif_make_atom(env, "mapasync");
  atom_notls = enif_make_atom(env, "notls");
  atom_nolock = enif_make_atom(env, "nolock");
  atom_nordahead = enif_make_atom(env, "nordahead");
  atom_nomeminit = enif_make_atom(env, "nomeminit");
  atom_reversekey = enif_make_atom(env, "reversekey");
  atom_dupsort = enif_make_atom(env, "dupsort");
  atom_integerkey = enif_make_atom(env, "integerkey");
  atom_dupfixed = enif_make_atom(env, "dupfixed");
  atom_get_multiple = enif_make_atom(env, "get_multiple");
  atom_next_multiple = enif_make_atom(env, "next_multiple");
  atom_integerdup = enif_make_atom(env, "integerdup");
  atom_integerkey = enif_make_atom(env, "integerkey");
  atom_reversedup = enif_make_atom(env, "reversedup");
  atom_create = enif_make_atom(env, "create");

  // errno
  atom_e2big = enif_make_atom(env, "e2big");
  atom_eacces = enif_make_atom(env, "eacces");
  atom_eaddrinuse = enif_make_atom(env, "eaddrinuse");
  atom_eaddrnotavail = enif_make_atom(env, "eaddrnotavail");
  atom_eadv = enif_make_atom(env, "eadv");
  atom_eafnosupport = enif_make_atom(env, "eafnosupport");
  atom_eagain = enif_make_atom(env, "eagain");
  atom_ealready = enif_make_atom(env, "ealready");
  atom_ebade = enif_make_atom(env, "ebade");
  atom_ebadf = enif_make_atom(env, "ebadf");
  atom_ebadfd = enif_make_atom(env, "ebadfd");
  atom_ebadmsg = enif_make_atom(env, "ebadmsg");
  atom_ebadr = enif_make_atom(env, "ebadr");
  atom_ebadrqc = enif_make_atom(env, "ebadrqc");
  atom_ebadslt = enif_make_atom(env, "ebadslt");
  atom_ebfont = enif_make_atom(env, "ebfont");
  atom_ebusy = enif_make_atom(env, "ebusy");
  atom_ecanceled = enif_make_atom(env, "ecanceled");
  atom_echild = enif_make_atom(env, "echild");
  atom_echrng = enif_make_atom(env, "echrng");
  atom_ecomm = enif_make_atom(env, "ecomm");
  atom_econnaborted = enif_make_atom(env, "econnaborted");
  atom_econnrefused = enif_make_atom(env, "econnrefused");
  atom_econnreset = enif_make_atom(env, "econnreset");
  atom_edeadlock = enif_make_atom(env, "edeadlock");
  atom_edestaddrreq = enif_make_atom(env, "edestaddrreq");
  atom_edom = enif_make_atom(env, "edom");
  atom_edotdot = enif_make_atom(env, "edotdot");
  atom_edquot = enif_make_atom(env, "edquot");
  atom_eexist = enif_make_atom(env, "eexist");
  atom_efault = enif_make_atom(env, "efault");
  atom_efbig = enif_make_atom(env, "efbig");
  atom_ehostdown = enif_make_atom(env, "ehostdown");
  atom_ehostunreach = enif_make_atom(env, "ehostunreach");
  atom_ehwpoison = enif_make_atom(env, "ehwpoison");
  atom_eidrm = enif_make_atom(env, "eidrm");
  atom_eilseq = enif_make_atom(env, "eilseq");
  atom_einprogress = enif_make_atom(env, "einprogress");
  atom_eintr = enif_make_atom(env, "eintr");
  atom_einval = enif_make_atom(env, "einval");
  atom_eio = enif_make_atom(env, "eio");
  atom_eisconn = enif_make_atom(env, "eisconn");
  atom_eisdir = enif_make_atom(env, "eisdir");
  atom_eisnam = enif_make_atom(env, "eisnam");
  atom_ekeyexpired = enif_make_atom(env, "ekeyexpired");
  atom_ekeyrejected = enif_make_atom(env, "ekeyrejected");
  atom_ekeyrevoked = enif_make_atom(env, "ekeyrevoked");
  atom_el2hlt = enif_make_atom(env, "el2hlt");
  atom_el2nsync = enif_make_atom(env, "el2nsync");
  atom_el3hlt = enif_make_atom(env, "el3hlt");
  atom_el3rst = enif_make_atom(env, "el3rst");
  atom_elibacc = enif_make_atom(env, "elibacc");
  atom_elibbad = enif_make_atom(env, "elibbad");
  atom_elibexec = enif_make_atom(env, "elibexec");
  atom_elibmax = enif_make_atom(env, "elibmax");
  atom_elibscn = enif_make_atom(env, "elibscn");
  atom_elnrng = enif_make_atom(env, "elnrng");
  atom_eloop = enif_make_atom(env, "eloop");
  atom_emediumtype = enif_make_atom(env, "emediumtype");
  atom_emfile = enif_make_atom(env, "emfile");
  atom_emlink = enif_make_atom(env, "emlink");
  atom_emsgsize = enif_make_atom(env, "emsgsize");
  atom_emultihop = enif_make_atom(env, "emultihop");
  atom_enametoolong = enif_make_atom(env, "enametoolong");
  atom_enavail = enif_make_atom(env, "enavail");
  atom_enetdown = enif_make_atom(env, "enetdown");
  atom_enetreset = enif_make_atom(env, "enetreset");
  atom_enetunreach = enif_make_atom(env, "enetunreach");
  atom_enfile = enif_make_atom(env, "enfile");
  atom_enoano = enif_make_atom(env, "enoano");
  atom_enobufs = enif_make_atom(env, "enobufs");
  atom_enocsi = enif_make_atom(env, "enocsi");
  atom_enodata = enif_make_atom(env, "enodata");
  atom_enodev = enif_make_atom(env, "enodev");
  atom_enoent = enif_make_atom(env, "enoent");
  atom_enoexec = enif_make_atom(env, "enoexec");
  atom_enokey = enif_make_atom(env, "enokey");
  atom_enolck = enif_make_atom(env, "enolck");
  atom_enolink = enif_make_atom(env, "enolink");
  atom_enomedium = enif_make_atom(env, "enomedium");
  atom_enomem = enif_make_atom(env, "enomem");
  atom_enomsg = enif_make_atom(env, "enomsg");
  atom_enonet = enif_make_atom(env, "enonet");
  atom_enopkg = enif_make_atom(env, "enopkg");
  atom_enoprotoopt = enif_make_atom(env, "enoprotoopt");
  atom_enospc = enif_make_atom(env, "enospc");
  atom_enosr = enif_make_atom(env, "enosr");
  atom_enostr = enif_make_atom(env, "enostr");
  atom_enosys = enif_make_atom(env, "enosys");
  atom_enotblk = enif_make_atom(env, "enotblk");
  atom_enotconn = enif_make_atom(env, "enotconn");
  atom_enotdir = enif_make_atom(env, "enotdir");
  atom_enotempty = enif_make_atom(env, "enotempty");
  atom_enotnam = enif_make_atom(env, "enotnam");
  atom_enotrecoverable = enif_make_atom(env, "enotrecoverable");
  atom_enotsock = enif_make_atom(env, "enotsock");
  atom_enotsup = enif_make_atom(env, "enotsup");
  atom_enotty = enif_make_atom(env, "enotty");
  atom_enotuniq = enif_make_atom(env, "enotuniq");
  atom_enxio = enif_make_atom(env, "enxio");
  atom_eoverflow = enif_make_atom(env, "eoverflow");
  atom_eownerdead = enif_make_atom(env, "eownerdead");
  atom_eperm = enif_make_atom(env, "eperm");
  atom_epfnosupport = enif_make_atom(env, "epfnosupport");
  atom_epipe = enif_make_atom(env, "epipe");
  atom_eproto = enif_make_atom(env, "eproto");
  atom_eprotonosupport = enif_make_atom(env, "eprotonosupport");
  atom_eprototype = enif_make_atom(env, "eprototype");
  atom_erange = enif_make_atom(env, "erange");
  atom_eremchg = enif_make_atom(env, "eremchg");
  atom_eremote = enif_make_atom(env, "eremote");
  atom_eremoteio = enif_make_atom(env, "eremoteio");
  atom_erestart = enif_make_atom(env, "erestart");
  atom_erfkill = enif_make_atom(env, "erfkill");
  atom_erofs = enif_make_atom(env, "erofs");
  atom_eshutdown = enif_make_atom(env, "eshutdown");
  atom_esocktnosupport = enif_make_atom(env, "esocktnosupport");
  atom_espipe = enif_make_atom(env, "espipe");
  atom_esrch = enif_make_atom(env, "esrch");
  atom_esrmnt = enif_make_atom(env, "esrmnt");
  atom_estale = enif_make_atom(env, "estale");
  atom_estrpipe = enif_make_atom(env, "estrpipe");
  atom_etime = enif_make_atom(env, "etime");
  atom_etimedout = enif_make_atom(env, "etimedout");
  atom_etoomanyrefs = enif_make_atom(env, "etoomanyrefs");
  atom_etxtbsy = enif_make_atom(env, "etxtbsy");
  atom_euclean = enif_make_atom(env, "euclean");
  atom_eunatch = enif_make_atom(env, "eunatch");
  atom_eusers = enif_make_atom(env, "eusers");
  atom_exdev = enif_make_atom(env, "exdev");
  atom_exfull = enif_make_atom(env, "exfull");

  // LMDB errors
  atom_keyexist = enif_make_atom(env, "keyexist");
  atom_notfound = enif_make_atom(env, "notfound");
  atom_page_notfound = enif_make_atom(env, "page_notfound");
  atom_corrupted = enif_make_atom(env, "corrupted");
  atom_panic = enif_make_atom(env, "panic");
  atom_version_mismatch = enif_make_atom(env, "version_mismatch");
  atom_invalid = enif_make_atom(env, "invalid");
  atom_map_full = enif_make_atom(env, "map_full");
  atom_dbs_full = enif_make_atom(env, "dbs_full");
  atom_readers_full = enif_make_atom(env, "readers_full");
  atom_tls_full = enif_make_atom(env, "tls_full");
  atom_txn_full = enif_make_atom(env, "txn_full");
  atom_cursor_full = enif_make_atom(env, "cursor_full");
  atom_page_full = enif_make_atom(env, "page_full");
  atom_map_resized = enif_make_atom(env, "map_resized");
  atom_incompatible = enif_make_atom(env, "incompatible");
  atom_bad_rslot = enif_make_atom(env, "bad_rslot");
  atom_bad_txn = enif_make_atom(env, "bad_txn");
  atom_bad_valsize = enif_make_atom(env, "bad_valsize");
  atom_bad_dbi = enif_make_atom(env, "bad_dbi");

  // LMDB cursor op
  atom_first = enif_make_atom(env, "first");
  atom_first_dup = enif_make_atom(env, "first_dup");
  atom_get_both = enif_make_atom(env, "get_both");
  atom_get_both_range = enif_make_atom(env, "get_both_range");
  atom_get_current = enif_make_atom(env, "get_current");
  atom_get_multiple = enif_make_atom(env, "get_multiple");
  atom_last = enif_make_atom(env, "last");
  atom_last_dup = enif_make_atom(env, "last_dup");
  atom_next = enif_make_atom(env, "next");
  atom_next_dup = enif_make_atom(env, "next_dup");
  atom_next_multiple = enif_make_atom(env, "next_multiple");
  atom_next_nodup = enif_make_atom(env, "next_nodup");
  atom_prev = enif_make_atom(env, "prev");
  atom_prev_dup = enif_make_atom(env, "prev_dup");
  atom_prev_nodup = enif_make_atom(env, "prev_nodup");
  atom_set = enif_make_atom(env, "set");
  atom_set_key = enif_make_atom(env, "set_key");
  atom_set_range = enif_make_atom(env, "set_range");
}

ERL_NIF_TERM errtag(ErlNifEnv *env, int err) {
  switch (err) {
  case E2BIG:
    return atom_e2big;
  case EACCES:
    return atom_eacces;
  case EADDRINUSE:
    return atom_eaddrinuse;
  case EADDRNOTAVAIL:
    return atom_eaddrnotavail;
  case EADV:
    return atom_eadv;
  case EAFNOSUPPORT:
    return atom_eafnosupport;
  case EAGAIN:
    return atom_eagain;
  case EALREADY:
    return atom_ealready;
  case EBADE:
    return atom_ebade;
  case EBADF:
    return atom_ebadf;
  case EBADFD:
    return atom_ebadfd;
  case EBADMSG:
    return atom_ebadmsg;
  case EBADR:
    return atom_ebadr;
  case EBADRQC:
    return atom_ebadrqc;
  case EBADSLT:
    return atom_ebadslt;
  case EBFONT:
    return atom_ebfont;
  case EBUSY:
    return atom_ebusy;
  case ECANCELED:
    return atom_ecanceled;
  case ECHILD:
    return atom_echild;
  case ECHRNG:
    return atom_echrng;
  case ECOMM:
    return atom_ecomm;
  case ECONNABORTED:
    return atom_econnaborted;
  case ECONNREFUSED:
    return atom_econnrefused;
  case ECONNRESET:
    return atom_econnreset;
  case EDEADLOCK:
    return atom_edeadlock;
  case EDESTADDRREQ:
    return atom_edestaddrreq;
  case EDOM:
    return atom_edom;
  case EDOTDOT:
    return atom_edotdot;
  case EDQUOT:
    return atom_edquot;
  case EEXIST:
    return atom_eexist;
  case EFAULT:
    return atom_efault;
  case EFBIG:
    return atom_efbig;
  case EHOSTDOWN:
    return atom_ehostdown;
  case EHOSTUNREACH:
    return atom_ehostunreach;
  case EHWPOISON:
    return atom_ehwpoison;
  case EIDRM:
    return atom_eidrm;
  case EILSEQ:
    return atom_eilseq;
  case EINPROGRESS:
    return atom_einprogress;
  case EINTR:
    return atom_eintr;
  case EINVAL:
    return atom_einval;
  case EIO:
    return atom_eio;
  case EISCONN:
    return atom_eisconn;
  case EISDIR:
    return atom_eisdir;
  case EISNAM:
    return atom_eisnam;
  case EKEYEXPIRED:
    return atom_ekeyexpired;
  case EKEYREJECTED:
    return atom_ekeyrejected;
  case EKEYREVOKED:
    return atom_ekeyrevoked;
  case EL2HLT:
    return atom_el2hlt;
  case EL2NSYNC:
    return atom_el2nsync;
  case EL3HLT:
    return atom_el3hlt;
  case EL3RST:
    return atom_el3rst;
  case ELIBACC:
    return atom_elibacc;
  case ELIBBAD:
    return atom_elibbad;
  case ELIBEXEC:
    return atom_elibexec;
  case ELIBMAX:
    return atom_elibmax;
  case ELIBSCN:
    return atom_elibscn;
  case ELNRNG:
    return atom_elnrng;
  case ELOOP:
    return atom_eloop;
  case EMEDIUMTYPE:
    return atom_emediumtype;
  case EMFILE:
    return atom_emfile;
  case EMLINK:
    return atom_emlink;
  case EMSGSIZE:
    return atom_emsgsize;
  case EMULTIHOP:
    return atom_emultihop;
  case ENAMETOOLONG:
    return atom_enametoolong;
  case ENAVAIL:
    return atom_enavail;
  case ENETDOWN:
    return atom_enetdown;
  case ENETRESET:
    return atom_enetreset;
  case ENETUNREACH:
    return atom_enetunreach;
  case ENFILE:
    return atom_enfile;
  case ENOANO:
    return atom_enoano;
  case ENOBUFS:
    return atom_enobufs;
  case ENOCSI:
    return atom_enocsi;
  case ENODATA:
    return atom_enodata;
  case ENODEV:
    return atom_enodev;
  case ENOENT:
    return atom_enoent;
  case ENOEXEC:
    return atom_enoexec;
  case ENOKEY:
    return atom_enokey;
  case ENOLCK:
    return atom_enolck;
  case ENOLINK:
    return atom_enolink;
  case ENOMEDIUM:
    return atom_enomedium;
  case ENOMEM:
    return atom_enomem;
  case ENOMSG:
    return atom_enomsg;
  case ENONET:
    return atom_enonet;
  case ENOPKG:
    return atom_enopkg;
  case ENOPROTOOPT:
    return atom_enoprotoopt;
  case ENOSPC:
    return atom_enospc;
  case ENOSR:
    return atom_enosr;
  case ENOSTR:
    return atom_enostr;
  case ENOSYS:
    return atom_enosys;
  case ENOTBLK:
    return atom_enotblk;
  case ENOTCONN:
    return atom_enotconn;
  case ENOTDIR:
    return atom_enotdir;
  case ENOTEMPTY:
    return atom_enotempty;
  case ENOTNAM:
    return atom_enotnam;
  case ENOTRECOVERABLE:
    return atom_enotrecoverable;
  case ENOTSOCK:
    return atom_enotsock;
  case ENOTSUP:
    return atom_enotsup;
  case ENOTTY:
    return atom_enotty;
  case ENOTUNIQ:
    return atom_enotuniq;
  case ENXIO:
    return atom_enxio;
  case EOVERFLOW:
    return atom_eoverflow;
  case EOWNERDEAD:
    return atom_eownerdead;
  case EPERM:
    return atom_eperm;
  case EPFNOSUPPORT:
    return atom_epfnosupport;
  case EPIPE:
    return atom_epipe;
  case EPROTO:
    return atom_eproto;
  case EPROTONOSUPPORT:
    return atom_eprotonosupport;
  case EPROTOTYPE:
    return atom_eprototype;
  case ERANGE:
    return atom_erange;
  case EREMCHG:
    return atom_eremchg;
  case EREMOTE:
    return atom_eremote;
  case EREMOTEIO:
    return atom_eremoteio;
  case ERESTART:
    return atom_erestart;
  case ERFKILL:
    return atom_erfkill;
  case EROFS:
    return atom_erofs;
  case ESHUTDOWN:
    return atom_eshutdown;
  case ESOCKTNOSUPPORT:
    return atom_esocktnosupport;
  case ESPIPE:
    return atom_espipe;
  case ESRCH:
    return atom_esrch;
  case ESRMNT:
    return atom_esrmnt;
  case ESTALE:
    return atom_estale;
  case ESTRPIPE:
    return atom_estrpipe;
  case ETIME:
    return atom_etime;
  case ETIMEDOUT:
    return atom_etimedout;
  case ETOOMANYREFS:
    return atom_etoomanyrefs;
  case ETXTBSY:
    return atom_etxtbsy;
  case EUCLEAN:
    return atom_euclean;
  case EUNATCH:
    return atom_eunatch;
  case EUSERS:
    return atom_eusers;
  case EXDEV:
    return atom_exdev;
  case EXFULL:
    return atom_exfull;

  case MDB_KEYEXIST:
    return atom_keyexist;
  case MDB_NOTFOUND:
    return atom_notfound;
  case MDB_PAGE_NOTFOUND:
    return atom_page_notfound;
  case MDB_CORRUPTED:
    return atom_corrupted;
  case MDB_PANIC:
    return atom_panic;
  case MDB_VERSION_MISMATCH:
    return atom_version_mismatch;
  case MDB_INVALID:
    return atom_invalid;
  case MDB_MAP_FULL:
    return atom_map_full;
  case MDB_DBS_FULL:
    return atom_dbs_full;
  case MDB_READERS_FULL:
    return atom_readers_full;
  case MDB_TLS_FULL:
    return atom_tls_full;
  case MDB_TXN_FULL:
    return atom_txn_full;
  case MDB_CURSOR_FULL:
    return atom_cursor_full;
  case MDB_PAGE_FULL:
    return atom_page_full;
  case MDB_MAP_RESIZED:
    return atom_map_resized;
  case MDB_INCOMPATIBLE:
    return atom_incompatible;
  case MDB_BAD_RSLOT:
    return atom_bad_rslot;
  case MDB_BAD_TXN:
    return atom_bad_txn;
  case MDB_BAD_VALSIZE:
    return atom_bad_valsize;
  case MDB_BAD_DBI:
    return atom_bad_dbi;

  default:
    return enif_make_tuple2(env, atom_errno, enif_make_int(env, err));
  }
}

unsigned int encode_flag(ERL_NIF_TERM atom) {
  unsigned int flag;

  if (atom == atom_fixedmap)
    flag = MDB_FIXEDMAP;
  else if (atom == atom_nosubdir)
    flag = MDB_NOSUBDIR;
  else if (atom == atom_rdonly)
    flag = MDB_RDONLY;
  else if (atom == atom_writemap)
    flag = MDB_WRITEMAP;
  else if (atom == atom_nometasync)
    flag = MDB_NOMETASYNC;
  else if (atom == atom_nosync)
    flag = MDB_NOSYNC;
  else if (atom == atom_mapasync)
    flag = MDB_MAPASYNC;
  else if (atom == atom_notls)
    flag = MDB_NOTLS;
  else if (atom == atom_nolock)
    flag = MDB_NOLOCK;
  else if (atom == atom_nordahead)
    flag = MDB_NORDAHEAD;
  else if (atom == atom_nomeminit)
    flag = MDB_NOMEMINIT;
  else if (atom == atom_reversekey)
    flag = MDB_REVERSEKEY;
  else if (atom == atom_dupsort)
    flag = MDB_DUPSORT;
  else if (atom == atom_integerkey)
    flag = MDB_INTEGERKEY;
  else if (atom == atom_dupfixed)
    flag = MDB_DUPFIXED;
  else if (atom == atom_get_multiple)
    flag = MDB_GET_MULTIPLE;
  else if (atom == atom_next_multiple)
    flag = MDB_NEXT_MULTIPLE;
  else if (atom == atom_integerdup)
    flag = MDB_INTEGERDUP;
  else if (atom == atom_integerkey)
    flag = MDB_INTEGERKEY;
  else if (atom == atom_reversedup)
    flag = MDB_REVERSEDUP;
  else if (atom == atom_create)
    flag = MDB_CREATE;
  else
    flag = 0;

  return flag;
}

MDB_cursor_op encode_cursor_op(ERL_NIF_TERM atom) {
  MDB_cursor_op op;

  if (atom == atom_first)
    op = MDB_FIRST;
  else if (atom == atom_first_dup)
    op = MDB_FIRST_DUP;
  else if (atom == atom_get_both)
    op = MDB_GET_BOTH;
  else if (atom == atom_get_both_range)
    op = MDB_GET_BOTH_RANGE;
  else if (atom == atom_get_current)
    op = MDB_GET_CURRENT;
  else if (atom == atom_get_multiple)
    op = MDB_GET_MULTIPLE;
  else if (atom == atom_last)
    op = MDB_LAST;
  else if (atom == atom_last_dup)
    op = MDB_LAST_DUP;
  else if (atom == atom_next)
    op = MDB_NEXT;
  else if (atom == atom_next_dup)
    op = MDB_NEXT_DUP;
  else if (atom == atom_next_multiple)
    op = MDB_NEXT_MULTIPLE;
  else if (atom == atom_next_nodup)
    op = MDB_NEXT_NODUP;
  else if (atom == atom_prev)
    op = MDB_PREV;
  else if (atom == atom_prev_dup)
    op = MDB_PREV_DUP;
  else if (atom == atom_prev_nodup)
    op = MDB_PREV_NODUP;
  else if (atom == atom_set)
    op = MDB_SET;
  else if (atom == atom_set_key)
    op = MDB_SET_KEY;
  else if (atom == atom_set_range)
    op = MDB_SET_RANGE;
  else
    op = -1;

  return op;
}

int encode_errtag(ERL_NIF_TERM atom) {
  unsigned int err;

  if (atom == atom_e2big)
    err = E2BIG;
  else if (atom == atom_eacces)
    err = EACCES;
  else if (atom == atom_eaddrinuse)
    err = EADDRINUSE;
  else if (atom == atom_eaddrnotavail)
    err = EADDRNOTAVAIL;
  else if (atom == atom_eadv)
    err = EADV;
  else if (atom == atom_eafnosupport)
    err = EAFNOSUPPORT;
  else if (atom == atom_eagain)
    err = EAGAIN;
  else if (atom == atom_ealready)
    err = EALREADY;
  else if (atom == atom_ebade)
    err = EBADE;
  else if (atom == atom_ebadf)
    err = EBADF;
  else if (atom == atom_ebadfd)
    err = EBADFD;
  else if (atom == atom_ebadmsg)
    err = EBADMSG;
  else if (atom == atom_ebadr)
    err = EBADR;
  else if (atom == atom_ebadrqc)
    err = EBADRQC;
  else if (atom == atom_ebadslt)
    err = EBADSLT;
  else if (atom == atom_ebfont)
    err = EBFONT;
  else if (atom == atom_ebusy)
    err = EBUSY;
  else if (atom == atom_ecanceled)
    err = ECANCELED;
  else if (atom == atom_echild)
    err = ECHILD;
  else if (atom == atom_echrng)
    err = ECHRNG;
  else if (atom == atom_ecomm)
    err = ECOMM;
  else if (atom == atom_econnaborted)
    err = ECONNABORTED;
  else if (atom == atom_econnrefused)
    err = ECONNREFUSED;
  else if (atom == atom_econnreset)
    err = ECONNRESET;
  else if (atom == atom_edeadlock)
    err = EDEADLOCK;
  else if (atom == atom_edestaddrreq)
    err = EDESTADDRREQ;
  else if (atom == atom_edom)
    err = EDOM;
  else if (atom == atom_edotdot)
    err = EDOTDOT;
  else if (atom == atom_edquot)
    err = EDQUOT;
  else if (atom == atom_eexist)
    err = EEXIST;
  else if (atom == atom_efault)
    err = EFAULT;
  else if (atom == atom_efbig)
    err = EFBIG;
  else if (atom == atom_ehostdown)
    err = EHOSTDOWN;
  else if (atom == atom_ehostunreach)
    err = EHOSTUNREACH;
  else if (atom == atom_ehwpoison)
    err = EHWPOISON;
  else if (atom == atom_eidrm)
    err = EIDRM;
  else if (atom == atom_eilseq)
    err = EILSEQ;
  else if (atom == atom_einprogress)
    err = EINPROGRESS;
  else if (atom == atom_eintr)
    err = EINTR;
  else if (atom == atom_einval)
    err = EINVAL;
  else if (atom == atom_eio)
    err = EIO;
  else if (atom == atom_eisconn)
    err = EISCONN;
  else if (atom == atom_eisdir)
    err = EISDIR;
  else if (atom == atom_eisnam)
    err = EISNAM;
  else if (atom == atom_ekeyexpired)
    err = EKEYEXPIRED;
  else if (atom == atom_ekeyrejected)
    err = EKEYREJECTED;
  else if (atom == atom_ekeyrevoked)
    err = EKEYREVOKED;
  else if (atom == atom_el2hlt)
    err = EL2HLT;
  else if (atom == atom_el2nsync)
    err = EL2NSYNC;
  else if (atom == atom_el3hlt)
    err = EL3HLT;
  else if (atom == atom_el3rst)
    err = EL3RST;
  else if (atom == atom_elibacc)
    err = ELIBACC;
  else if (atom == atom_elibbad)
    err = ELIBBAD;
  else if (atom == atom_elibexec)
    err = ELIBEXEC;
  else if (atom == atom_elibmax)
    err = ELIBMAX;
  else if (atom == atom_elibscn)
    err = ELIBSCN;
  else if (atom == atom_elnrng)
    err = ELNRNG;
  else if (atom == atom_eloop)
    err = ELOOP;
  else if (atom == atom_emediumtype)
    err = EMEDIUMTYPE;
  else if (atom == atom_emfile)
    err = EMFILE;
  else if (atom == atom_emlink)
    err = EMLINK;
  else if (atom == atom_emsgsize)
    err = EMSGSIZE;
  else if (atom == atom_emultihop)
    err = EMULTIHOP;
  else if (atom == atom_enametoolong)
    err = ENAMETOOLONG;
  else if (atom == atom_enavail)
    err = ENAVAIL;
  else if (atom == atom_enetdown)
    err = ENETDOWN;
  else if (atom == atom_enetreset)
    err = ENETRESET;
  else if (atom == atom_enetunreach)
    err = ENETUNREACH;
  else if (atom == atom_enfile)
    err = ENFILE;
  else if (atom == atom_enoano)
    err = ENOANO;
  else if (atom == atom_enobufs)
    err = ENOBUFS;
  else if (atom == atom_enocsi)
    err = ENOCSI;
  else if (atom == atom_enodata)
    err = ENODATA;
  else if (atom == atom_enodev)
    err = ENODEV;
  else if (atom == atom_enoent)
    err = ENOENT;
  else if (atom == atom_enoexec)
    err = ENOEXEC;
  else if (atom == atom_enokey)
    err = ENOKEY;
  else if (atom == atom_enolck)
    err = ENOLCK;
  else if (atom == atom_enolink)
    err = ENOLINK;
  else if (atom == atom_enomedium)
    err = ENOMEDIUM;
  else if (atom == atom_enomem)
    err = ENOMEM;
  else if (atom == atom_enomsg)
    err = ENOMSG;
  else if (atom == atom_enonet)
    err = ENONET;
  else if (atom == atom_enopkg)
    err = ENOPKG;
  else if (atom == atom_enoprotoopt)
    err = ENOPROTOOPT;
  else if (atom == atom_enospc)
    err = ENOSPC;
  else if (atom == atom_enosr)
    err = ENOSR;
  else if (atom == atom_enostr)
    err = ENOSTR;
  else if (atom == atom_enosys)
    err = ENOSYS;
  else if (atom == atom_enotblk)
    err = ENOTBLK;
  else if (atom == atom_enotconn)
    err = ENOTCONN;
  else if (atom == atom_enotdir)
    err = ENOTDIR;
  else if (atom == atom_enotempty)
    err = ENOTEMPTY;
  else if (atom == atom_enotnam)
    err = ENOTNAM;
  else if (atom == atom_enotrecoverable)
    err = ENOTRECOVERABLE;
  else if (atom == atom_enotsock)
    err = ENOTSOCK;
  else if (atom == atom_enotsup)
    err = ENOTSUP;
  else if (atom == atom_enotty)
    err = ENOTTY;
  else if (atom == atom_enotuniq)
    err = ENOTUNIQ;
  else if (atom == atom_enxio)
    err = ENXIO;
  else if (atom == atom_eoverflow)
    err = EOVERFLOW;
  else if (atom == atom_eownerdead)
    err = EOWNERDEAD;
  else if (atom == atom_eperm)
    err = EPERM;
  else if (atom == atom_epfnosupport)
    err = EPFNOSUPPORT;
  else if (atom == atom_epipe)
    err = EPIPE;
  else if (atom == atom_eproto)
    err = EPROTO;
  else if (atom == atom_eprotonosupport)
    err = EPROTONOSUPPORT;
  else if (atom == atom_eprototype)
    err = EPROTOTYPE;
  else if (atom == atom_erange)
    err = ERANGE;
  else if (atom == atom_eremchg)
    err = EREMCHG;
  else if (atom == atom_eremote)
    err = EREMOTE;
  else if (atom == atom_eremoteio)
    err = EREMOTEIO;
  else if (atom == atom_erestart)
    err = ERESTART;
  else if (atom == atom_erfkill)
    err = ERFKILL;
  else if (atom == atom_erofs)
    err = EROFS;
  else if (atom == atom_eshutdown)
    err = ESHUTDOWN;
  else if (atom == atom_esocktnosupport)
    err = ESOCKTNOSUPPORT;
  else if (atom == atom_espipe)
    err = ESPIPE;
  else if (atom == atom_esrch)
    err = ESRCH;
  else if (atom == atom_esrmnt)
    err = ESRMNT;
  else if (atom == atom_estale)
    err = ESTALE;
  else if (atom == atom_estrpipe)
    err = ESTRPIPE;
  else if (atom == atom_etime)
    err = ETIME;
  else if (atom == atom_etimedout)
    err = ETIMEDOUT;
  else if (atom == atom_etoomanyrefs)
    err = ETOOMANYREFS;
  else if (atom == atom_etxtbsy)
    err = ETXTBSY;
  else if (atom == atom_euclean)
    err = EUCLEAN;
  else if (atom == atom_eunatch)
    err = EUNATCH;
  else if (atom == atom_eusers)
    err = EUSERS;
  else if (atom == atom_exdev)
    err = EXDEV;
  else if (atom == atom_exfull)
    err = EXFULL;

  else if (atom == atom_keyexist)
    err = MDB_KEYEXIST;
  else if (atom == atom_notfound)
    err = MDB_NOTFOUND;
  else if (atom == atom_page_notfound)
    err = MDB_PAGE_NOTFOUND;
  else if (atom == atom_corrupted)
    err = MDB_CORRUPTED;
  else if (atom == atom_panic)
    err = MDB_PANIC;
  else if (atom == atom_version_mismatch)
    err = MDB_VERSION_MISMATCH;
  else if (atom == atom_invalid)
    err = MDB_INVALID;
  else if (atom == atom_map_full)
    err = MDB_MAP_FULL;
  else if (atom == atom_dbs_full)
    err = MDB_DBS_FULL;
  else if (atom == atom_readers_full)
    err = MDB_READERS_FULL;
  else if (atom == atom_tls_full)
    err = MDB_TLS_FULL;
  else if (atom == atom_txn_full)
    err = MDB_TXN_FULL;
  else if (atom == atom_cursor_full)
    err = MDB_CURSOR_FULL;
  else if (atom == atom_page_full)
    err = MDB_PAGE_FULL;
  else if (atom == atom_map_resized)
    err = MDB_MAP_RESIZED;
  else if (atom == atom_incompatible)
    err = MDB_INCOMPATIBLE;
  else if (atom == atom_bad_rslot)
    err = MDB_BAD_RSLOT;
  else if (atom == atom_bad_txn)
    err = MDB_BAD_TXN;
  else if (atom == atom_bad_valsize)
    err = MDB_BAD_VALSIZE;
  else if (atom == atom_bad_dbi)
    err = MDB_BAD_DBI;
  else
    err = 0;

  return err;
}
