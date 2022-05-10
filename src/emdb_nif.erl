%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <xramtsov@gmail.com>
%%% @copyright (C) 2022, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  9 May 2022 by Evgeny Khramtsov <xramtsov@gmail.com>
%%%-------------------------------------------------------------------
-module(emdb_nif).

-on_load(load/0).

%% API
-export([encode_flag/1]).
-export([encode_flags/1]).
-export([encode_errtag/1]).
-export([encode_cursor_op/1]).
-export([env_create/0]).
-export([env_open/4]).
-export([env_close/1]).
-export([env_set_mapsize/2]).
-export([env_set_maxreaders/2]).
-export([env_set_maxdbs/2]).
-export([strerror/1]).
-export([version/0]).
-export([txn_begin/2]).
-export([txn_commit/1]).
-export([txn_abort/1]).
-export([dbi_open/3]).
-export([dbi_close/2]).
-export([put/5]).
-export([get/3]).
-export([del/4]).
-export([cursor_open/2]).
-export([cursor_close/1]).
-export([cursor_get/2]).
%% Shut up xref
-export([load/0]).

%%%===================================================================
%%% API
%%%===================================================================
encode_flag(_) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

encode_flags(Flags) ->
    encode_flags(Flags, 0).

encode_flags([Flag|Flags], Encoded) ->
    encode_flags(Flags, Encoded bor encode_flag(Flag));
encode_flags([], Encoded) ->
    Encoded.

encode_errtag(_) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

encode_cursor_op(_) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

strerror(_) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

version() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

env_create() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

env_open(_Env, _Path, _Flags, _Mode) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

env_close(_Env) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

env_set_mapsize(_Env, _Size) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

env_set_maxdbs(_Env, _MaxDbs) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

env_set_maxreaders(_Env, _MaxReaders) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

txn_begin(_Env, _Flags) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

txn_commit(_Env) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

txn_abort(_Env) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

dbi_open(_Txn, _Name, _Flags) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

dbi_close(_Txn, _Dbi) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

put(_Txn, _Dbi, _Key, _Val, _Flags) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

get(_Txn, _Dbi, _Key) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

del(_Txn, _Dbi, _Key, _Val) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

cursor_open(_Txn, _Dbi) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

cursor_close(_Cur) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

cursor_get(_Cur, _Op) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec load() -> ok | {error, term()}.
load() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    AppDir = filename:dirname(EbinDir),
    PrivDir = filename:join([AppDir, "priv"]),
    SOPath = filename:join(PrivDir, ?MODULE),
    erlang:load_nif(SOPath, 0).
