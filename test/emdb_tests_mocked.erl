%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <xramtsov@gmail.com>
%%% @copyright (C) 2022, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 11 May 2022 by Evgeny Khramtsov <xramtsov@gmail.com>
%%%-------------------------------------------------------------------
-module(emdb_tests_mocked).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start_failed_test() ->
    Error = {error, einval},
    meck_nif(),
    meck:expect(emdb_nif, env_open, fun(_, _, _, _) -> Error end),
    emdb:start(),
    meck:expect(emdb_nif, env_create, fun() -> Error end),
    emdb:start(),
    unmeck_nif().

start_test() ->
    meck_nif(),
    emdb:start(),
    unmeck_nif().

cursor_failed_test() ->
    Err = {error, einval},
    T = ?FUNCTION_NAME,
    meck_nif(),
    ok = emdb:open_table(T),
    meck:expect(emdb_nif, cursor_open, fun(_, _) -> Err end),
    Err = emdb:txn(fun() -> emdb:first(T) end),
    ok = emdb:close_table(T),
    unmeck_nif().

aborted_test() ->
    Reason = einval,
    meck_nif(),
    meck:expect(emdb_nif, txn_commit, fun(_) -> {error, Reason} end),
    ?assertError({aborted, {commit_failed, Reason}},
                 emdb:txn(fun() -> ok end)),
    meck:expect(emdb_nif, txn_begin, fun(_, _) -> {error, Reason} end),
    ?assertError({aborted, {begin_failed, Reason}},
                 emdb:txn(fun() -> ok end)),
    unmeck_nif().

dbi_open_failed_test() ->
    Reason = einval,
    Err = {error, Reason},
    Aborted = {aborted, Reason},
    T = ?FUNCTION_NAME,
    meck_nif(),
    meck:expect(emdb_nif, dbi_open, fun(_, _, _) -> Err end),
    Err = emdb:open_table(T),
    meck:expect(emdb_nif, dbi_open, fun(_, _, _) -> erlang:error(Aborted) end),
    ?assertEqual({error, Aborted}, emdb:open_table(T)),
    unmeck_nif().

stop_test() ->
    meck_nif(),
    ok = emdb:stop(),
    unmeck_nif().

%%%===================================================================
%%% Internal functions
%%%===================================================================
meck_nif() ->
    meck:new(emdb_nif),
    meck:expect(emdb_nif, encode_flag, fun(_) -> 0 end),
    meck:expect(emdb_nif, encode_flags, fun(_) -> 0 end),
    meck:expect(emdb_nif, encode_errtag, fun(Tag) -> atom_to_list(Tag) end),
    meck:expect(emdb_nif, encode_cursor_op, fun(_) -> 0 end),
    meck:expect(emdb_nif, strerror, fun(S) -> S end),
    meck:expect(emdb_nif, version, fun() -> {0, 0, 0} end),
    meck:expect(emdb_nif, env_create, fun() -> {ok, make_ref()} end),
    meck:expect(emdb_nif, env_open, fun(_, _, _, _) -> ok end),
    meck:expect(emdb_nif, env_close, fun(_) -> true end),
    meck:expect(emdb_nif, env_set_mapsize, fun(_, _) -> ok end),
    meck:expect(emdb_nif, env_set_maxdbs, fun(_, _) -> ok end),
    meck:expect(emdb_nif, env_set_maxreaders, fun(_, _) -> ok end),
    meck:expect(emdb_nif, txn_begin, fun(_, _) -> {ok, make_ref()} end),
    meck:expect(emdb_nif, txn_commit, fun(_) -> ok end),
    meck:expect(emdb_nif, txn_abort, fun(_) -> ok end),
    meck:expect(emdb_nif, dbi_open, fun(_, _, _) -> {ok, 0} end),
    meck:expect(emdb_nif, dbi_close, fun(_, _) -> ok end),
    meck:expect(emdb_nif, put, fun(_, _, _, _, _) -> ok end),
    meck:expect(emdb_nif, get, fun(_, _, _) -> {error, notfound} end),
    meck:expect(emdb_nif, del, fun(_, _, _, _) -> ok end),
    meck:expect(emdb_nif, cursor_open, fun(_, _) -> {ok, make_ref()} end),
    meck:expect(emdb_nif, cursor_close, fun(_) -> ok end),
    meck:expect(emdb_nif, cursor_get, fun(_, _) -> {error, notfound} end).

unmeck_nif() ->
    meck:unload(emdb_nif).
