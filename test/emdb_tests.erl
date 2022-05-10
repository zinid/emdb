%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <xramtsov@gmail.com>
%%% @copyright (C) 2022, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  9 May 2022 by Evgeny Khramtsov <xramtsov@gmail.com>
%%%-------------------------------------------------------------------
-module(emdb_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================
not_running_test() ->
    ?assertError(emdb_not_running, emdb_env:get()).

start_test() ->
    set_db_dir(),
    del_db_dir(),
    ok = emdb:start().

double_start_test() ->
    ok = emdb:start().

version_test() ->
    {_, _, _} = emdb:version().

unexpected_msg_test() ->
    emdb_env ! unexpected_info,
    ?assertExit({timeout, _}, gen_server:call(emdb_env, unexpected_call, 100)),
    gen_server:cast(emdb_env, unexpected_cast).

open_close_table_test() ->
    T = ?FUNCTION_NAME,
    ok = emdb:open_table(T),
    ok = emdb:close_table(T).

put_get_del_test() ->
    Key = rand(),
    Val = val(),
    T = ?FUNCTION_NAME,
    ok = emdb:open_table(T),
    Err = emdb:txn(fun() ->
                           ok = emdb:put(T, Key, Val),
                           {ok, Val} = emdb:get(T, Key),
                           ok = emdb:del(T, Key),
                           {error, notfound} = emdb:get(T, Key)
                   end),
    test_err(Err),
    ok = emdb:close_table(T).

first_last_test() ->
    T = ?FUNCTION_NAME,
    ok = emdb:open_table(T),
    emdb:txn(fun() ->
                     {error, notfound} = emdb:first(T),
                     {error, notfound} = emdb:last(T),
                     lists:foreach(
                       fun(I) ->
                               emdb:put(T, I, I)
                       end, lists:seq(1, 1000)),
                     {ok, {1, 1}} = emdb:first(T),
                     {ok, {1000, 1000}} = emdb:last(T)
             end),
    ok = emdb:close_table(T).

rdonly_transaction_test() ->
    Key = rand(),
    Val = val(),
    T = ?FUNCTION_NAME,
    ok = emdb:open_table(T),
    emdb:txn(fun() ->
                     ok = emdb:put(T, Key, Val)
             end),
    emdb:txn(fun() ->
                     {ok, Val} = emdb:get(T, Key)
             end, [rdonly]),
    ok = emdb:close_table(T).

write_in_rdonly_transaction_test() ->
    Key = rand(),
    Val = val(),
    T = ?FUNCTION_NAME,
    ok = emdb:open_table(T),
    Err = emdb:txn(fun() ->
                           {error, eacces} = emdb:put(T, Key, Val)
                   end, [rdonly]),
    test_err(Err),
    ok = emdb:close_table(T).

table_not_found_test() ->
    Key = rand(),
    Val = val(),
    T = ?FUNCTION_NAME,
    ?assertError({table_not_found, T},
                 emdb:txn(fun() ->
                                  emdb:put(T, Key, Val)
                          end)).

transaction_not_found_test() ->
    Key = rand(),
    Val = val(),
    T = ?FUNCTION_NAME,
    ?assertError(transaction_not_found, emdb:put(T, Key, Val)).

nested_transaction_test() ->
    ?assertError(nested_transaction,
                 emdb:txn(fun() ->
                                  emdb:txn(fun() -> ok end)
                          end)).

abort_test() ->
    Key = rand(),
    Val = val(),
    T = ?FUNCTION_NAME,
    ok = emdb:open_table(T),
    ?assertError(aborted,
                 emdb:txn(fun() ->
                                  ok = emdb:put(T, Key, Val),
                                  erlang:error(aborted)
                          end)),
    emdb:txn(fun() ->
                     {error, notfound} = emdb:get(T, Key)
             end),
    ok = emdb:close_table(T).

invalid_table_name_test() ->
    ?assertError(badarg, emdb:open_table(1)),
    ?assertError(badarg, emdb:close_table(1)),
    ?assertError(badarg, emdb_env:dbi(1)).

format_error_test() ->
    "foo" = emdb:format_error(foo),
    Errno = emdb_nif:encode_errtag(enomem),
    emdb:format_error({errno, Errno}),
    emdb:format_error({foo, bar}).

cursor_failed_test() ->
    Err = {error, einval},
    T = ?FUNCTION_NAME,
    ok = emdb:open_table(T),
    meck:new(emdb_nif),
    meck:expect(emdb_nif, txn_begin, fun(_, _) -> {ok, make_ref()} end),
    meck:expect(emdb_nif, txn_abort, fun(_) -> ok end),
    meck:expect(emdb_nif, txn_commit, fun(_) -> ok end),
    meck:expect(emdb_nif, cursor_open, fun(_, _) -> Err end),
    Err = emdb:txn(fun() -> emdb:first(T) end),
    meck:unload(emdb_nif).

aborted_test() ->
    Reason = einval,
    meck:new(emdb_nif),
    meck:expect(emdb_nif, txn_begin, fun(_, _) -> {error, Reason} end),
    ?assertError({aborted, {begin_failed, Reason}},
                 emdb:txn(fun() -> ok end)),
    meck:expect(emdb_nif, txn_begin, fun(_, _) -> {ok, make_ref()} end),
    meck:expect(emdb_nif, txn_commit, fun(_) -> {error, Reason} end),
    ?assertError({aborted, {commit_failed, Reason}},
                 emdb:txn(fun() -> ok end)),
    meck:unload(emdb_nif).

stop_test() ->
    logger:set_primary_config(#{level => critical}),
    ok = emdb:stop(),
    del_db_dir().

double_stop_test() ->
    ok = emdb:stop().

start_failed_test() ->
    Error = {error, einval},
    meck:new(emdb_nif),
    meck:expect(emdb_nif, encode_errtag, fun(Tag) -> atom_to_list(Tag) end),
    meck:expect(emdb_nif, strerror, fun(S) -> S end),
    meck:expect(emdb_nif, env_set_maxreaders, fun(_, _) -> ok end),
    meck:expect(emdb_nif, env_set_maxdbs, fun(_, _) -> ok end),
    meck:expect(emdb_nif, env_set_mapsize, fun(_, _) -> ok end),
    meck:expect(emdb_nif, encode_flags, fun(_) -> 0 end),
    meck:expect(emdb_nif, env_close, fun(_) -> true end),
    meck:expect(emdb_nif, env_create, fun() -> Error end),
    meck:expect(emdb_nif, env_open, fun(_, _, _, _) -> Error end),
    emdb:start(),
    meck:expect(emdb_nif, env_create, fun() -> {ok, make_ref()} end),
    emdb:start(),
    meck:unload().

invalid_dir_test() ->
    application:set_env(emdb, dir, "/root/foo/bar"),
    emdb:start(),
    set_db_dir().

invalid_env_opts_test() ->
    Opts = [file_mode, mapsize, maxdbs, maxreaders],
    lists:foreach(
      fun(Opt) ->
              application:set_env(emdb, Opt, -1),
              application:start(emdb),
              application:unset_env(emdb, Opt)
      end, Opts).

%%%===================================================================
%%% Internal functions
%%%===================================================================
test_err({error, Reason}) ->
    emdb:format_error(Reason),
    emdb_nif:strerror(emdb_nif:encode_errtag(Reason)).

rand() ->
    rand:uniform(1 bsl 64).

val() ->
    val.

set_db_dir() ->
    Dir = filename:join(["_build", "test", "eunit", "emdb"]),
    application:load(emdb),
    application:set_env(emdb, dir, Dir).

del_db_dir() ->
    {ok, Dir} = application:get_env(emdb, dir),
    file:del_dir_r(Dir).
