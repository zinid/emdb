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
start_test() ->
    set_db_dir(),
    del_db_dir(),
    ok = emdb:start().

double_start_test() ->
    ok = emdb:start().

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

stop_test() ->
    ok = emdb:stop(),
    del_db_dir().

double_stop_test() ->
    ok = emdb:stop().

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
