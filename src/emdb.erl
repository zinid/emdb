%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <xramtsov@gmail.com>
%%% @copyright (C) 2022, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  9 May 2022 by Evgeny Khramtsov <xramtsov@gmail.com>
%%%-------------------------------------------------------------------
-module(emdb).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
%% API
-export([start/0, stop/0]).
-export([txn/1, txn/2]).
-export([open_table/1, open_table/2]).
-export([close_table/1]).
-export([first/1]).
-export([last/1]).
-export([put/3, put/4]).
-export([get/2]).
-export([del/2, del/3]).
-export([version/0]).
-export([format_error/1]).
%% Exported types
-export_type([error_reason/0]).
-export_type([put_flag/0]).
-export_type([cursor_op/0]).

-type error_reason() :: {errno, integer()} | atom().
-type tabname() :: atom().
-type put_flag() :: nodupdata
                  | nooverwrite
                  | reserve
                  | append
                  | appenddup.
-type cursor_op() :: first
                   | first_dup
                   | get_both
                   | get_both_range
                   | get_current
                   | get_multiple
                   | last
                   | last_dup
                   | next
                   | next_dup
                   | next_multiple
                   | next_nodup
                   | prev
                   | prev_dup
                   | prev_nodup
                   | set
                   | set_key
                   | set_range.

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-spec start(normal | {takeover | failover, node()}, _) ->
          {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    emdb_sup:start_link().

-spec stop(_) -> any().
stop(_State) ->
    ok.

%%%===================================================================
%%% API
%%%===================================================================
-spec start() -> ok | {error, _}.
start() ->
    case application:ensure_all_started(?MODULE) of
        {ok, _} -> ok;
        {error, _} = Err -> Err
    end.

-spec stop() -> ok | {error, _}.
stop() ->
    case application:stop(?MODULE) of
        ok -> ok;
        {error, {not_started, _}} -> ok;
        {error, _} = Err -> Err
    end.

-spec version() -> {Major :: integer(), Minor :: integer(), Patch :: integer()}.
version() ->
    emdb_nif:version().

-spec open_table(tabname()) -> ok | {error, error_reason()}.
open_table(TabName) ->
    open_table(TabName, [create]).

-spec open_table(tabname(), [emdb_env:dbi_flag()]) -> ok | {error, error_reason()}.
open_table(TabName, Flags) ->
    emdb_env:open_table(TabName, Flags).

-spec close_table(tabname()) -> ok | {error, error_reason()}.
close_table(TabName) ->
    emdb_env:close_table(TabName).

-spec txn(emdb_txn:txn_fun()) -> term().
txn(F) ->
    emdb_txn:txn(F).

-spec txn(emdb_txn:txn_fun(), [emdb_txn:txn_flag()]) -> term().
txn(F, Flags) ->
    emdb_txn:txn(F, Flags).

-spec put(tabname(), term(), term()) -> ok | {error, error_reason()}.
put(TabName, Key, Val) ->
    put(TabName, Key, Val, []).

-spec put(tabname(), term(), term(), [put_flag()]) -> ok | {error, error_reason()}.
put(TabName, Key, Val, Flags) ->
    emdb_nif:put(emdb_txn:get(),
                 emdb_env:dbi(TabName),
                 term_to_binary(Key),
                 term_to_binary(Val),
                 emdb_nif:encode_flags(Flags)).

-spec get(tabname(), term()) -> {ok, term()} | {error, error_reason()}.
get(TabName, Key) ->
    case emdb_nif:get(emdb_txn:get(),
                      emdb_env:dbi(TabName),
                      term_to_binary(Key)) of
        {ok, Val} -> {ok, binary_to_term(Val)};
        {error, _} = Err -> Err
    end.

-spec del(tabname(), term()) -> ok | {error, error_reason()}.
del(TabName, Key) ->
    del(TabName, Key, undefined).

-spec del(tabname(), term(), term()) -> ok | {error, error_reason()}.
del(TabName, Key, Val) ->
    emdb_nif:del(emdb_txn:get(),
                 emdb_env:dbi(TabName),
                 term_to_binary(Key),
                 term_to_binary(Val)).

-spec first(tabname()) -> {ok, {term(), term()}} | {error, error_reason()}.
first(TabName) ->
    cursor_get(TabName, first).

-spec last(tabname()) -> {ok, {term(), term()}} | {error, error_reason()}.
last(TabName) ->
    cursor_get(TabName, last).

-spec format_error(term()) -> iolist().
format_error(Reason) when is_atom(Reason) ->
    try emdb_nif:strerror(emdb_nif:encode_errtag(Reason))
    catch error:badarg ->
            atom_to_list(Reason)
    end;
format_error({errno, Errno}) ->
    "errno = " ++ integer_to_list(Errno);
format_error(Term) ->
    io_lib:format("~0p", [Term]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
cursor_get(TabName, Op) ->
    case emdb_nif:cursor_open(emdb_txn:get(),
                              emdb_env:dbi(TabName)) of
        {ok, Cur} ->
            EncOp = emdb_nif:encode_cursor_op(Op),
            Ret = case emdb_nif:cursor_get(Cur, EncOp) of
                      {ok, {Key, Val}} ->
                          {ok, {binary_to_term(Key),
                                binary_to_term(Val)}};
                      {error, _} = Err ->
                          Err
                  end,
            emdb_nif:cursor_close(Cur),
            Ret;
        {error, _} = Err ->
            Err
    end.
