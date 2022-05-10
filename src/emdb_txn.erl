%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <xramtsov@gmail.com>
%%% @copyright (C) 2022, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 10 May 2022 by Evgeny Khramtsov <xramtsov@gmail.com>
%%%-------------------------------------------------------------------
-module(emdb_txn).

%% API
-export([txn/1, txn/2]).
-export([get/0]).
%% Exported types
-export_type([txn/0]).
-export_type([txn_fun/0]).
-export_type([txn_flag/0]).

-define(TXN_REF, {?MODULE, ref}).

-opaque txn() :: reference().
-type txn_fun() :: fun(() -> any()).
-type txn_flag() :: rdonly.

%%%===================================================================
%%% API
%%%===================================================================
-spec txn(txn_fun()) -> term().
txn(F) ->
    txn(F, 0).

-spec txn(txn_fun(), [txn_flag()] | non_neg_integer()) -> term().
txn(F, Flags) when is_list(Flags) ->
    txn(F, emdb_nif:encode_flags(Flags));
txn(F, EncFlags) ->
    check_nested(),
    case emdb_nif:txn_begin(emdb_env:get(), EncFlags) of
        {ok, Txn} ->
            erlang:put(?TXN_REF, Txn),
            try F() of
                Ret ->
                    case emdb_nif:txn_commit(Txn) of
                        ok ->
                            Ret;
                        {error, Reason} ->
                            erlang:error({aborted, {commit_failed, Reason}})
                    end
            catch E:R:St ->
                    emdb_nif:txn_abort(Txn),
                    erlang:raise(E, R, St)
            after
                erlang:erase(?TXN_REF)
            end;
        {error, Reason} ->
            erlang:error({aborted, {begin_failed, Reason}})
    end.

-spec get() -> txn().
get() ->
    case erlang:get(?TXN_REF) of
        undefined ->
            erlang:error(transaction_not_found);
        Txn ->
            Txn
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_nested() ->
    case erlang:get(?TXN_REF) of
        undefined -> ok;
        _ -> erlang:error(nested_transaction)
    end.
