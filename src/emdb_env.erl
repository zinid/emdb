%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <xramtsov@gmail.com>
%%% @copyright (C) 2022, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 10 May 2022 by Evgeny Khramtsov <xramtsov@gmail.com>
%%%-------------------------------------------------------------------
-module(emdb_env).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0]).
-export([open_table/2]).
-export([close_table/1]).
-export([get/0]).
-export([dbi/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).
-export_type([dbi_flag/0]).
-export_type([env/0]).

-define(CALL_TIMEOUT, timer:minutes(1)).

-record(state, {env :: env()}).

-type state() :: #state{}.
-opaque env() :: reference().
-type dbi_flag() :: reversekey
                  | dupsort
                  | integerkey
                  | dupfixed
                  | get_multiple
                  | next_multiple
                  | integerdup
                  | integerkey
                  | reversedup
                  | create.
-type env_flag() :: fixedmap
                  | nosubdir
                  | rdonly
                  | writemap
                  | nometasync
                  | nosync
                  | mapasync
                  | notls
                  | nolock
                  | nordahead
                  | nomeminit.

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec open_table(atom(), [dbi_flag()]) -> ok | {error, term()}.
open_table(Name, Flags) when is_atom(Name) ->
    EncFlags = emdb_nif:encode_flags(Flags),
    gen_server:call(?MODULE, {open_table, Name, EncFlags}, ?CALL_TIMEOUT);
open_table(Name, Flags) ->
    erlang:error(badarg, [Name, Flags]).

-spec close_table(atom()) -> ok | {error, term()}.
close_table(Name) ->
    gen_server:call(?MODULE, {close_table, Name}, ?CALL_TIMEOUT).

-spec dbi(atom()) -> integer().
dbi(Name) ->
    case ets:lookup(emdb_tables, Name) of
        [{_, Dbi}] -> Dbi;
        [] -> erlang:error({table_not_found, Name})
    end.

-spec get() -> env().
get() ->
    persistent_term:get(?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init([]) -> {ok, state()} | {stop, term()}.
init([]) ->
    process_flag(trap_exit, true),
    logger:set_process_metadata(#{domain => [emdb, env]}),
    case get_vars() of
        {ok, #{dir := Dir, flags := Flags, opts := Opts, file_mode := Mode}} ->
            case emdb_nif:env_create() of
                {ok, Env} ->
                    case setopts(Env, Opts) of
                        ok ->
                            case emdb_nif:env_open(
                                   Env, Dir, emdb_nif:encode_flags(Flags), Mode) of
                                ok ->
                                    ets:new(emdb_tables,
                                            [named_table, public,
                                             {read_concurrency, true}]),
                                    persistent_term:put(?MODULE, Env),
                                    {ok, #state{env = Env}};
                                {error, Reason} ->
                                    ?LOG_CRITICAL("Failed to open LMDB environment: ~s",
                                                  [emdb:format_error(Reason)]),
                                    _ = emdb_nif:env_close(Env),
                                    {stop, Reason}
                            end;
                        {error, Reason} ->
                            _ = emdb_nif:env_close(Env),
                            {stop, Reason}
                    end;
                {error, Reason} ->
                    ?LOG_CRITICAL("Failed to create LMDB environment: ~s",
                                  [emdb:format_error(Reason)]),
                    {stop, Reason}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, state()) ->
          {noreply, state()} | {reply, term(), state()}.
handle_call({open_table, Name, EncFlags}, _From, State) ->
    Reply = case ets:member(emdb_tables, Name) of
                true -> ok;
                false -> dbi_open(Name, EncFlags)
            end,
    {reply, Reply, State};
handle_call({close_table, Name}, _From, State) ->
    Reply = case ets:lookup(emdb_tables, Name) of
                [{_, Dbi}] ->
                    dbi_close(State#state.env, Name, Dbi);
                [] ->
                    {error, notfound}
            end,
    {reply, Reply, State};
handle_call(Request, {Pid, _}, State) ->
    ?LOG_WARNING("unexpected call from ~0p: ~0p", [Pid, Request]),
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(Request, State) ->
    ?LOG_WARNING("unexpected cast: ~0p", [Request]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
    ?LOG_WARNING("unexpected info: ~0p", [Info]),
    {noreply, State}.

-spec terminate(term(), state()) -> any().
terminate(_Reason, _State) ->
    persistent_term:erase(?MODULE).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec env_flags() -> [env_flag()].
env_flags() ->
    [fixedmap, nosubdir, rdonly, writemap,
     nometasync, nosync, mapasync, notls,
     nolock, nordahead, nomeminit].

setopts(Env, [{Opt, Val}|Opts]) ->
    case setopt(Env, Opt, Val) of
        ok -> setopts(Env, Opts);
        {error, Reason} = Err ->
            ?LOG_CRITICAL("Failed to set LMDB environment option '~s': ~s",
                          [Opt, emdb:format_error(Reason)]),
            Err
    end;
setopts(_, _) ->
    ok.

setopt(Env, mapsize, Val) when is_integer(Val), Val > 0 ->
    emdb_nif:env_set_mapsize(Env, Val);
setopt(Env, maxdbs, Val) when is_integer(Val), Val > 0 ->
    emdb_nif:env_set_maxdbs(Env, Val);
setopt(Env, maxreaders, Val) when is_integer(Val), Val > 0 ->
    emdb_nif:env_set_maxreaders(Env, Val);
setopt(Env, Key, Val) ->
    erlang:error(badarg, [Env, Key, Val]).

dbi_open(Name, EncFlags) ->
    try emdb_txn:txn(
          fun() ->
                  Txn = emdb_txn:get(),
                  case emdb_nif:dbi_open(Txn, Name, EncFlags) of
                      {ok, Dbi} ->
                          ets:insert(emdb_tables, {Name, Dbi}),
                          ok;
                      {error, _} = Err ->
                          Err
                  end
          end)
    catch error:{aborted, _} = Reason ->
            {error, Reason}
    end.

dbi_close(Env, Name, Dbi) ->
    ets:delete(emdb_tables, Name),
    emdb_nif:dbi_close(Env, Dbi).

get_vars() ->
    get_vars([dir, flags, mapsize, maxdbs, maxreaders, file_mode],
             #{opts => []}).

get_vars([dir|Vars], Acc) ->
    DefaultDir = case file:get_cwd() of
                     {ok, Cwd} -> filename:join(Cwd, "emdb");
                     _ -> "emdb"
                 end,
    Dir = application:get_env(emdb, dir, DefaultDir),
    case filelib:ensure_dir(filename:join(Dir, "foo")) of
        ok ->
            get_vars(Vars, Acc#{dir => Dir});
        {error, Reason} = Err ->
            ?LOG_CRITICAL("Failed to create directory ~ts: ~s",
                          [Dir, file:format_error(Reason)]),
            Err
    end;
get_vars([flags|Vars], Acc) ->
    Flags = lists:filter(
              fun(Flag) ->
                      application:get_env(emdb, Flag, false) == true
              end, env_flags()),
    get_vars(Vars, Acc#{flags => Flags});
get_vars([file_mode|Vars], Acc) ->
    case application:get_env(emdb, file_mode, 8#644) of
        Mode when is_integer(Mode), Mode >= 0, Mode =< 8#1777 ->
            get_vars(Vars, Acc#{file_mode => Mode});
        Junk ->
            ret_einval(file_mode, Junk)
    end;
get_vars([Key|Vars], #{opts := Opts} = Acc) when Key == mapsize;
                                                 Key == maxdbs;
                                                 Key == maxreaders ->
    case get_pos_int(Key, default(Key)) of
        {ok, Val} ->
            get_vars(Vars, Acc#{opts => [{Key, Val}|Opts]});
        {error, Junk} ->
            ret_einval(Key, Junk)
    end;
get_vars([], Acc) ->
    {ok, Acc}.

default(mapsize) -> 1024*1024;
default(maxdbs) -> 10;
default(maxreaders) -> 128.

get_pos_int(Key, Default) ->
    case application:get_env(emdb, Key, Default) of
        Val when is_integer(Val), Val > 0 ->
            {ok, Val};
        Junk ->
            {error, Junk}
    end.

ret_einval(Key, Val) ->
    ?LOG_CRITICAL("Invalid value of environment variable '~s': ~0p", [Key, Val]).
