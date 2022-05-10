%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <xramtsov@gmail.com>
%%% @copyright (C) 2022, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  9 May 2022 by Evgeny Khramtsov <xramtsov@gmail.com>
%%%-------------------------------------------------------------------
-module(emdb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 1},
    ChildSpecs = [#{id => emdb_env,
                    start => {emdb_env, start_link, []},
                    shutdown => timer:seconds(30)}],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
