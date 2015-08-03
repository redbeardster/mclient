
-module(mclient_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

RestartStrategy = one_for_one,
    MaxRestarts = 1, % one restart every
    MaxSecondsBetweenRestarts = 5, % five seconds
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent, % or temporary, or transient
    Shutdown = 2000, % milliseconds, could be infinity or brutal_kill
    Type = worker, % could also be supervisor
    Drop = {mclient, {mclient, start_link, []},
        Restart, Shutdown, Type, [mclient]},
    {ok, {SupFlags, [Drop]}}.

