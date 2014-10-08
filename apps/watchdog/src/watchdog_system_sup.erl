-module(watchdog_system_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-ignore_xref([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Element = {watchdog_system, {watchdog_system, start_link, []},
               transient, infinity, worker, [watchdog_system]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.

