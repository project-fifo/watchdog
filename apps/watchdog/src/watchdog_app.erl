-module(watchdog_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Port = case application:get_env(watchdog, port) of
               {ok, APort} ->
                   APort;
               _ ->
                   4444
           end,
    Listeners = 10,
    {ok, _} = ranch:start_listener(watchdog_tcp, Listeners,
                                   ranch_tcp, [{port, Port}],
                                   watchdog_tcp, []),
    watchdog_sup:start_link().

stop(_State) ->
    ok.
