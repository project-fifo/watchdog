-module(watchdog).
-export([start/0]).
-ignore_xref([start/0]).

start() ->
    application:ensure_all_started(watchdog).
