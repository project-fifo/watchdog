-module(watchdog_tcp).

-behaviour(ranch_protocol).

-define(KEEPALIVE, 0).
-define(LIST, 1).
-define(GET, 2).
-define(QRY, $q).


-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    Transport:setopts(Socket, [binary, {packet, 4}, {active, false}]),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Data} ->
            case binary_to_term(Data) of
                {{_C, _S, _N} = CSN, Vsn, Msg} ->
                    watchdog_system:notify(CSN, Vsn, Msg);
                T ->
                    lager:warning("[tcp] Unknown term: ~p", [T])
            end,
            loop(Socket, Transport);
        {error,timeout} ->
            loop(Socket, Transport);
        E ->
            lager:warning("[tcp] closing with: ~p", [E]),
            ok = Transport:close(Socket)
    end.
