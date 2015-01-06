%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@schroedinger.local>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2014 by Heinz Nikolaus Gies <heinz@schroedinger.local>
%%%-------------------------------------------------------------------
-module(watchdog_upstream).

-behaviour(gen_server).

%% API
-export([start_link/0, clear/5, raise/6, mfa/9, file/8, ping/4]).
-ignore_xref([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {server, token, socket, connect_timeout = 5000}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

file(Cluster, System, Node, Vsn, File, Line, Error, Level) ->
    gen_server:cast(?SERVER, {file, Cluster, System, Node, Vsn, File, Line,
                              Error, Level}).

mfa(Cluster, System, Node, Vsn, Module, Function, Arity, Error, Severity) ->
    gen_server:cast(?SERVER, {mfa, Cluster, System, Node, Vsn, Module, Function,
                              Arity, Error, Severity}).

raise(Cluster, System, Node, Type, Alert, Severity) ->
    gen_server:cast(?SERVER, {raise_alert, Cluster, System, Node, Type, Alert,
                              Severity}).

clear(Cluster, System, Node, Type, Alert) ->
    gen_server:cast(?SERVER, {clear_alert, Cluster, System, Node, Type, Alert}).

ping(Cluster, System, Node, Vsn) ->
    gen_server:cast(?SERVER, {ping, Cluster, System, Node, Vsn}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Server = case application:get_env(watchdog, upstream_server) of
                 {ok, S} -> S;
                 undefined -> undefined
             end,
    Token = case application:get_env(watchdog, auth_token) of
                {ok, T} -> <<_:36/binary>> = list_to_binary(T);
                undefined -> undefined
            end,
    {ok, #state{server = Server, token = Token}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State = #state{server = undefined}) ->
    {noreply, State};


handle_cast(_Msg, State = #state{token = undefined}) ->
    {noreply, State};

handle_cast(Msg, State = #state{socket = undefined, server = {Addr, Port},
                                token = Token, connect_timeout = Timeout}) ->
    case ssl:connect(Addr, Port, [binary, {packet, 2}], Timeout) of
        {ok, Sock} ->
            lager:info("[upstream] Connected to ~s:~p", [Addr, Port]),
            {ok, Bin} = encode({auth, Token}),
            ssl:send(Sock, Bin),
            handle_cast(Msg, State#state{socket = Sock});
        E ->
            lager:error("[upstream] Error connecting to ~s:~p: ~p",
                        [Addr, Port, E]),
            {noreply, State}
    end;

handle_cast(Msg, State = #state{socket = Sock}) ->
    case encode(Msg) of
        {ok, Bin} ->
            case ssl:send(Sock, Bin) of
                ok ->
                    {noreply, State};
                E ->
                    lager:error("[upstream] Error sending: ~p", [E]),
                    ssl:close(Sock),
                    {noreply, State#state{socket = undefined}}
            end;
        _ ->
            {noreply, State}
    end;


handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

encode({auth, Token}) ->
    {ok, <<0, Token:36/binary>>};

encode({file, _Cluster, _System, _Node, _File, _Line, _Error, Level})
  when Level =< 0 ->
    ignored;

encode({mfa, _Cluster, _System, _Node, _Module, _Function, _Arity, _Error, Level})
  when Level =< 0 ->
    ignored;

encode({file, Cluster, System, Node, Vsn, File, Line, Error, Level}) ->
    {ok, <<1,
           (byte_size(Cluster)):8, Cluster/binary,
           (byte_size(System)):8, System/binary,
           (byte_size(Node)):8, Node/binary,
           (byte_size(Vsn)):8, Vsn/binary,
           (byte_size(File)):8, File/binary,
           Line:16,
           (byte_size(Error)):8, Error/binary,
           Level:8>>};

encode({mfa, Cluster, System, Node, Vsn, Module, Function, Arity, Error, Level}) ->
    {ok, <<2,
           (byte_size(Cluster)):8, Cluster/binary,
           (byte_size(System)):8, System/binary,
           (byte_size(Node)):8, Node/binary,
           (byte_size(Vsn)):8, Vsn/binary,
           (byte_size(Module)):8, Module/binary,
           (byte_size(Function)):8, Function/binary,
           Arity:8,
           (byte_size(Error)):8, Error/binary,
           Level:8>>};

encode({raise_alert, Cluster, System, Node, Type, Alert, Severity}) ->
    {ok, <<3,
           (byte_size(Cluster)):8, Cluster/binary,
           (byte_size(System)):8, System/binary,
           (byte_size(Node)):8, Node/binary,
           (byte_size(Type)):8, Type/binary,
           (byte_size(Alert)):8, Alert/binary,
           Severity:8>>};

encode({clear_alert, Cluster, System, Node, Type, Alert}) ->
    {ok, <<4,
           (byte_size(Cluster)):8, Cluster/binary,
           (byte_size(System)):8, System/binary,
           (byte_size(Node)):8, Node/binary,
           (byte_size(Type)):8, Type/binary,
           (byte_size(Alert)):8, Alert/binary>>};

encode({ping, Cluster, System, Node, Vsn}) ->
    {ok, <<5,
           (byte_size(Cluster)):8, Cluster/binary,
           (byte_size(System)):8, System/binary,
           (byte_size(Node)):8, Node/binary,
           (byte_size(Vsn)):8, Vsn/binary>>};

encode(Msg) ->
    lager:error("[watchdog:ssl] Unsupported message: ~p", [Msg]),
    bad_message.
