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
-export([start_link/0, clear/5, raise/6, mfa/7, file/6]).
-ignore_xref([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {server, token, socket}).

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

file(Cluster, System, Node, File, Line, Level) ->
    gen_server:cast(?SERVER, {file, Cluster, System, Node, File, Line, Level}).

mfa(Cluster, System, Node, Module, Function, Arity, Level) ->
    gen_server:cast(?SERVER, {mfa, Cluster, System, Node, Module, Function,
                              Arity, Level}).

raise(Cluster, System, Node, Type, Alert, Severity) ->
    gen_server:cast(?SERVER, {rise_alert, Cluster, System, Node, Type, Alert,
                              Severity}).

clear(Cluster, System, Node, Type, Alert) ->
    gen_server:cast(?SERVER, {clear_alert, Cluster, System, Node, Type, Alert}).

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
                                token = Token}) ->
    case gen_tcp:connect(Addr, Port, [binary, {packet, 2}], 500) of
        {ok, Sock} ->
            lager:info("[upstream] Connected to ~s:~p", [Addr, Port]),
            {ok, Bin} = encode({auth, Token}),
            gen_tcp:send(Sock, Bin),
            handle_cast(Msg, State#state{socket = Sock});
        E ->
            lager:error("[upstream] Error sending: ~p", [E]),
            {noreply, State}
    end;

handle_cast(Msg, State = #state{socket = Sock}) ->
    case encode(Msg) of
        {ok, Bin} ->
            case gen_tcp:send(Sock, Bin) of
                ok ->
                    {noreply, State};
                E ->
                    lager:error("[upstream] Error sending: ~p", [E]),
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

encode({file, _Cluster, _System, _Node, _File, _Line, Level}) when Level =< 0 ->
    ignored;

encode({mfa, _Cluster, _System, _Node, _Module, _Function, _Arity, Level})
  when Level =< 0 ->
    ignored;

encode({file, Cluster, System, Node, File, Line, Level}) ->
    {ok, <<1,
           (byte_size(Cluster)):8, Cluster/binary,
           (byte_size(System)):8, System/binary,
           (byte_size(Node)):8, Node/binary,
           (byte_size(File)):8, File/binary,
           Line:16, Level:8>>};

encode({mfa, Cluster, System, Node, Module, Function, Arity, Level}) ->
    {ok, <<2,
           (byte_size(Cluster)):8, Cluster/binary,
           (byte_size(System)):8, System/binary,
           (byte_size(Node)):8, Node/binary,
           (byte_size(Module)):8, Module/binary,
           (byte_size(Function)):8, Function/binary,
           Arity:8, Level:8>>};

encode({rise_alert, Cluster, System, Node, Type, Alert, Severity}) ->
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

encode(_) ->
    bad_message.
