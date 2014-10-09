%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@schroedinger.local>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created :  8 Oct 2014 by Heinz Nikolaus Gies <heinz@schroedinger.local>
%%%-------------------------------------------------------------------
-module(watchdog_system).

-behaviour(gen_server).

%% API
-export([start_link/4, notify/2]).

-ignore_xref([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TICK, 10*1000). %10s

-define(INFO_THRESHOLD, undefined).
-define(WARN_THRESHOLD, 50).
-define(ERROR_THRESHOLD, 5).
-define(CRASH_THRESHOLD, 1).

-record(state, {type, cluster, system, node, id, tbl}).

%%%===================================================================
%%% API
%%%===================================================================

notify({C, S, N}, Msg) ->
    CPid = case gproc:where({n, l, {cluster, C}}) of
               undefined ->
                   {ok,CPidX} = start(cluster, C, S, N),
                   CPidX;
               CPidX ->
                   CPidX
           end,
    gen_server:cast(CPid, {notify, Msg}),
    SPid = case gproc:where({n, l, {system, S}}) of
               undefined ->
                   {ok, SPidX} = start(system, C, S, N),
                   SPidX;
               SPidX ->
                   SPidX
    end,
    gen_server:cast(SPid, {notify, Msg}),
    NPid = case gproc:where({n, l, {node, N}}) of
               undefined ->
                   {ok, NPidX} = start(node, C, S, N),
                   NPidX;
               NPidX ->
                   NPidX
           end,
    gen_server:cast(NPid, {notify, Msg}).

start(Type, C, S, N) ->
    supervisor:start_child(watchdog_system_sup, [Type, C, S, N]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Type, C, S, N) ->
    gen_server:start_link(?MODULE, [Type, C, S, N], []).

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
init([cluster, C, S, N]) ->
    erlang:send_after(?TICK, self(), tick),
    gproc:reg({n, l, {cluster, C}}, cluster),
    {ok, #state{type = cluster, cluster = C, system = S, node = N, id = C}};
init([system, C, S, N]) ->
    erlang:send_after(?TICK, self(), tick),
    gproc:reg({n, l, {system, S}}, system),
    {ok, #state{type = system, cluster = C, system = S, node = N, id = {C, S}}};
init([node, C, S, N]) ->
    erlang:send_after(?TICK, self(), tick),
    gproc:reg({n, l, {node, N}}, node),
    {ok, #state{type = node, cluster = C, system = S, node = N, id = {C, S, N}}}.

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
handle_cast({notify, {{lager, Lvl}, {flf, File, Line, _Function}}},
            State = #state{id = ID}) ->
    Name = {ID, {File, Line}},
    case folsom_metrics:new_spiral(Name) of
        ok ->
            folsom_metrics:tag_metric(Name, {ID, Lvl});
        _ ->
            ok
    end,
    folsom_metrics:notify({Name, 1}),
    {noreply, State};

handle_cast({notify, {_Error, {mfaf,{_M, _F, _A, {File,Line}}}}},
            State = #state{id = ID}) ->
    Name = {ID, {File, Line}},
    case folsom_metrics:new_spiral(Name) of
        ok ->
            folsom_metrics:tag_metric(Name, {ID, crash});
        _ ->
            ok
    end,
    folsom_metrics:notify({Name, 1}),
    {noreply, State};

handle_cast({notify, {_Error, {mfa,{gen_server,_ , _}}}}, State) ->
    {noreply, State};

handle_cast({notify, {_Error, {mfa, MFA}}},
            State = #state{id = ID}) ->
    Name = {ID, MFA},
    case folsom_metrics:new_spiral(Name) of
        ok ->
            folsom_metrics:tag_metric(Name, {ID, crash});
        _ ->
            ok
    end,
    folsom_metrics:notify({Name, 1}),
    {noreply, State};

handle_cast({notify, Msg}, State) ->
    io:format("Msg: ~p~n", [Msg]),
    {noreply, State};

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
handle_info(tick, State = #state{id = ID}) ->
    EType = case State#state.type of
                node -> node_error;
                system -> system_error;
                cluster -> cluster_error
            end,
    run(ID, info, EType, ?INFO_THRESHOLD),
    run(ID, warning, EType, ?WARN_THRESHOLD),
    run(ID, error, EType, ?ERROR_THRESHOLD),
    run(ID, crash, EType, ?CRASH_THRESHOLD),
    erlang:send_after(?TICK, self(), tick),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

run(_ID, _Lvl, _, undefined) ->
    0;

run(ID, Lvl, EType, Threshold) ->
    case run_list(folsom_metrics:get_metrics_value({ID, Lvl}), Threshold, 0) of
        E when E >= Threshold ->
            elarm:raise(EType, <<"Over threshold">>, [{level, E}]),
            error;
        _ ->
            elarm:clear(EType, <<"Over threshold">>),
            ok
    end.

run_list([{{_ID, {File, Line}}, [{count, _Cnt},{one, One}]} | R],
         Threshold, Total)
  when One >= Threshold ->
    elarm:raise(file_error, <<File/binary, ":", (i2b(Line))/binary>>,
                [{level, One}]),
    run_list(R, Threshold, Total + One);

run_list([{{_ID, {File, Line}} = Metric, [{count, _Cnt}, {one, 0}]} | R],
         Threshold, Total) ->
    folsom_metrics:delete_metric(Metric),
    elarm:clear(file_error, <<File/binary, ":", (i2b(Line))/binary>>),
    run_list(R, Threshold, Total);

run_list([{{_ID, {File, Line}}, [{count, _Cnt},{one, One}]} | R],
         Threshold, Total) ->
    elarm:clear(file_error, <<File/binary, ":", (i2b(Line))/binary>>),
    run_list(R, Threshold, Total + One);

run_list([{{_ID, {M, F, A}}, [{count, _Cnt}, {one, One}]} | R],
         Threshold, Total)
  when One >= Threshold ->
    elarm:raise(function_error,
                <<(a2b(M))/binary, $:, (a2b(F))/binary, $/, (i2b(A))/binary>>,
                [{level, One}]),
    run_list(R, Threshold, Total + One);

run_list([{{_ID, {M, F, A}} = Metric, [{count, _Cnt}, {one, 0}]} | R],
         Threshold, Total) ->
    folsom_metrics:delete_metric(Metric),
    elarm:clear(function_error,
                <<(a2b(M))/binary, $:, (a2b(F))/binary, $/, (i2b(A))/binary>>),
    run_list(R, Threshold, Total);

run_list([{{_ID, {M, F, A}}, [{count, _Cnt}, {one, One}]} | R],
         Threshold, Total) ->
    elarm:clear(function_error,
                <<(a2b(M))/binary, $:, (a2b(F))/binary, $/, (i2b(A))/binary>>),
    run_list(R, Threshold, Total + One);

run_list([{_, [{count, _Cnt}, {one, One}]} | R], Threshold, Total) ->
    run_list(R, Threshold, Total + One);

run_list([], _, Total) ->
    Total.

a2b(A) ->
    list_to_binary(atom_to_list(A)).

i2b(I) ->
    integer_to_binary(I).

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
