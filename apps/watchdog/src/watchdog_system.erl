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
-export([start_link/4, notify/3]).

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

-record(state, {type, cluster, system, node, version, id, tbl, alarms = sets:new()}).

%%%===================================================================
%%% API
%%%===================================================================

notify({C, S, N}, _Vsn, {raise, Type, Alert, Severity}) ->
    NPid = case gproc:where({n, l, {node, N}}) of
               undefined ->
                   {ok, NPidX} = start(node, C, S, N),
                   NPidX;
               NPidX ->
                   NPidX
           end,
    gen_server:cast(NPid, {raise, Type, Alert, Severity});

notify({C, S, N}, _Vsn, {clear, Type, Alert}) ->
    NPid = case gproc:where({n, l, {node, N}}) of
               undefined ->
                   {ok, NPidX} = start(node, C, S, N),
                   NPidX;
               NPidX ->
                   NPidX
           end,
    gen_server:cast(NPid, {clear, Type, Alert});

notify({C, S, N}, Vsn, Msg) ->
    CPid = case gproc:where({n, l, {cluster, C}}) of
               undefined ->
                   {ok,CPidX} = start(cluster, C, S, N),
                   CPidX;
               CPidX ->
                   CPidX
           end,
    gen_server:cast(CPid, {notify, Vsn, Msg}),
    SPid = case gproc:where({n, l, {system, S}}) of
               undefined ->
                   {ok, SPidX} = start(system, C, S, N),
                   SPidX;
               SPidX ->
                   SPidX
    end,
    gen_server:cast(SPid, {notify, Vsn, Msg}),
    NPid = case gproc:where({n, l, {node, N}}) of
               undefined ->
                   {ok, NPidX} = start(node, C, S, N),
                   NPidX;
               NPidX ->
                   NPidX
           end,
    gen_server:cast(NPid, {notify, Vsn, Msg}).

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
    lager:info("[watchdog] Starting cluster ~s/~s/~s", [C, S, N]),
    erlang:send_after(?TICK, self(), tick),
    gproc:reg({n, l, {cluster, C}}, cluster),
    {ok, #state{type = cluster, cluster = C, system = S, node = N, id = C}};
init([system, C, S, N]) ->
    lager:info("[watchdog] Starting system ~s/~s/~s", [C, S, N]),
    erlang:send_after(?TICK, self(), tick),
    gproc:reg({n, l, {system, S}}, system),
    {ok, #state{type = system, cluster = C, system = S, node = N, id = {C, S}}};
init([node, C, S, N]) ->
    lager:info("[watchdog] Starting node ~s/~s/~s", [C, S, N]),
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
handle_cast({raise, Type, Alert, Severity}, State) ->
    State1 = raise(Type, Alert, Severity, State),
    {noreply, State1};
handle_cast({clear, Type, Alert}, State) ->
    State1 = clear(Type, Alert, State),
    {noreply, State1};

handle_cast({notify, _Vsn, {_, {flf, <<"gen_server.erl">>, _L, _F}}}, State) ->
    {noreply, State};

handle_cast({notify, Vsn, {{lager, Lvl}, {flf, File, Line, _Function}}},
            State = #state{id = ID}) ->
    Name = {ID, {File, Line}},
    report(ID, Vsn, File, Line, lager, level_to_int(Lvl)),
    case folsom_metrics:new_spiral(Name) of
        ok ->
            folsom_metrics:tag_metric(Name, {ID, Lvl});
        _ ->
            ok
    end,
    folsom_metrics:notify({Name, 1}),
    {noreply, State};

handle_cast({notify, Vsn, {Error, {mfaf, {_M, _F, _A, {File, Line}}}}},
            State = #state{id = ID}) ->
    Name = {ID, {File, Line}},
    report(ID, Vsn, File, Line, Error, 4),
    case folsom_metrics:new_spiral(Name) of
        ok ->
            folsom_metrics:tag_metric(Name, {ID, crash});
        _ ->
            ok
    end,
    folsom_metrics:notify({Name, 1}),
    {noreply, State};

handle_cast({notify, _Vsn, {_Error, {mfa, {<<"gen_server">>, _ , _}}}}, State) ->
    {noreply, State};

handle_cast({notify, Vsn, {Error, {mfa, MFA}}},
            State = #state{id = ID}) ->
    Name = {ID, MFA},
    report(ID, Vsn, MFA, Error, 4),
    case folsom_metrics:new_spiral(Name) of
        ok ->
            folsom_metrics:tag_metric(Name, {ID, crash});
        _ ->
            ok
    end,
    folsom_metrics:notify({Name, 1}),
    {noreply, State};

handle_cast({notify, _, Msg}, State) ->
    lager:info("[system] Unknown message: ~p~n", [Msg]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

t2b(Term) ->
    list_to_binary(io_lib:format("~p", [Term])).

report({C, S, N}, Vsn, File, Line, Error, Level) ->
    watchdog_upstream:file(C, S, N, Vsn, File, Line, t2b(Error), Level);

report(_, _, _, _, _, _) ->
    ok.

report({C, S, N}, Vsn, {M, F, A}, Error, Level) ->
    watchdog_upstream:mfa(C, S, N, Vsn, M, F, A, t2b(Error), Level);
report(_, _, _, _, _) ->
    ok.

level_to_int(debug) ->
    0;
level_to_int(info) ->
    1;
level_to_int(warning) ->
    2;
level_to_int(error) ->
    3;
level_to_int(crash) ->
    4;
level_to_int(_) ->
    0.

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
    EID = id2s(ID),
    {N1, S1} = run(ID, info,  ?INFO_THRESHOLD, State, 0),
    {N2, S2} = run(ID, warning, ?WARN_THRESHOLD, S1, N1),
    {N3, S3} = run(ID, error, ?ERROR_THRESHOLD, S2, N2),
    S5 = case run(ID, crash, ?CRASH_THRESHOLD, S3, N3) of
             {E, S4} when E > 0 ->
                 raise(EType, EID, E, S4);
             {0, S4} ->
                 clear(EType, EID, S4);
             {_, S4} ->
                 S4
         end,
    erlang:send_after(?TICK, self(), tick),
    {noreply, S5};

handle_info(_Info, State) ->
    {noreply, State}.

run(_ID, _Lvl, _, undefined, S1) ->
    S1;

run(ID, Lvl, Threshold, S1, N) ->
    case run_list(folsom_metrics:get_metrics_value({ID, Lvl}), Threshold, 0, S1) of
        {E, S2} when E >= Threshold ->
            {N + E, S2};
        {_, S2} ->
            {N, S2}
    end.

run_list([{{_ID, {File, Line}}, [{count, _Cnt},{one, One}]} | R],
         Threshold, Total, S1)
  when One >= Threshold ->
    S2 = raise(file_error, <<File/binary, ":", (i2b(Line))/binary>>, One, S1),
    run_list(R, Threshold, Total + One, S2);

run_list([{{_ID, {File, Line}} = Metric, [{count, _Cnt}, {one, 0}]} | R],
         Threshold, Total, S1) ->
    folsom_metrics:delete_metric(Metric),
    S2 = clear(file_error, <<File/binary, ":", (i2b(Line))/binary>>, S1),
    run_list(R, Threshold, Total, S2);

run_list([{{_ID, {_File, _Line}}, [{count, _Cnt}, {one, One}]} | R],
         Threshold, Total, S1) ->
    run_list(R, Threshold, Total + One, S1);

run_list([{{_ID, {M, F, A}}, [{count, _Cnt}, {one, One}]} | R],
         Threshold, Total, S1)
  when One >= Threshold ->
    Error = <<M/binary, $:, F/binary, $/, (i2b(A))/binary>>,
    S2 = raise(function_error, Error, One, S1),
    run_list(R, Threshold, Total + One, S2);

run_list([{{_ID, {M, F, A}} = Metric, [{count, _Cnt}, {one, 0}]} | R],
         Threshold, Total, S1) ->
    folsom_metrics:delete_metric(Metric),
    Error = <<M/binary, $:, F/binary, $/, (i2b(A))/binary>>,
    S2 = clear(function_error, Error, S1),
    run_list(R, Threshold, Total, S2);

run_list([{{_ID, {_M, _F, _A}}, [{count, _Cnt}, {one, One}]} | R],
         Threshold, Total, S1) ->
    run_list(R, Threshold, Total + One, S1);

run_list([{_, [{count, _Cnt}, {one, One}]} | R], Threshold, Total, S1) ->
    run_list(R, Threshold, Total + One, S1);

run_list([], _, Total, S1) ->
    {Total, S1}.


raise(Type, Alert, Lvl, State = #state{alarms = Alarms}) ->
    E = {Type, Alert},
    case sets:is_element(E, Alarms) of
        true ->
            State;
        false ->
            {C, S, N} = case State#state.id of
                            {C1, S1, N1} -> {C1, S1, N1};
                            {C1, S1} -> {C1, S1, <<>>};
                            C1 -> {C1, <<>>, <<>>}
                        end,
            watchdog_upstream:raise(C, S, N, a2b(Type), Alert, Lvl),
            elarm:raise(Type, Alert, [{level, Lvl}]),
            State#state{alarms = sets:add_element(E, Alarms)}
    end.

clear(Type, Alert, State = #state{alarms = Alarms}) ->
    E = {Type, Alert},
    case sets:is_element(E, Alarms) of
        false ->
            State;
        true ->
            {C, S, N} = case State#state.id of
                            {C1, S1, N1} -> {C1, S1, N1};
                            {C1, S1} -> {C1, S1, <<>>};
                            C1 -> {C1, <<>>, <<>>}
                        end,
            watchdog_upstream:clear(C, S, N, a2b(Type), Alert),
            elarm:clear(Type, Alert),
            State#state{alarms = sets:del_element(E, Alarms)}
    end.


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

id2s({Cluster, System, Node}) ->
    <<Cluster/binary, $/, System/binary, $/, Node/binary>>;
id2s({Cluster, System}) ->
    <<Cluster/binary, $/, System/binary>>;
id2s(Cluster) ->
    Cluster.
