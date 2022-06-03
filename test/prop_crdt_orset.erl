-module(prop_crdt_orset).
-include_lib("proper/include/proper.hrl").

%% Model Callbacks
-export([command/1,
         initial_state/0,
         next_state/3,
         precondition/2,
         postcondition/3]).

%% TODO: comment
-export([add/2,
         remove/2,
         merge/2]).


%%%%%%%%%%%%%%%%%%
%%% PROPERTIES %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Cmds,
            commands(?MODULE),
            begin
                actual_system_start(),
                {History, State, Result} = run_commands(?MODULE, Cmds),
                actual_system_stop(),
                ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n", [History, State, Result]),
                          aggregate(command_names(Cmds), Result =:= ok))
            end).


%%%%%%%%%%%%%
%%% MODEL %%%
%%%%%%%%%%%%%
%% @doc Initial model value at system start. Should be deterministic.
initial_state() ->
    #{
      node_a => riak_dt_orset:new(),
      node_b => riak_dt_orset:new(),
      node_c => riak_dt_orset:new()
     }.


%% @doc List of possible commands to run against the system
command(_State) ->
    TargetNode = oneof([node_a, node_b, node_c]),
    Items = oneof(lists:seq(1, 10)),
    oneof([{call, ?MODULE, add, [TargetNode, Items]},
           {call, ?MODULE, remove, [TargetNode, Items]},
           {call, ?MODULE, merge, [TargetNode, TargetNode]}]).


%% @doc Determines whether a command should be valid under the
%% current state.
precondition(_State, {call, _Mod, _Fun, _Args}) ->
    true.


%% @doc Given the state `State' *prior* to the call
%% `{call, Mod, Fun, Args}', determine whether the result
%% `Res' (coming from the actual system) makes sense.
postcondition(State, {call, _Mod, _Fun, [Node | _]} = Call, Res) ->
    State1 = next_state(State, Res, Call),
    riak_dt_orset:value(maps:get(Node, State1)) =:= Res.

%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state(State, _Res, {call, ?MODULE, add, [Node, Item]}) ->
    {ok, Set} = riak_dt_orset:update({add, Item}, Node, maps:get(Node, State)),
    State#{Node => Set};
next_state(State, _Res, {call, ?MODULE, remove, [Node, Item]}) ->
    case riak_dt_orset:update({remove, Item}, Node, maps:get(Node, State)) of
        {ok, Set} ->
            State#{Node => Set};
        {error, {precondition, {not_present, _}}} ->
            State
    end;
next_state(State, _Res, {call, ?MODULE, merge, [Node0, Node1]}) ->
    Set = riak_dt_orset:merge(maps:get(Node0, State), maps:get(Node1, State)),
    State#{Node0 => Set, Node1 => Set}.


%%%%%%%%%%%%%%%%%%%%%
%%% Actual System %%%
%%%%%%%%%%%%%%%%%%%%%

actual_system_start() ->
    ets:new(?MODULE, [named_table]).


add(Node, Item) ->
    Set = crdt_orset:add(Item, Node, get_set(Node)),
    true = ets:insert(?MODULE, {Node, Set}),
    crdt_orset:to_list(Set).


remove(Node, Item) ->
    Set = crdt_orset:remove(Item, get_set(Node)),
    true = ets:insert(?MODULE, {Node, Set}),
    crdt_orset:to_list(Set).


merge(Node0, Node1) ->
    Set0 = get_set(Node0),
    Set1 = get_set(Node1),
    Set = crdt_orset:merge(Set0, Set1),
    true = ets:insert(?MODULE, {Node0, Set}),
    true = ets:insert(?MODULE, {Node1, Set}),
    crdt_orset:to_list(Set).


get_set(Node) ->
    case ets:lookup(?MODULE, Node) of
        [] ->
            crdt_orset:new();
        [{_, Set}] ->
            Set
    end.


actual_system_stop() ->
    ets:delete(?MODULE).
