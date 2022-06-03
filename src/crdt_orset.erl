-module(crdt_orset).

-export([new/0,
         add/3,
         remove/2,
         merge/2,
         to_list/1,
         to_binary/1,
         from_binary/1]).

-export_type([orset/0]).

-define(SET, crdt_orset_v1).

-record(?SET, {
          items = ordsets:new() :: ordsets:ordset({item(), actor(), clock()}),
          clocks = #{} :: #{actor() => clock()}
         }).

-type orset() :: #?SET{}.
-type item() :: term().
-type actor() :: term().
-type clock() :: pos_integer().


-spec new() -> orset().
new() ->
    #?SET{}.


-spec to_list(orset()) -> [item()].
to_list(#?SET{items = Items}) ->
    lists:uniq([ Item || {Item, _, _} <- ordsets:to_list(Items) ]).


-spec add(item(), actor(), orset()) -> orset().
add(Item, Actor, #?SET{items = Items0, clocks = Clocks0}) ->
    Clocks1 = maps:update_with(Actor, fun(C) -> C + 1 end, 1, Clocks0),
    Clock = maps:get(Actor, Clocks1),
    Items1 = ordsets:add_element({Item, Actor, Clock}, Items0),
    Items2 = ordsets:filter(fun({I, A, C}) when I =:= Item andalso A =:= Actor ->
                                    C >= Clock;
                               (_) ->
                                    true
                            end,
                            Items1),
    #?SET{items = Items2, clocks = Clocks1}.


-spec remove(item(), orset()) -> orset().
remove(Item, #?SET{items = Items0} = Set) ->
    Items1 = ordsets:filter(fun({I, _, _}) -> I =/= Item end, Items0),
    Set#?SET{items = Items1}.


-spec merge(orset(), orset()) -> orset().
merge(#?SET{items = ItemsA, clocks = ClocksA}, #?SET{items = ItemsB, clocks = ClocksB}) ->
    CommonItems = ordsets:intersection(ItemsA, ItemsB),
    OnlyAItems = ordsets:filter(fun({_, A, C}) -> C > maps:get(A, ClocksB, 0) end, ordsets:subtract(ItemsA, ItemsB)),
    OnlyBItems = ordsets:filter(fun({_, A, C}) -> C > maps:get(A, ClocksA, 0) end, ordsets:subtract(ItemsB, ItemsA)),
    MergedItems0 = ordsets:union([CommonItems, OnlyAItems, OnlyBItems]),

    ItemAndActorToClocks = maps:groups_from_list(fun({I, A, _}) -> {I, A} end,
                                                 fun({_, _, C}) -> C end,
                                                 ordsets:to_list(MergedItems0)),
    ItemAndActorToClock = maps:map(fun(_, V) -> lists:max(V) end, ItemAndActorToClocks),
    MergedItems1 = ordsets:filter(fun({I, A, C}) ->
                                          MaxClock = maps:get({I, A}, ItemAndActorToClock),
                                          C =:= MaxClock
                                  end,
                                  MergedItems0),

    MergedClocks = maps:merge_with(fun(_, C0, C1) -> max(C0, C1) end, ClocksA, ClocksB),

    #?SET{items = MergedItems1, clocks = MergedClocks}.


-spec to_binary(orset()) -> binary().
to_binary(#?SET{} = Set) ->
    term_to_binary(Set).


-spec from_binary(binary()) -> {ok, orset()} | {error, invalid_binary}.
from_binary(<<Bin/binary>>) ->
    try
        binary_to_term(Bin)
    of
        #?SET{} = Set ->
            {ok, Set};
        _ ->
            {error, invalid_binary}
    catch
        error:badarg ->
            {error, invalid_binary}
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


single_actor_test() ->
    A0 = crdt_orset:new(),
    ?assertEqual([], crdt_orset:to_list(A0)),

    A1 = crdt_orset:add(foo, a, A0),
    ?assertEqual([foo], crdt_orset:to_list(A1)),

    A2 = crdt_orset:add(bar, a, A1),
    ?assertEqual([bar, foo], crdt_orset:to_list(A2)),

    A3 = crdt_orset:remove(foo, A2),
    ?assertEqual([bar], crdt_orset:to_list(A3)),

    ok.


two_actors_test() ->
    %% add.
    A0 = crdt_orset:new(),
    A1 = crdt_orset:add(foo, a, A0),
    A2 = crdt_orset:add(bar, a, A1),

    %% add.
    B0 = crdt_orset:new(),
    B1 = crdt_orset:add(bar, b, B0),
    B2 = crdt_orset:add(baz, b, B1),

    %% merge.
    A3 = crdt_orset:merge(A2, B2),
    B3 = crdt_orset:merge(B2, A2),
    ?assertEqual([bar, baz, foo], crdt_orset:to_list(A3)),
    ?assertEqual([bar, baz, foo], crdt_orset:to_list(B3)),

    %% remove and add (different items).
    A4 = crdt_orset:remove(baz, A3),
    B4 = crdt_orset:add(qux, b, B3),

    A5 = crdt_orset:merge(A4, B4),
    B5 = crdt_orset:merge(B4, A4),
    ?assertEqual([bar, foo, qux], crdt_orset:to_list(A5)),
    ?assertEqual([bar, foo, qux], crdt_orset:to_list(B5)),

    %% remove and add (the same items).
    A6 = crdt_orset:remove(foo, A5),
    B6 = crdt_orset:add(foo, b, B5),

    A7 = crdt_orset:merge(A6, B6),
    B7 = crdt_orset:merge(B6, A6),
    ?assertEqual([bar, foo, qux], crdt_orset:to_list(A7)),
    ?assertEqual([bar, foo, qux], crdt_orset:to_list(B7)),

    ok.


to_from_binary_test() ->
    A0 = crdt_orset:new(),
    A1 = crdt_orset:add(foo, a, A0),
    A2 = crdt_orset:add(bar, a, A1),
    A3 = crdt_orset:remove(foo, A2),

    Bin = crdt_orset:to_binary(A3),
    ?assertMatch({ok, A3}, crdt_orset:from_binary(Bin)),

    ok.

-endif.
