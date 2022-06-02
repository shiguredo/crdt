-module(crdt_orset).

-export([new/0,
         add/3,
         remove/2,
         merge/2,
         to_list/1]).

-export_type([orset/0]).

-define(SET, ?MODULE).

-record(?SET, {
          items = gb_sets:new() :: gb_sets:set({item(), actor(), non_neg_integer()}),
          actors = #{} :: #{actor() => non_neg_integer()}
         }).

-type orset() :: #?SET{}.
-type item() :: term().
-type actor() :: term().


-spec new() -> orset().
new() ->
    #?SET{}.


-spec to_list(orset()) -> [item()].
to_list(#?SET{items = Items}) ->
    lists:uniq([ Item || {Item, _, _} <- gb_sets:to_list(Items) ]).


-spec add(item(), actor(), orset()) -> orset().
add(Item, Actor, #?SET{items = Items0, actors = Actors0} = Set) ->
    Actors1 = maps:update_with(Actor, fun(C) -> C + 1 end, 0, Actors0),
    C = maps:get(Actor, Actors1),
    Items1 = gb_sets:add({Item, Actor, C}, Items0),
    Items2 = gb_sets:filter(fun(X) ->
                                    case X of
                                        {Item, Actor, C0} when C0 < C ->
                                            false;
                                        _ ->
                                            true
                                    end
                            end,
                            Items1),
    Set#?SET{items = Items2, actors = Actors1}.


-spec remove(item(), orset()) -> orset().
remove(Item, #?SET{items = Items0} = Set) ->
    Items1 = gb_sets:filter(fun(X) ->
                                    case X of
                                        {Item, _, _} ->
                                            false;
                                        _ ->
                                            true
                                    end
                            end,
                            Items0),
    Set#?SET{items = Items1}.


-spec merge(orset(), orset()) -> orset().
merge(#?SET{items = ItemsA, actors = ActorsA}, #?SET{items = ItemsB, actors = ActorsB}) ->
    M0 = gb_sets:intersection(ItemsA, ItemsB),
    M1 = gb_sets:filter(fun({_, Actor, C}) -> C > maps:get(Actor, ActorsB, 0) end, gb_sets:difference(ItemsA, ItemsB)),
    M2 = gb_sets:filter(fun({_, Actor, C}) -> C > maps:get(Actor, ActorsA, 0) end, gb_sets:difference(ItemsB, ItemsA)),
    U = gb_sets:union([M0, M1, M2]),
    X = maps:map(fun(_, V) -> lists:max(V) end,
                 maps:groups_from_list(fun({Item, _, _}) -> Item end, fun({_, _, C}) -> C end, gb_sets:to_list(U))),
    Items = gb_sets:filter(fun(Item, _, C) -> C =:= maps:get(Item, X) end, U),
    Actors = maps:merge_with(fun(_, C0, C1) -> max(C0, C1) end, ActorsA, ActorsB),
    #?SET{items = Items, actors = Actors}.
