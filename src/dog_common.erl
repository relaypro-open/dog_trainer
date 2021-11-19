-module(dog_common).

-export([
        lmm/2,
        rkmm/3,
        merge_maps_of_lists/1,
        re_filter/2,
        to_list/1
        ]).

-spec to_list(Item :: iolist() | atom() | tuple() | map() | binary() | integer() | float() ) -> list().
to_list(Item) when is_atom(Item) ->
    erlang:atom_to_list(Item);
to_list(Item) when is_list(Item) ->
    Item;
to_list(Item) when is_tuple(Item) ->
    erlang:tuple_to_list(Item);
to_list(Item) when is_map(Item) ->
    maps:to_list(Item);
to_list(Item) when is_binary(Item) ->
    erlang:binary_to_list(Item);
to_list(Item) when is_integer(Item) ->
    integer_to_list(Item);
to_list(Item) when is_float(Item) ->
    float_to_list(Item).

-spec re_filter(List :: [iolist()], Re :: string()) -> [iolist()].
re_filter(List,Re) ->
  lists:filter(fun(String) -> 
                   case re:run(String,Re) of 
                     {match,_} -> true; 
                     nomatch -> false 
                   end 
               end,List).

merge_maps_of_lists(ListOfMapsOfLists) ->
    merge_maps_of_lists(ListOfMapsOfLists,#{}).

merge_maps_of_lists([],Acc) ->
    Acc;
merge_maps_of_lists(ListOfMapsOfLists,Acc) ->
    Map = hd(ListOfMapsOfLists),
    Rest = tl(ListOfMapsOfLists),
    Keys = maps:keys(Map),
    NewAcc = lists:map(fun(Key) ->
        ExistingList = maps:get(Key,Acc,[]),
        NewList = maps:get(Key,Map),
        MergedList = sets:to_list(sets:from_list(lists:merge(ExistingList,NewList))),
        {Key,MergedList}
              end, Keys),
    merge_maps_of_lists(Rest,maps:from_list(NewAcc)).

lmm(ListOfMaps, Key) ->
    list_of_maps_to_map(ListOfMaps, Key, #{}).

list_of_maps_to_map([],_Key,MapAcc) ->
    MapAcc;
list_of_maps_to_map(ListOfMaps,Key,MapAcc) ->
    KeyValue = maps:get(Key,hd(ListOfMaps)),
    NewMap = maps:remove(Key,hd(ListOfMaps)),
    MapAcc@1 = maps:put(KeyValue,NewMap,MapAcc),
    list_of_maps_to_map(tl(ListOfMaps),Key,MapAcc@1).

%> MM = #{drew => #{test => rest, a => b}, bob => #{test => zest, a=> c}}.
%> rkmm(MM,a,name).                                           
% #{b => #{name => drew,test => rest},
%   c => #{name => bob,test => zest}}
rkmm(MapOfMaps,NewKey,OldKeysNewKey) ->
    Iterator = maps:iterator(MapOfMaps),
    Next = maps:next(Iterator),
    rekey_map_of_maps(Next,NewKey,OldKeysNewKey,#{}).

rekey_map_of_maps(none,_NewKey,_OldKeysNewKey,MapAcc) ->
    MapAcc;
rekey_map_of_maps(Iterator,NewKey,OldKeysNewKey,MapAcc) ->
    {OldKey,OldValue,ThisIterator} = Iterator,
    io:format("Iterator: ~p~n",[Iterator]),
    NewKeyValue = maps:get(NewKey,OldValue),
    NewMap@0 = maps:remove(NewKey,OldValue),
    NewMap@1 = maps:put(OldKeysNewKey,OldKey,NewMap@0),
    MapAcc@1 = maps:put(NewKeyValue,NewMap@1,MapAcc),
    NewIterator = maps:next(ThisIterator),
    rekey_map_of_maps(NewIterator,NewKey,OldKeysNewKey,MapAcc@1).
