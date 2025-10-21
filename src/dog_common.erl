-module(dog_common).

-include("dog_trainer.hrl").

-export([
    concat/2,
    create_hash/1,
    eel_test/0,
    eq_join/4,
    format_value/1,
    format_var/1,
    format_vars/1,
    inverse_map_of_lists/1,
    list_of_maps_to_map/2,
    lmm/2,
    merge_lists_in_tuples/1,
    merge_maps_of_lists/1,
    quoted_comma_delimited/1,
    re_filter/2,
    rekey_map_of_maps/3,
    rkmm/3,
    to_binary/1,
    to_list/1,
    to_terraform_name/1,
    tuple_pairs_to_map_of_lists/1
]).

-spec to_list(Item :: iolist() | atom() | tuple() | map() | binary() | integer() | float()) ->
    list().
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

-spec to_binary(Item :: iolist() | atom() | tuple() | map() | binary() | integer() | float()) ->
    binary().
to_binary(Item) when is_atom(Item) ->
    erlang:atom_to_binary(Item, utf8);
to_binary(Item) when is_list(Item) -> % Assumes Item is a valid iolist or string
    erlang:iolist_to_binary(Item);
to_binary(Item) when is_tuple(Item) -> % String representation like "{a,b}"
    erlang:iolist_to_binary(io_lib:format("~p", [Item]));
to_binary(Item) when is_map(Item) -> % String representation like "#{a=>b}"
    erlang:iolist_to_binary(io_lib:format("~p", [Item]));
to_binary(Item) when is_binary(Item) ->
    Item;
to_binary(Item) when is_integer(Item) ->
    erlang:integer_to_binary(Item);
to_binary(Item) when is_float(Item) ->
    erlang:float_to_binary(Item).

-spec re_filter(List :: [iolist()], Re :: string()) -> [iolist()].
re_filter(List, Re) ->
    lists:filter(
        fun(String) ->
            case re:run(String, Re) of
                {match, _} -> true;
                nomatch -> false
            end
        end,
        List
    ).

merge_maps_of_lists(ListOfMapsOfLists) ->
    merge_maps_of_lists(ListOfMapsOfLists, #{}).

merge_maps_of_lists([], Acc) ->
    Acc;
merge_maps_of_lists(ListOfMapsOfLists, Acc) ->
    Map = hd(ListOfMapsOfLists),
    Rest = tl(ListOfMapsOfLists),
    Keys = maps:keys(Map),
    NewAcc = lists:map(
        fun(Key) ->
            ExistingList = maps:get(Key, Acc, []),
            NewList = maps:get(Key, Map),
            MergedList = sets:to_list(sets:from_list(lists:merge(ExistingList, NewList))),
            {Key, MergedList}
        end,
        Keys
    ),
    merge_maps_of_lists(Rest, maps:from_list(NewAcc)).

lmm(ListOfMaps, Key) ->
    list_of_maps_to_map(ListOfMaps, Key).

list_of_maps_to_map(ListOfMaps, Key) ->
    list_of_maps_to_map(ListOfMaps, Key, #{}).

list_of_maps_to_map([], _Key, MapAcc) ->
    MapAcc;
list_of_maps_to_map(ListOfMaps, Key, MapAcc) ->
    KeyValue = maps:get(Key, hd(ListOfMaps)),
    NewMap = maps:remove(Key, hd(ListOfMaps)),
    MapAcc@1 = maps:put(KeyValue, NewMap, MapAcc),
    list_of_maps_to_map(tl(ListOfMaps), Key, MapAcc@1).

%> MM = #{drew => #{test => rest, a => b}, bob => #{test => zest, a=> c}}.
%> rkmm(MM,a,name).
% #{b => #{name => drew,test => rest},
%   c => #{name => bob,test => zest}}

rekey_map_of_maps(MapOfMaps, NewKey, OldKeysNewKey) ->
    rkmm(MapOfMaps, NewKey, OldKeysNewKey).

rkmm(MapOfMaps, NewKey, OldKeysNewKey) ->
    Iterator = maps:iterator(MapOfMaps),
    Next = maps:next(Iterator),
    rekey_map_of_maps(Next, NewKey, OldKeysNewKey, #{}).

rekey_map_of_maps(none, _NewKey, _OldKeysNewKey, MapAcc) ->
    MapAcc;
rekey_map_of_maps(Iterator, NewKey, OldKeysNewKey, MapAcc) ->
    {OldKey, OldValue, ThisIterator} = Iterator,
    io:format("Iterator: ~p~n", [Iterator]),
    NewKeyValue = maps:get(NewKey, OldValue),
    NewMap@0 = maps:remove(NewKey, OldValue),
    NewMap@1 = maps:put(OldKeysNewKey, OldKey, NewMap@0),
    MapAcc@1 = maps:put(NewKeyValue, NewMap@1, MapAcc),
    NewIterator = maps:next(ThisIterator),
    rekey_map_of_maps(NewIterator, NewKey, OldKeysNewKey, MapAcc@1).

merge_lists_in_tuples(List) ->
    Map = lists:foldl(fun fun_merge_lists_in_tuples/2, maps:new(), List),
    lists:map(
        fun({K, V}) ->
            UniqueV = lists:usort(lists:flatten(V)),
            {K, UniqueV}
        end,
        maps:to_list(Map)
    ).

fun_merge_lists_in_tuples(H, A) ->
    K = element(1, H),
    case maps:is_key(K, A) of
        true ->
            V = maps:get(K, A),
            maps:put(K, [element(2, H) | V], A);
        false ->
            maps:put(K, element(2, H), A)
    end.

inverse_map_of_lists(Map) ->
    MapList = maps:to_list(Map),
    Tuplelist = lists:map(
        fun({Key, Values}) ->
            lists:map(fun(Value) -> {Value, Key} end, Values)
        end,
        MapList
    ),
    lists:flatten(Tuplelist).

tuple_pairs_to_map_of_lists(TupleList) ->
    tuple_pairs_to_map_of_lists(TupleList, #{}).

tuple_pairs_to_map_of_lists([], Accum) ->
    Accum;
tuple_pairs_to_map_of_lists(TupleList, Accum) ->
    [Head | Tail] = TupleList,
    {Key, Value} = Head,
    Accum@1 = maps_append(Key, Value, Accum),
    tuple_pairs_to_map_of_lists(Tail, Accum@1).

-spec maps_append(Key :: _, Value :: _, Map :: map()) -> map().
maps_append(Key, Value, Map) ->
    Map@1 =
        case maps:find(Key, Map) of
            error ->
                maps:put(Key, [Value], Map);
            {ok, Values} ->
                maps:put(Key, lists:append(Values, [Value]), Map)
        end,
    Map@1.

-spec create_hash(Bytes :: binary()) -> any().
create_hash(Bytes) ->
    base16:encode(crypto:hash(sha256, Bytes)).

-spec to_terraform_name(Name :: iolist()) -> iolist().
to_terraform_name(Name) ->
    NoDots = string:replace(Name, ".", "_", all),
    NoOpenParenthesis = string:replace(NoDots, "(", "_", all),
    NoCloseParenthesis = string:replace(NoOpenParenthesis, ")", "_", all),
    NoForwardSlash = string:replace(NoCloseParenthesis, "/", "_", all),
    NoSpaces = string:replace(NoForwardSlash, " ", "_", all),
    NoColons = string:replace(NoSpaces, ":", "_", all),
    NoColons.

eel_test() ->
    Bindings = #{
        'Title' => <<"title">>,
        'List' => ["one", "two", "three"]
    },
    {ok, Snapshot} = eel:compile(<<
        "<h1><%= Title .%></h1>"
        "<ul>"
        "    <%= lists:map(fun(Item) -> %>"
        "        <li><%= Item .%></li>"
        "    <% end, List) .%>"
        "</ul>"
        "<%= Length = erlang:length(List), %>"
        "    <div>Item count: <%= Length .%></div>"
        "    <%= case Length > 0 of true -> %>"
        "        <ul>"
        "            <%= lists:map(fun(N) -> %>"
        "                <li><%= N .%></li>"
        "            <% end, lists:seq(1, Length)) .%>"
        "        </ul>"
        "    <% ; false -> <<>> end .%>"
        "<%  .%>"
    >>),
    {ok, RenderSnapshot} = eel_renderer:render(Bindings, Snapshot),
    {IoData, _} = {eel_evaluator:eval(RenderSnapshot), RenderSnapshot},
    io:format("~s", [erlang:iolist_to_binary(IoData)]).

format_vars(Vars) ->
    VarsList =
        case Vars of
            [] ->
                [];
            _ ->
                lists:map(
                    fun(Var) ->
                        format_var(Var)
                    end,
                    maps:to_list(Vars)
                )
        end,
    maps:from_list(VarsList).

format_var({Key, Value}) ->
    {Key, format_value(Value)}.

format_value(Value) ->
    case is_list(Value) of
        false ->
            string:replace(string:replace(io_lib:format("~p", [Value]), "<<", ""), ">>", "");
        true ->
            ListValues = lists:map(
                fun(L) ->
                    string:replace(string:replace(io_lib:format("~p", [L]), "<<", ""), ">>", "")
                end,
                Value
            ),
            "[" ++ lists:join(",", ListValues) ++ "]"
    end.

-spec quoted_comma_delimited(List :: list()) -> iolist().
quoted_comma_delimited(List) ->
    string:join([io_lib:format("\"~s\"", [X]) || X <- List], ",").

-spec eq_join(Table1Name :: string(), Table2Name :: string(), Key1 :: string(), Key2 :: string()) ->
    JoinTable :: list().
eq_join(Table1Name, Table2Name, Key1, Key2) ->
    {ok, OneR} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, Table1Name)
        end
    ),
    {ok, OneR2} = rethink_cursor:all(OneR),
    OneResult = lists:flatten(OneR2),
    {ok, TwoR} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, Table2Name)
        end
    ),
    {ok, TwoR2} = rethink_cursor:all(TwoR),
    TwoResult = lists:flatten(TwoR2),
    TwoMap = dog_common:list_of_maps_to_map(TwoResult, Key1),
    JoinedResult = lists:map(
        fun(One) ->
            TwoName = maps:get(Key2, One),
            Two = maps:get(TwoName, TwoMap, #{}),
            maps:merge(Two, One)
        end,
        OneResult
    ),
    JoinedResult.

%% EXTERNAL

concat(Words, string) ->
    internal_concat(Words);
concat(Words, binary) ->
    list_to_binary(internal_concat(Words)).

%% INTERNAL

internal_concat(Elements) ->
    NonBinaryElements = [case Element of _ when is_binary(Element) -> binary_to_list(Element); _ -> Element end || Element <- Elements],
    lists:concat(NonBinaryElements).
