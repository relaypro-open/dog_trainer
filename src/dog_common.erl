-module(dog_common).

-export([
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
