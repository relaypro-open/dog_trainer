-module(dog_inventory_api_v2).

-include_lib("kernel/include/logger.hrl").

-define(VALIDATION_TYPE, <<"inventory">>).
-define(TYPE_TABLE, inventory).

%API
-export([
    create/1,
    delete/1,
    get_all/0,
    get_by_id/1,
    get_by_name/1,
    get_schema/0,
    update/2
]).

-spec create(Inventory :: map()) -> {ok | error, Key :: iolist() | name_exists}.
create(InventoryMap@0) ->
    Name = maps:get(<<"name">>, InventoryMap@0),
    {ok, ExistingInventorys} = get_all(),
    ExistingNames = [maps:get(<<"name">>, Inventory) || Inventory <- ExistingInventorys],
    case lists:member(Name, ExistingNames) of
        false ->
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, ?TYPE_TABLE),
                    reql:insert(X, InventoryMap@0, #{return_changes => always})
                end
            ),
            NewVal = maps:get(<<"new_val">>, hd(maps:get(<<"changes">>, R))),
            {ok, NewVal};
        true ->
            {error, name_exists}
    end.

-spec delete(InventoryId :: binary()) -> ok | {error, Error :: map()}.
delete(Id) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, Id),
            reql:delete(X)
        end
    ),
    ?LOG_DEBUG("delete R: ~p~n", [R]),
    Deleted = maps:get(<<"deleted">>, R),
    case Deleted of
        1 -> ok;
        _ -> {error, #{<<"error">> => <<"error">>}}
    end.

-spec get_all() -> {ok, list()}.
get_all() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Inventorys =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Inventorys}.

-spec get_by_id(InventoryId :: binary()) -> {ok, map()} | {ok, null} | {error, atom()}.
get_by_id(InventoryId) ->
    dog_inventory:get_by_id(InventoryId).

-spec get_by_name(binary()) -> {'ok', map()} | {'error', notfound}.
get_by_name(Name) ->
    dog_inventory:get_by_name(Name).

-spec update(InventoryId :: binary(), UpdateMap :: map()) -> {atom(), any()}.
update(Id, UpdateMap@0) ->
    ?LOG_DEBUG("UpdateMap: ~p~n", [UpdateMap@0]),
    case get_by_id(Id) of
        {ok, OldService} ->
            NewService = maps:merge(OldService, UpdateMap@0),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewService) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:get(X, Id),
                            reql:update(X, UpdateMap@0, #{return_changes => always})
                        end
                    ),
                    ?LOG_DEBUG("update R: ~p~n", [R]),
                    Replaced = maps:get(<<"replaced">>, R),
                    Unchanged = maps:get(<<"unchanged">>, R),
                    case {Replaced, Unchanged} of
                        {1, 0} ->
                            NewVal = maps:get(<<"new_val">>, hd(maps:get(<<"changes">>, R))),
                            {true, NewVal};
                        {0, 1} ->
                            OldVal = maps:get(<<"old_val">>, hd(maps:get(<<"changes">>, R))),
                            {false, OldVal};
                        _ ->
                            {false, no_updated}
                    end;
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {error, Error} ->
            {false, Error}
    end.

-spec get_schema() -> binary().
get_schema() ->
    dog_json_schema:get_file(?VALIDATION_TYPE).