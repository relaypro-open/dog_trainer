-module(dog_inventory).

-include_lib("kernel/include/logger.hrl").

-define(VALIDATION_TYPE, <<"inventory">>).
-define(TYPE_TABLE, inventory).

%API
-export([
    create/1,
    delete/1,
    dump_all/0,
    get_by_id/1,
    get_by_name/1,
    get_all/0,
    get_schema/0,
    update/2
]).

-export([
    init/0
]).

-spec init() -> any().
init() ->
    pass.

-spec get_by_name(Name :: binary()) -> {ok, map()} | {error, atom()}.
get_by_name(Name) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get_all(X, Name, #{index => <<"name">>})
        end
    ),
    {ok, R3} = rethink_cursor:all(R),
    Result = lists:flatten(R3),
    case Result of
        [] ->
            ?LOG_ERROR("error, inventory name not found: ~p", [Name]),
            {error, notfound};
        _ ->
            Inventory = hd(Result),
            {ok, Inventory}
    end.

-spec get_by_id(Id :: binary()) -> {ok, map()} | {error, atom()}.
get_by_id(Id) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, Id)
        end
    ),
    case R of
        null ->
            {error, notfound};
        _ ->
            Inventory = R,
            {ok, Inventory}
    end.

-spec delete(Id :: binary()) -> ok | {error, Error :: map()}.
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

-spec update(Id :: binary(), UpdateMap :: map()) -> {atom(), any()}.
update(Id, UpdateMap) ->
    case get_by_id(Id) of
        {ok, OldInventory} ->
            NewInventory = maps:merge(OldInventory, UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewInventory) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:get(X, Id),
                            reql:update(X, UpdateMap)
                        end
                    ),
                    ?LOG_DEBUG("update R: ~p~n", [R]),
                    Replaced = maps:get(<<"replaced">>, R),
                    Unchanged = maps:get(<<"unchanged">>, R),
                    case {Replaced, Unchanged} of
                        {1, 0} -> {true, Id};
                        {0, 1} -> {false, Id};
                        _ -> {false, no_update}
                    end;
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {error, Error} ->
            {false, Error}
    end.

-spec create(Group :: map()) -> {ok | error, Key :: iolist() | name_exists}.
create(InventoryMap@0) ->
    Name = maps:get(<<"name">>, InventoryMap@0),
    {ok, ExistingInventorys} = get_all(),
    ExistingNames = [maps:get(<<"name">>, Inventory) || Inventory <- ExistingInventorys],
    case lists:member(Name, ExistingNames) of
        false ->
            case dog_json_schema:validate(?VALIDATION_TYPE, InventoryMap@0) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:insert(X, InventoryMap@0)
                        end
                    ),
                    Key = hd(maps:get(<<"generated_keys">>, R)),
                    ?LOG_DEBUG("create R: ~p~n", [R]),
                    {ok, Key};
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        true ->
            {error, name_exists}
    end.

-spec get_all() -> {ok, list()}.
get_all() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:pluck(X, [<<"name">>, <<"id">>])
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Inventorys =
        case lists:flatten(Result) of
            [] ->
                [];
            Else ->
                Else
        end,
    {ok, Inventorys}.

-spec dump_all() -> {ok, list()}.
dump_all() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, inventory)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Inventorys =
        case lists:flatten(Result) of
            [] ->
                [];
            Else ->
                Else
        end,
    {ok, Inventorys}.

-spec get_schema() -> binary().
get_schema() ->
    dog_json_schema:get_file(?VALIDATION_TYPE).
