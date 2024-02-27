-module(dog_link).

-include("dog_trainer.hrl").

-define(VALIDATION_TYPE, <<"link">>).
-define(TYPE_TABLE, link).

%API
-export([
    create/1,
    delete/1,
    delete_related_external/1,
    dump_all/0,
    get_by_id/1,
    get_by_name/1,
    get_all/0,
    get_schema/0,
    update/2
]).

-export([
    get/1,
    get_id_by_name/1,
    get_name_by_id/1,
    get_all_active/0,
    get_all_active_outbound/0,
    init/0,
    is_enabled/1
]).

-spec init() -> any().
init() ->
    pass.

-spec get(Name :: binary()) -> [map()].
get(Name) ->
    {ok, LinkDefinition} = get_by_name(Name),
    ?LOG_DEBUG("LinkDefinition: ~p", [LinkDefinition]),
    Link = maps:get(<<"links">>, LinkDefinition),
    Link.

-spec get_name_by_id(Id :: binary()) -> Name :: binary() | {error, Error :: atom()}.
get_name_by_id(Id) ->
    ?LOG_DEBUG("Id: ~p", [Id]),
    case get_by_id(Id) of
        {ok, LinkDefinition} ->
            ?LOG_DEBUG("LinkDefinition: ~p", [LinkDefinition]),
            Name = maps:get(<<"name">>, LinkDefinition),
            Name;
        {error, Error} ->
            ?LOG_ERROR("error, link id not found: ~p, ~p", [Id, Error]),
            {error, Error}
    end.

-spec get_id_by_name(Name :: binary()) -> [iolist()].
get_id_by_name(Name) ->
    {ok, LinkDefinition} = get_by_name(Name),
    ?LOG_DEBUG("LinkDefinition: ~p", [LinkDefinition]),
    Id = maps:get(<<"id">>, LinkDefinition),
    Id.

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
            ?LOG_ERROR("error, link name not found: ~p", [Name]),
            {error, notfound};
        _ ->
            Link = hd(Result),
            {ok, Link}
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
            Link = R,
            {ok, Link}
    end.

-spec is_enabled(Id :: binary()) -> boolean().
is_enabled(Id) ->
    {ok, Link} = get_by_id(Id),
    maps:get(<<"enabled">>, Link).

-spec delete(Id :: binary()) -> ok | {error, Error :: map()}.
delete(Id) ->
    case is_enabled(Id) of
        true ->
            ?LOG_INFO("link ~p not deleted, is enabled~n", [Id]),
            {error, #{<<"errors">> => #{<<"unable to delete">> => <<"link enabled">>}}};
        false ->
            delete_related_external(Id),
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
            end
    end.

-spec delete_related_external(Id :: binary()) -> (ok | error).
delete_related_external(Id) ->
    ?LOG_DEBUG("Id: ~p", [Id]),
    {ok, Link} = get_by_id(Id),
    LinkName = maps:get(<<"name">>, Link),
    dog_external:delete(LinkName).

-spec update(Id :: binary(), UpdateMap :: map()) -> {atom(), any()}.
update(Id, UpdateMap) ->
    case get_by_id(Id) of
        {ok, OldLink} ->
            NewLink = maps:merge(OldLink, UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewLink) of
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
create(LinkMap@0) ->
    Name = maps:get(<<"name">>, LinkMap@0),
    {ok, ExistingLinks} = get_all(),
    ExistingNames = [maps:get(<<"name">>, Link) || Link <- ExistingLinks],
    case lists:member(Name, ExistingNames) of
        false ->
            case dog_json_schema:validate(?VALIDATION_TYPE, LinkMap@0) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:insert(X, LinkMap@0)
                        end
                    ),
                    Key = hd(maps:get(<<"generated_keys">>, R)),
                    ?LOG_DEBUG("create R: ~p~n", [R]),
                    create_empty_external(Name),
                    {ok, Key};
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        true ->
            {error, name_exists}
    end.

create_empty_external(EnvName) ->
    ExternalMap = dog_external:empty_external(EnvName),
    dog_external:create(ExternalMap).

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
    Links =
        case lists:flatten(Result) of
            [] ->
                [];
            Else ->
                Else
        end,
    {ok, Links}.

-spec dump_all() -> {ok, list()}.
dump_all() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, link)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Links =
        case lists:flatten(Result) of
            [] ->
                [];
            Else ->
                Else
        end,
    {ok, Links}.

-spec get_all_active() -> {ok, list()} | error.
get_all_active() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:filter(X, #{<<"enabled">> => true})
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    ?LOG_DEBUG("Result: ~p", [Result]),
    Links = hd(hd((Result))),
    ?LOG_DEBUG("Links: ~p", [Links]),
    case Links of
        [] ->
            {ok, []};
        _ ->
            {ok, Links}
    end.

-spec get_all_active_outbound() -> {ok, list()} | error.
get_all_active_outbound() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:filter(X, #{<<"enabled">> => true})
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    ?LOG_DEBUG("Result: ~p", [Result]),
    Links = hd(hd((Result))),
    ?LOG_DEBUG("Links: ~p", [Links]),
    case Links of
        {ok, []} ->
            {ok, []};
        Else ->
            OutboundLinks = [
                Link
             || Link <- Else,
                (maps:get(<<"direction">>, Link) == <<"outbound">>) or
                    (maps:get(<<"direction">>, Link) == <<"bidirectional">>)
            ],
            {ok, OutboundLinks}
    end.

-spec get_schema() -> binary().
get_schema() ->
    dog_json_schema:get_file(?VALIDATION_TYPE).
