-module(dog_service).

-include("dog_trainer.hrl").
-include_lib("kernel/include/logger.hrl").

-define(VALIDATION_TYPE, <<"service">>).
-define(TYPE_TABLE, service).

%API
-export([
    create/1,
    delete/1,
    get_by_id/1,
    get_by_name/1,
    get_all/0,
    get_schema/0,
    update/2
]).

-export([
    get/1,
    get_all_grouped_by_id/0,
    get_all_grouped_by_name/0,
    get_all_in_rule/1,
    get_id_by_name/1,
    get_name_by_id/1,
    in_active_profile/1,
    in_profile/1,
    init/0,
    where_used/1,
    any_service/0
]).

-spec init() -> any().
init() ->
    pass.

-spec get(Name :: binary()) -> [map()].
get(Name) ->
    {ok, ServiceDefinition} = get_by_name(Name),
    ?LOG_DEBUG("ServiceDefinition: ~p", [ServiceDefinition]),
    S = maps:get(<<"services">>, ServiceDefinition),
    Services = lists:map(fun(Service) -> parse_service(Service) end, S),
    Services.

-spec get_name_by_id(Id :: binary()) -> [iolist()].
get_name_by_id(<<"any">>) ->
    <<"ANY">>;
get_name_by_id(Id) ->
    ?LOG_DEBUG("Id: ~p", [Id]),
    case get_by_id(Id) of
        {ok, ServiceDefinition} ->
            ?LOG_DEBUG("ServiceDefinition: ~p", [ServiceDefinition]),
            Name = maps:get(<<"name">>, ServiceDefinition),
            Name;
        {error, Error} ->
            ?LOG_ERROR("error, service id not found: ~p, ~p", [Id, Error]),
            {error, Error}
    end.

-spec get_id_by_name(Name :: binary()) -> [iolist()].
get_id_by_name(Name) ->
    {ok, ServiceDefinition} = get_by_name(Name),
    ?LOG_DEBUG("ServiceDefinition: ~p", [ServiceDefinition]),
    Id = maps:get(<<"id">>, ServiceDefinition),
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
            ?LOG_ERROR("error, service name not found: ~p", [Name]),
            {error, notfound};
        _ ->
            {ok, hd(Result)}
    end.

-spec get_by_id(Id :: binary()) -> {ok, map()} | {error, atom()}.
get_by_id(Id) ->
    case Id of
        <<"any">> ->
            {ok, any_service()};
        _ ->
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
                    {ok, R}
            end
    end.

-spec parse_service(map()) -> map().
parse_service(Json) ->
    Protocol = parse_protocol(maps:get(<<"protocol">>, Json)),
    Ports = parse_ports(maps:get(<<"ports">>, Json)),
    M = maps:new(),
    M1 = maps:put(<<"protocol">>, Protocol, M),
    M2 = maps:put(<<"ports">>, Ports, M1),
    M2.

-spec parse_protocol(Protocol :: binary()) -> 'error' | binary().
parse_protocol(Protocol) ->
    case Protocol of
        <<"tcp">> -> "tcp";
        <<"udp">> -> "udp";
        <<"icmp">> -> "icmp";
        _ -> error
    end.

-spec parse_ports([any()]) -> [[any()]].
parse_ports(Ports) ->
    M = lists:map(
        fun(Port) ->
            [
                case X of
                    $- -> $:;
                    _ -> X
                end
             || X <- binary_to_list(Port)
            ]
        end,
        Ports
    ),
    M.

-spec delete(ZoneId :: binary()) -> ok | {error, Error :: map()}.
delete(ZoneId) ->
    ?LOG_DEBUG(#{zone_id => ZoneId}),
    case in_profile(ZoneId) of
        {false, []} ->
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, ?TYPE_TABLE),
                    reql:get(X, ZoneId),
                    reql:delete(X)
                end
            ),
            ?LOG_DEBUG("delete R: ~p~n", [R]),
            Deleted = maps:get(<<"deleted">>, R),
            case Deleted of
                1 -> ok;
                _ -> {error, #{<<"error">> => <<"error">>}}
            end;
        {true, Profiles} ->
            ?LOG_INFO("service ~p not deleted, in profiles: ~p~n", [ZoneId, Profiles]),
            {error, #{<<"errors">> => #{<<"in active profile">> => Profiles}}}
    end.

-spec update(Id :: binary(), UpdateMap :: map()) -> {atom(), any()}.
update(Id, UpdateMap) ->
    case get_by_id(Id) of
        {ok, OldService} ->
            NewService = maps:merge(OldService, UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewService) of
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
create(ServiceMap@0) ->
    Name = maps:get(<<"name">>, ServiceMap@0),
    {ok, ExistingServices} = get_all(),
    ExistingNames = [maps:get(<<"name">>, Service) || Service <- ExistingServices],
    case lists:member(Name, ExistingNames) of
        false ->
            case dog_json_schema:validate(?VALIDATION_TYPE, ServiceMap@0) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:insert(X, ServiceMap@0)
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
    Services =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Services}.

-spec get_all_grouped_by_id() -> map().
get_all_grouped_by_id() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, service)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Services =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    maps:from_list([{maps:get(<<"id">>, Service), Service} || Service <- Services]).

-spec get_all_grouped_by_name() -> map().
get_all_grouped_by_name() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, service)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Services =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    maps:from_list([{maps:get(<<"name">>, Service), Service} || Service <- Services]).

-spec where_used_inbound(ServiceId :: binary()) -> {ok, ProfileIds :: list()}.
where_used_inbound(ServiceId) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ruleset),
            reql:filter(X, fun(Rule) ->
                reql:get_field(Rule, <<"rules">>),
                reql:get_field(Rule, <<"inbound">>),
                reql:get_field(Rule, <<"group">>),
                reql:contains(Rule, ServiceId)
            end),
            reql:get_field(X, <<"id">>)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    RuleIds =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    ProfileIds = [element(2, dog_ruleset:where_used(RulesId)) || RulesId <- RuleIds],
    ?LOG_INFO("ProfileIds: ~p~n", [R]),

    {ok, ProfileIds}.

%TODO: differentiate between ROLE(Group) and SERVICE(Service) groups.
-spec where_used_outbound(ServiceId :: binary()) -> {ok, RuleIds :: list()}.
where_used_outbound(ServiceId) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ruleset),
            reql:filter(X, fun(Rule) ->
                reql:get_field(Rule, <<"rules">>),
                reql:get_field(Rule, <<"outbound">>),
                reql:get_field(Rule, <<"group">>),
                reql:contains(Rule, ServiceId)
            end),
            reql:get_field(X, <<"id">>)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    RuleIds =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    ?LOG_INFO("ProfileIds: ~p~n", [R]),
    ProfileIds = [element(2, dog_ruleset:where_used(RulesId)) || RulesId <- RuleIds],
    {ok, ProfileIds}.

where_used(ServiceId) ->
    {ok, Inbound} = where_used_inbound(ServiceId),
    {ok, Outbound} = where_used_outbound(ServiceId),
    {ok, lists:flatten(sets:to_list(sets:from_list([Inbound, Outbound])))}.

get_all_in_rule(RuleId) ->
    R = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ruleset),
            reql:get(X, RuleId),
            reql:pluck(X, [
                #{<<"rules">> => #{<<"inbound">> => <<"service">>, <<"outbound">> => <<"service">>}}
            ])
        end
    ),
    case R of
        {error, _} ->
            {error, notfound};
        {ok, Result} ->
            Inbound = maps:get(<<"inbound">>, maps:get(<<"rules">>, Result)),
            InboundList = lists:map(fun(X) -> maps:get(<<"service">>, X) end, Inbound),
            Outbound = maps:get(<<"outbound">>, maps:get(<<"rules">>, Result)),
            OutboundList = lists:map(fun(X) -> maps:get(<<"service">>, X) end, Outbound),
            ServiceList = dog_ips:uniq(InboundList ++ OutboundList),
            ServiceList
    end.

any_service() ->
    #{
        <<"id">> => <<"any">>,
        <<"name">> => <<"any">>,
        <<"services">> => [
            #{
                <<"ports">> => [<<"0:65535">>],
                <<"protocol">> => <<"any">>
            }
        ]
    }.

-spec get_schema() -> binary().
get_schema() ->
    dog_json_schema:get_file(?VALIDATION_TYPE).

-spec in_active_profile(Id :: binary()) -> {false, []} | {true, Profiles :: list()}.
in_active_profile(Id) ->
    {ok, Used} = where_used(Id),
    {ok, Active} = dog_profile:all_active(),
    Profiles = sets:to_list(sets:intersection(sets:from_list(Used), sets:from_list(Active))),
    case Profiles of
        [] ->
            {false, []};
        _ ->
            {true, Profiles}
    end.

-spec in_profile(Id :: binary()) -> {false, []} | {true, Profiles :: list()}.
in_profile(Id) ->
    {ok, Used} = where_used(Id),
    {ok, All} = dog_profile:all(),
    Profiles = sets:to_list(sets:intersection(sets:from_list(Used), sets:from_list(All))),
    case Profiles of
        [] ->
            {false, []};
        _ ->
            {true, Profiles}
    end.
