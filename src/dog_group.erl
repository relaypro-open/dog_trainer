-module(dog_group).

-include_lib("kernel/include/logger.hrl").

-define(VALIDATION_TYPE, <<"group">>).
-define(TYPE_TABLE, group).

%API
-export([
    create/1,
    delete/1,
    get_by_id/1,
    get_by_name/1,
    get_all/0,
    get_schema/0,
    replace/2,
    update/2
]).

-export([
    all_active/0,
    all_ipv4s_grouped/0,
    all_ipv6s_grouped/0,
    get_active_groups/0,
    get_all_grouped_by_id/0,
    get_all_grouped_by_name/0,
    get_all_inbound_ports_by_protocol/1,
    get_all_internal_ec2_security_group_ids/0,
    get_all_ips_by_id/1,
    get_all_ipv4s_by_id/1,
    get_all_ipv6s_by_id/1,
    get_ec2_security_group_ids_by_name/1,
    get_ec2_security_group_ids_from_members/1,
    get_external_ips_by_id/1,
    get_external_ipv4s_by_id/1,
    get_external_ipv6s_by_id/1,
    get_hosts_by_id/1,
    get_id_by_name/1,
    get_ids_with_profile_id/1,
    get_internal_ec2_security_group_ids_by_id/1,
    get_internal_ips_by_id/1,
    get_internal_ipv4s_by_id/1,
    get_internal_ipv6s_by_id/1,
    get_name_by_id/1,
    get_ppps_inbound_ec2/2,
    get_ppps_outbound_ec2/2,
    get_profile_by_id/1,
    get_profile_by_name/1,
    group_alert_active/1,
    in_active_profile/1,
    in_profile/1,
    init/0,
    merge/1,
    replace_profile_by_profile_id/2,
    replace_profile_by_profile_id/3,
    role_group_effects_groups/1,
    set_hash4_ipsets/2,
    set_hash4_iptables/2,
    set_hash6_ipsets/2,
    set_hash6_iptables/2,
    zone_group_effects_groups/1
]).

%Batch commands
-export([
    all_ec2_sg_mappings/0,
    set_ec2_group_mappings_from_members/0,
    update_group_ec2_security_groups/2,
    where_ec2_sg_id_used/1
]).

-spec init() -> any().
init() ->
    pass.

-spec get_all_ips_by_id(Id :: binary()) -> {ok, iolist()}.
get_all_ips_by_id(Id) ->
    case Id of
        <<"self">> ->
            {ok, []};
        <<"all-active">> ->
            {ok, dog_ips:uniq(lists:flatten(all_ipv4s() ++ all_ipv6s()))};
        <<"internal-active">> ->
            {ok, all_internal_ips()};
        <<"external-active">> ->
            {ok, all_external_ipv4s()};
        _ ->
            {ok, Ipv4s} = get_all_ipv4s_by_id(Id),
            {ok, Ipv6s} = get_all_ipv6s_by_id(Id),
            {ok, lists:sort(dog_ips:uniq(lists:flatten(Ipv4s ++ Ipv6s)))}
    end.

zone_groups_in_groups_profiles() ->
    {ok, Groups} = get_active_groups(),
    Profiles = lists:map(
        fun(Group) ->
            Name = maps:get(<<"name">>, Group),
            ?LOG_DEBUG("Name: ~p~n", [Name]),
            case get_profile_by_name(Name) of
                {ok, Profile} ->
                    {Name, Profile};
                {error, notfound} ->
                    []
            end
        end,
        Groups
    ),
    GroupsInGroups = lists:map(
        fun({Name, Profile}) ->
            GroupsInProfile = dog_profile:get_zone_groups_in_profile(Profile),
            {Name, GroupsInProfile}
        end,
        lists:flatten(Profiles)
    ),
    maps:from_list(GroupsInGroups).

role_groups_in_groups_profiles() ->
    {ok, Groups} = get_active_groups(),
    ProfilesRaw = lists:map(
        fun(Group) ->
            Name = maps:get(<<"name">>, Group),
            ?LOG_DEBUG("Name: ~p~n", [Name]),
            case get_profile_by_name(Name) of
                {ok, Profile} ->
                    {Name, Profile};
                _ ->
                    []
            end
        end,
        Groups
    ),
    Profiles = lists:flatten(ProfilesRaw),
    ?LOG_DEBUG("Profiles: ~p", [Profiles]),
    GroupNamesInGroups = lists:map(
        fun({Name, Profile}) ->
            GroupIdsInProfile = dog_profile:get_role_groups_in_profile(Profile),
            GroupNamesInProfile = lists:map(
                fun(GroupId) ->
                    case get_name_by_id(GroupId) of
                        {ok, GroupName} ->
                            GroupName;
                        _ ->
                            []
                    end
                end,
                GroupIdsInProfile
            ),
            {Name, GroupNamesInProfile}
        end,
        lists:flatten(Profiles)
    ),
    ?LOG_DEBUG("GroupNamesInGroups: ~p", [GroupNamesInGroups]),
    maps:from_list(GroupNamesInGroups).

-spec role_group_effects_groups(GroupName :: binary()) -> ({ok, list()} | error).
role_group_effects_groups(GroupName) ->
    GroupsInGroups = role_groups_in_groups_profiles(),
    TupleList = dog_common:inverse_map_of_lists(GroupsInGroups),
    GroupEffectingGroups = dog_common:tuple_pairs_to_map_of_lists(TupleList),
    {ok, OtherGroupsEffected} =
        case maps:find(GroupName, GroupEffectingGroups) of
            error -> {ok, []};
            Else -> Else
        end,
    {ok, sets:to_list(sets:from_list(lists:flatten([OtherGroupsEffected, GroupName])))}.

-spec zone_group_effects_groups(ZoneId :: binary()) -> ({ok, list()} | error).
zone_group_effects_groups(ZoneId) ->
    GroupsInGroups = zone_groups_in_groups_profiles(),
    TupleList = dog_common:inverse_map_of_lists(GroupsInGroups),
    GroupEffectingGroups = dog_common:tuple_pairs_to_map_of_lists(TupleList),
    case maps:find(ZoneId, GroupEffectingGroups) of
        error -> {ok, []};
        Else -> Else
    end.

-spec get_active_groups() -> {ok, list()}.
get_active_groups() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:order_by(X, <<"profile_id">>, #{index => <<"profile_id">>}),
            reql:pluck(X, [<<"name">>, <<"id">>])
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Groups =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Groups}.

-spec get_all() -> {ok, list()}.
get_all() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:pluck(X, [
                <<"name">>,
                <<"id">>,
                <<"profile_id">>,
                <<"profile_name">>,
                <<"profile_version">>,
                <<"ec2_security_group_ids">>
            ])
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Groups =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    Groups@1 = lists:append(Groups, [all_active(), self_group()]),
    {ok, Groups@1}.

-spec get_all_grouped_by_id() -> map().
get_all_grouped_by_id() ->
    {ok, All} = get_all(),
    maps:from_list([{maps:get(<<"id">>, Group), Group} || Group <- All]).

-spec get_all_grouped_by_name() -> map().
get_all_grouped_by_name() ->
    {ok, All} = get_all(),
    maps:from_list([{maps:get(<<"name">>, Group), Group} || Group <- All]).

-spec get_group_names() -> list().
get_group_names() ->
    {ok, Groups} = get_all(),
    GroupNames = [maps:get(<<"name">>, Group) || Group <- Groups],
    GroupNames.

-spec create(Group :: map()) -> {ok, Key :: iolist()}.
create(Group@0) when is_map(Group@0) ->
    GroupName = maps:get(<<"name">>, Group@0),
    GroupResult = dog_group:get_by_name(GroupName),
    DefaultMap = #{
        <<"hash4_ipsets">> => <<"">>,
        <<"hash6_ipsets">> => <<"">>,
        <<"hash4_iptables">> => <<"">>,
        <<"hash6_iptables">> => <<"">>,
        <<"ipset_hash">> => <<"">>,
        <<"external_ipv4_addresses">> => [],
        <<"external_ipv6_addresses">> => [],
        <<"profile_version">> => <<"latest">>
    },
    case GroupResult of
        {error, notfound} ->
            Timestamp = dog_time:timestamp(),
            Group@1 = maps:put(<<"created">>, Timestamp, Group@0),
            Group@2 =
                case maps:find(<<"profile_name">>, Group@1) of
                    error ->
                        NewMap = maps:merge(DefaultMap, Group@1),
                        ?LOG_INFO("NewMap: ~p", [NewMap]),
                        NewMap;
                    {ok, ProfileName} ->
                        ProfileId =
                            case dog_profile:get_by_name(ProfileName) of
                                {error, notfound} ->
                                    <<"">>;
                                {ok, Profile} ->
                                    case maps:find(<<"id">>, Profile) of
                                        error ->
                                            <<"">>;
                                        {ok, Id} ->
                                            Id
                                    end
                            end,
                        NewMap = maps:merge(DefaultMap, Group@1),
                        ?LOG_DEBUG("NewMap: ~p", [NewMap]),
                        maps:merge(NewMap, #{<<"profile_id">> => ProfileId})
                end,
            case dog_json_schema:validate(?VALIDATION_TYPE, Group@2) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:insert(X, Group@2)
                        end
                    ),
                    Key = hd(maps:get(<<"generated_keys">>, R)),
                    {ok, Key};
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {ok, _} ->
            {error, exists}
    end.

-spec get_profile_by_id(binary()) -> {ok, binary()} | {error, notfound}.
get_profile_by_id(GroupId) ->
    try
        R = dog_rethink:run(
            fun(X) ->
                reql:db(X, dog),
                reql:table(X, ?TYPE_TABLE),
                reql:get(X, GroupId)
            end
        ),
        ProfileId = nested:get([<<"profile_id">>], R),
        ProfileId
    of
        {ok, Id} -> {ok, Id};
        {badkey, _} -> {error, notfound}
    catch
        Exception:ExceptionReason:Stacktrace ->
            ?LOG_INFO(#{
                exception => Exception,
                exceptionreason => ExceptionReason,
                stacktrace => Stacktrace
            }),
            {error, notfound}
    end.

-spec in_active_profile(GroupId :: binary()) -> {false, []} | {true, Rules :: list()}.
in_active_profile(GroupId) ->
    {ok, Used} = dog_zone:where_used(GroupId),
    {ok, Active} = dog_profile:all_active(),
    Rules = sets:to_list(sets:intersection(sets:from_list(Used), sets:from_list(Active))),
    case Rules of
        [] ->
            {false, []};
        _ ->
            {true, Rules}
    end.

-spec in_profile(GroupId :: binary()) -> {false, []} | {true, Rules :: list()}.
in_profile(GroupId) ->
    {ok, Used} = dog_zone:where_used(GroupId),
    {ok, All} = dog_profile:all(),
    Rules = sets:to_list(sets:intersection(sets:from_list(Used), sets:from_list(All))),
    case Rules of
        [] ->
            {false, []};
        _ ->
            {true, Rules}
    end.

all_active() ->
    ExternalIpv4s = all_external_ipv4s(),
    ExternalIpv6s = all_external_ipv6s(),
    InternalIpv4s = all_internal_ipv4s(),
    InternalIpv6s = all_internal_ipv6s(),
    #{
        <<"name">> => <<"all-active">>,
        <<"id">> => <<"all-active">>,
        <<"external_ipv4_addresses">> => ExternalIpv4s,
        <<"external_ipv6_addresses">> => ExternalIpv6s,
        <<"internal_ipv4_addresses">> => InternalIpv4s,
        <<"internal_ipv6_addresses">> => InternalIpv6s
    }.

self_group() ->
    #{
        <<"name">> => <<"self">>,
        <<"id">> => <<"self">>,
        <<"external_ipv4_addresses">> => [],
        <<"external_ipv6_addresses">> => [],
        <<"internal_ipv4_addresses">> => [],
        <<"internal_ipv6_addresses">> => []
        }.

internal_active() ->
    InternalIpv4s = all_internal_ipv4s(),
    InternalIpv6s = all_internal_ipv6s(),
    #{
        <<"name">> => <<"internal-active">>,
        <<"id">> => <<"internal-active">>,
        <<"internal_ipv4_addresses">> => InternalIpv4s,
        <<"internal_ipv6_addresses">> => InternalIpv6s
    }.

external_active() ->
    ExternalIpv4s = all_external_ipv4s(),
    ExternalIpv6s = all_external_ipv6s(),
    #{
        <<"name">> => <<"external-active">>,
        <<"id">> => <<"external-active">>,
        <<"external_ipv4_addresses">> => ExternalIpv4s,
        <<"external_ipv6_addresses">> => ExternalIpv6s
    }.

-spec get_id_by_name(GroupName :: binary()) -> {ok, binary()}.
get_id_by_name(GroupName) ->
    case GroupName of
        <<"self">> ->
            {ok, <<"self">>};
        <<"all-active">> ->
            {ok, <<"all-active">>};
        <<"internal-active">> ->
            {ok, <<"internal-active">>};
        <<"external-active">> ->
            {ok, <<"external-active">>};
        _ ->
            case get_by_name(GroupName) of
                {ok, Group} ->
                    Id = maps:get(<<"id">>, Group),
                    {ok, Id};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec get_name_by_id(GroupId :: binary()) -> {ok, binary()} | {error, atom()}.
get_name_by_id(GroupId) ->
    case GroupId of
        <<"self">> ->
            {ok, <<"self">>};
        <<"all-active">> ->
            {ok, <<"all-active">>};
        <<"internal-active">> ->
            {ok, <<"internal-active">>};
        <<"external-active">> ->
            {ok, <<"external-active">>};
        _ ->
            R = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, ?TYPE_TABLE),
                    reql:get(X, GroupId),
                    reql:get_field(X, <<"name">>)
                end
            ),
            R
    end.

-spec get_profile_by_name(GroupName :: binary()) -> {ok, map()} | {error, atom()}.
get_profile_by_name(GroupName) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get_all(X, GroupName, #{index => <<"name">>}),
            reql:has_fields(X, [<<"profile_id">>]),
            reql:pluck(X, [<<"profile_version">>, <<"profile_name">>, <<"profile_id">>])
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    case lists:flatten(Result) of
        [] ->
            {error, notfound};
        GroupRecord@0 ->
            GroupRecord@1 = hd(GroupRecord@0),
            ProfileId = maps:get(<<"profile_id">>, GroupRecord@1),
            dog_profile:get_by_id(ProfileId)
    end.

set_hash4_ipsets(GroupName, Hash) ->
    set_hash(GroupName, Hash, <<"hash4_ipsets">>).
set_hash6_ipsets(GroupName, Hash) ->
    set_hash(GroupName, Hash, <<"hash6_ipsets">>).
set_hash4_iptables(GroupName, Hash) ->
    set_hash(GroupName, Hash, <<"hash4_iptables">>).
set_hash6_iptables(GroupName, Hash) ->
    set_hash(GroupName, Hash, <<"hash6_iptables">>).

-spec set_hash(GroupName :: binary(), Hash :: binary(), Field :: binary()) -> {ok, any()}.
set_hash(GroupName, Hash, Field) ->
    ?LOG_DEBUG("GroupName: ~p, Hash: ~p", [GroupName, Hash]),
    {ok, GroupId} = dog_group:get_id_by_name(GroupName),
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, GroupId),
            reql:update(X, #{Field => Hash})
        end
    ),
    {ok, R}.

-spec get_by_name(GroupName :: binary()) -> {ok, map()} | {error, atom()}.
get_by_name(GroupName) ->
    case GroupName of
        <<"self">> ->
            {ok, self_group()};
        <<"all-active">> ->
            {ok, all_active()};
        <<"internal-active">> ->
            {ok, internal_active()};
        <<"external-active">> ->
            {ok, external_active()};
        _ ->
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, ?TYPE_TABLE),
                    reql:get_all(X, GroupName, #{index => <<"name">>})
                end
            ),
            {ok, R3} = rethink_cursor:all(R),
            R4 = lists:flatten(R3),
            case R4 of
                [] ->
                    {error, notfound};
                _ ->
                    Result = hd(R4),
                    case Result of
                        {error, Error} ->
                            ?LOG_ERROR("group name not found: ~p, ~p", [GroupName, Error]),
                            {error, Error};
                        GroupJson ->
                            {ok, GroupJson}
                    end
            end
    end.

-spec get_by_id(GroupId :: binary()) -> {ok, map()} | {error, atom()}.
get_by_id(GroupId) ->
    case GroupId of
        <<"self">> ->
            {ok, self_group()};
        <<"all-active">> ->
            {ok, all_active()};
        <<"internal-active">> ->
            {ok, internal_active()};
        <<"external-active">> ->
            {ok, external_active()};
        <<"any">> ->
            {ok, #{
                <<"external_ipv4_addresses">> => [<<"0.0.0.0">>],
                <<"external_ipv6_addresses">> => [<<"::/0">>],
                <<"name">> => <<"any">>
            }};
        _ ->
            get_document_by_id(GroupId)
    end.

-spec get_document_by_id(GroupId :: binary()) -> {atom(), any()}.
get_document_by_id(Id) ->
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
    end.

-spec replace(Id :: binary(), ReplaceMap :: map()) ->
    {'false', 'no_replaced' | 'notfound' | binary()}
    | {'true', binary()}
    | {'validation_error', binary()}.
replace(Id, ReplaceMap) ->
    case get_by_id(Id) of
        {ok, OldExternal} ->
            NewItem = maps:merge(OldExternal, ReplaceMap),
            NewItem2 = dog_time:merge_timestamp(NewItem),
            NewItem3 = maps:put(<<"id">>, Id, NewItem2),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewItem3) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:get(X, Id),
                            reql:replace(X, NewItem3)
                        end
                    ),
                    ?LOG_DEBUG("replaced R: ~p~n", [R]),
                    Replaced = maps:get(<<"replaced">>, R),
                    Unchanged = maps:get(<<"unchanged">>, R),
                    case {Replaced, Unchanged} of
                        {1, 0} -> {true, Id};
                        {0, 1} -> {false, Id};
                        _ -> {false, no_replaced}
                    end;
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {error, Error} ->
            {false, Error}
    end.

-spec update(GroupId :: binary(), UpdateMap :: map()) -> {atom(), any()}.
update(Id, UpdateMap) ->
    case get_by_id(Id) of
        {ok, OldGroup} ->
            OldGroup1 = maps:remove(<<"vars">>,OldGroup),
            OldGroup2 = maps:remove(<<"ec2_security_group_ids">>, OldGroup1),
            NewGroup = maps:merge(OldGroup2, UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewGroup) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:get(X, Id),
                            reql:replace(X, NewGroup, #{return_changes => always})
                        end
                    ),
                    ?LOG_DEBUG("update R: ~p~n", [R]),
                    Replaced = maps:get(<<"replaced">>, R),
                    Unchanged = maps:get(<<"unchanged">>, R),
                    case {Replaced, Unchanged} of
                        {1, 0} -> {true, Id};
                        {0, 1} -> {false, Id};
                        _ -> {false, no_updated}
                    end;
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {error, Error} ->
            {false, Error}
    end.

-spec delete(GroupId :: binary()) -> (ok | {error, Error :: iolist()}).
delete(Id) ->
    case in_profile(Id) of
        {false, []} ->
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
            end;
        {true, Profiles} ->
            ?LOG_INFO("group ~p not deleted, in profiles: ~p~n", [Id, Profiles]),
            {error, #{<<"errors">> => #{<<"in active profile">> => Profiles}}}
    end.

-spec get_internal_ips_by_id(binary()) -> {'ok', list()} | {'error', atom()}.
get_internal_ips_by_id(GroupId) ->
    case GroupId of
        <<"self">> ->
            {ok, []};
        <<"all-active">> ->
            {ok, all_ips()};
        <<"internal-active">> ->
            {ok, all_internal_ips()};
        <<"external-active">> ->
            {ok, all_external_ips()};
        _ ->
            {ok, Interfaces} = get_group_interfaces_by_id(GroupId),
            {ok, IPs} = dog_ips:addresses_from_interfaces(Interfaces),
            {ok, IPs}
    end.

-spec get_internal_ipv4s_by_id(binary()) -> {'ok', list()} | {'error', atom()}.
get_internal_ipv4s_by_id(GroupId) ->
    case GroupId of
        <<"self">> ->
            {ok, []};
        <<"all-active">> ->
            {ok, all_ipv4s()};
        <<"internal-active">> ->
            {ok, all_internal_ipv4s()};
        <<"external-active">> ->
            {ok, []};
        _ ->
            {ok, IPs} = get_internal_ips_by_id(GroupId),
            IPv4s = dog_ips:filter_ipv4(IPs),
            {ok, IPv4s}
    end.

-spec get_internal_ipv6s_by_id(binary()) -> {'ok', list()} | {'error', atom()}.
get_internal_ipv6s_by_id(GroupId) ->
    case GroupId of
        <<"self">> ->
            {ok, []};
        <<"all-active">> ->
            {ok, all_ipv6s()};
        <<"internal-active">> ->
            {ok, all_internal_ipv6s()};
        <<"external-active">> ->
            {ok, []};
        _ ->
            {ok, IPs} = get_internal_ips_by_id(GroupId),
            IPv6s = dog_ips:filter_ipv6(IPs),
            {ok, IPv6s}
    end.

-spec get_external_ips_by_id(binary()) -> {'ok', list()} | {'error', atom()}.
get_external_ips_by_id(Id) ->
    case Id of
        <<"self">> ->
            {ok, []};
        <<"all-active">> ->
            {ok,
                dog_ips:uniq(
                    lists:sort(lists:flatten(all_external_ipv4s() ++ all_external_ipv6s()))
                )};
        <<"internal-active">> ->
            {ok, []};
        <<"external-active">> ->
            {ok, all_external_ips()};
        _ ->
            {ok, Ipv4s} = get_external_ipv4s_by_id(Id),
            {ok, Ipv6s} = get_external_ipv6s_by_id(Id),
            {ok, dog_ips:uniq(lists:sort(lists:flatten(Ipv4s ++ Ipv6s)))}
    end.

-spec get_external_ipv6s_by_id(binary()) -> {'ok', list()} | {'error', atom()}.
get_external_ipv6s_by_id(Id) ->
    case Id of
        <<"self">> ->
            {ok, []};
        <<"all-active">> ->
            {ok, all_external_ipv6s()};
        <<"internal-active">> ->
            {ok, []};
        <<"external-active">> ->
            {ok, all_external_ipv6s()};
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
                    {notfound, []};
                _ ->
                    ExternalAddresses = maps:get(<<"external_ipv6_addresses">>, R, []),
                    {ok, dog_ips:filter_ipv6(ExternalAddresses)}
            end
    end.

-spec get_external_ipv4s_by_id(binary()) -> {'ok', list()} | {'error', atom()}.
get_external_ipv4s_by_id(Id) ->
    case Id of
        <<"self">> ->
            {ok, []};
        <<"all-active">> ->
            {ok, all_external_ipv4s()};
        <<"internal-active">> ->
            {ok, []};
        <<"external-active">> ->
            {ok, all_external_ipv6s()};
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
                    {notfound, []};
                _ ->
                    ExternalAddresses = maps:get(<<"external_ipv4_addresses">>, R, []),
                    {ok, dog_ips:filter_ipv4(ExternalAddresses)}
            end
    end.

-spec get_hosts_by_id(GroupId :: binary()) -> {ok, list()}.
get_hosts_by_id(GroupId) ->
    case GroupId of
        <<"self">> ->
            {ok, []};
        <<"all-active">> ->
            dog_host:get_all();
        <<"internal-active">> ->
            dog_host:get_all();
        <<"external-active">> ->
            {ok, []};
        _ ->
            {ok, Group} = get_by_id(GroupId),
            GroupName = maps:get(<<"name">>, Group),
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, host),
                    reql:filter(X, fun(Y) ->
                        reql:bracket(Y, <<"group">>),
                        reql:eq(Y, GroupName)
                    end),
                    reql:pluck(X, [<<"name">>, <<"id">>, <<"hostkey">>])
                end
            ),
            {ok, Result} = rethink_cursor:all(R),
            Hosts = lists:flatten(Result),
            Hosts@1 = lists:map(fun(X) -> X end, Hosts),
            case Hosts@1 of
                [] -> {ok, []};
                _ -> {ok, Hosts@1}
            end
    end.

-spec get_group_interfaces_by_id(GroupId :: binary()) -> {ok, iolist()}.
get_group_interfaces_by_id(GroupId) ->
    {ok, Group} = get_by_id(GroupId),
    GroupName = maps:get(<<"name">>, Group),
    get_group_interfaces_by_name(GroupName).

-spec get_all_group_interfaces() -> {'ok', [any()]}.
get_all_group_interfaces() ->
    get_all_group_interfaces(true).
-spec get_all_group_interfaces(OnlyActive :: boolean()) -> {'ok', [any()]}.
get_all_group_interfaces(OnlyActive) ->
    case OnlyActive of
        true ->
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, host),
                    reql:filter(X, #{<<"active">> => <<"active">>}),
                    reql:get_field(X, <<"interfaces">>)
                end
            ),
            {ok, Result} = rethink_cursor:all(R),
            Interfaces = lists:flatten(Result),
            ?LOG_DEBUG("Interfaces: ~p", [Interfaces]),
            Interfaces@1 = merge(Interfaces),
            ?LOG_DEBUG("Interfaces@1: ~p", [Interfaces@1]),
            case Interfaces@1 of
                [] -> {ok, []};
                _ -> {ok, Interfaces@1}
            end;
        false ->
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, host),
                    reql:get_field(X, <<"interfaces">>)
                end
            ),
            {ok, Result} = rethink_cursor:all(R),
            Interfaces = lists:flatten(Result),
            Interfaces@1 = merge(Interfaces),
            case Interfaces@1 of
                [] -> {ok, []};
                _ -> {ok, Interfaces@1}
            end
    end.

-spec get_group_interfaces_by_name(GroupName :: iolist()) -> {'ok', [any()]}.
get_group_interfaces_by_name(GroupName) ->
    get_group_interfaces_by_name(GroupName, true).
-spec get_group_interfaces_by_name(GroupName :: iolist(), OnlyActive :: boolean()) ->
    {'ok', [any()]}.
get_group_interfaces_by_name(GroupName, OnlyActive) ->
    case OnlyActive of
        true ->
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, host),
                    reql:get_all(X, GroupName, #{index => <<"group">>}),
                    reql:filter(X, #{<<"active">> => <<"active">>}),
                    reql:get_field(X, <<"interfaces">>)
                end
            ),
            {ok, Result} = rethink_cursor:all(R),
            Interfaces = lists:flatten(Result),
            Interfaces@1 = merge(Interfaces),
            case Interfaces@1 of
                [] -> {ok, []};
                _ -> {ok, Interfaces@1}
            end;
        false ->
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, host),
                    reql:get_all(X, GroupName, #{index => <<"group">>}),
                    reql:get_field(X, <<"interfaces">>)
                end
            ),
            {ok, Result} = rethink_cursor:all(R),
            Interfaces = lists:flatten(Result),
            Interfaces@1 = merge(Interfaces),
            case Interfaces@1 of
                [] -> {ok, []};
                _ -> {ok, Interfaces@1}
            end
    end.
%TODO
merge(Interfaces) ->
    Interfaces@1 = [jsx:decode(I) || I <- Interfaces],
    L3 = lists:foldl(
        fun(Interface, Acc) ->
            merge_join(Interface, Acc)
        end,
        [],
        Interfaces@1
    ),
    L4 = [{Key, lists:flatten(Values)} || {Key, Values} <- L3],
    L4.

-spec merge_join(L1 :: list(), L2 :: list()) -> list().
merge_join(L1, []) ->
    L1;
merge_join(L1, L2) ->
    R = sofs:relation(L1 ++ L2),
    F = sofs:relation_to_family(R),
    L3 = sofs:to_external(F),
    L3.

-spec all_ipv4s_grouped() -> map().
all_ipv4s_grouped() ->
    GroupNames = get_group_names(),
    Ipv4s = lists:map(
        fun(Name) ->
            {ok, Ips} = get_all_ipv4s_by_name(Name),
            {Name, Ips}
        end,
        GroupNames
    ),
    maps:from_list(lists:flatten(Ipv4s)).

-spec all_ipv6s_grouped() -> map().
all_ipv6s_grouped() ->
    GroupNames = get_group_names(),
    Ipv6s = lists:map(
        fun(Name) ->
            {ok, Ips} = get_all_ipv6s_by_name(Name),
            {Name, Ips}
        end,
        GroupNames
    ),
    maps:from_list(lists:flatten(Ipv6s)).

-spec all_ipv4s() -> list().
all_ipv4s() ->
    {ok, Interfaces} = get_all_group_interfaces(),
    {ok, Ips} = dog_ips:addresses_from_interfaces(Interfaces),
    AllExternalIPv4s = all_external_ipv4s(),
    Ipv4s = dog_ips:filter_ipv4(Ips),
    UniqueIpv4s = dog_ips:uniq(lists:flatten(Ipv4s ++ AllExternalIPv4s)),
    UniqueIpv4s.

-spec all_ipv6s() -> list().
all_ipv6s() ->
    {ok, Interfaces} = get_all_group_interfaces(),
    {ok, Ips} = dog_ips:addresses_from_interfaces(Interfaces),
    AllExternalIPv6s = all_external_ipv6s(),
    Ipv6s = dog_ips:filter_ipv6(Ips),
    UniqueIpv6s = dog_ips:uniq(lists:flatten(Ipv6s ++ AllExternalIPv6s)),
    UniqueIpv6s.

-spec all_ips() -> list().
all_ips() ->
    dog_ips:uniq(lists:flatten(all_ipv4s() ++ all_ipv6s())).

-spec all_internal_ips() -> list().
all_internal_ips() ->
    {ok, Interfaces} = dog_host:get_all_active_interfaces(),
    {ok, Ips} = dog_ips:addresses_from_interfaces(Interfaces),
    Ips.

-spec all_internal_ipv4s() -> list().
all_internal_ipv4s() ->
    Ips = all_internal_ips(),
    Ipv4s = dog_ips:filter_ipv4(Ips),
    Ipv4s.

-spec all_internal_ipv6s() -> list().
all_internal_ipv6s() ->
    Ips = all_internal_ips(),
    Ipv6s = dog_ips:filter_ipv6(Ips),
    Ipv6s.

-spec all_external_ips() -> list().
all_external_ips() ->
    {ok, ExternalIPs} = get_external_ips_by_id(<<"all-active">>),
    ExternalIPs.

-spec all_external_ipv4s() -> list().
all_external_ipv4s() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:pluck(X, [<<"external_ipv4_addresses">>])
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    ExternalIpv4s =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    Ipv4s = [maps:get(<<"external_ipv4_addresses">>, Addresses) || Addresses <- ExternalIpv4s],
    UniqueIpv4s = dog_ips:uniq(Ipv4s),
    dog_ips:filter_ipv4(lists:flatten(UniqueIpv4s)).

-spec all_external_ipv6s() -> list().
all_external_ipv6s() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:pluck(X, [<<"external_ipv6_addresses">>])
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    ExternalIpv6s =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    Ipv6s = [maps:get(<<"external_ipv6_addresses">>, Addresses, []) || Addresses <- ExternalIpv6s],
    UniqueIpv6s = dog_ips:uniq(Ipv6s),
    dog_ips:filter_ipv6(lists:flatten(UniqueIpv6s)).

-spec get_all_ipv4s_by_name(GroupName :: binary()) -> {'ok', [any()]}.
get_all_ipv4s_by_name(GroupName) ->
    {ok, GroupId} = get_id_by_name(GroupName),
    get_all_ipv4s_by_id(GroupId).

-spec get_all_ipv6s_by_name(GroupName :: binary()) -> {'ok', [any()]}.
get_all_ipv6s_by_name(GroupName) ->
    {ok, GroupId} = get_id_by_name(GroupName),
    get_all_ipv6s_by_id(GroupId).

-spec get_all_ipv4s_by_id(GroupId :: binary()) -> {'ok', [any()]}.
get_all_ipv4s_by_id(GroupId) ->
    case GroupId of
        <<"self">> ->
            {ok, []};
        <<"all-active">> ->
            {ok, all_ipv4s()};
        <<"internal-active">> ->
            {ok, all_internal_ipv4s()};
        <<"external-active">> ->
            {ok, all_external_ipv4s()};
        _ ->
            {ok, IPs} = get_internal_ips_by_id(GroupId),
            {ok, External_IPv4s} = get_external_ipv4s_by_id(GroupId),
            IPv4s = dog_ips:filter_ipv4(lists:flatten(IPs ++ External_IPv4s)),
            {ok, IPv4s}
    end.

-spec get_all_ipv6s_by_id(GroupId :: binary()) -> {'ok', [any()]}.
get_all_ipv6s_by_id(GroupId) ->
    case GroupId of
        <<"self">> ->
            {ok, []};
        <<"all-active">> ->
            {ok, all_ipv6s()};
        <<"internal-active">> ->
            {ok, all_internal_ipv6s()};
        <<"external-active">> ->
            {ok, all_external_ipv6s()};
        _ ->
            {ok, IPs} = get_internal_ips_by_id(GroupId),
            {ok, External_IPv6s} = get_external_ipv6s_by_id(GroupId),
            IPv6s = dog_ips:filter_ipv6(lists:flatten(IPs ++ External_IPv6s)),
            {ok, IPv6s}
    end.

-spec replace_profile_by_profile_id(OldId :: binary(), NewId :: binary()) -> list().
replace_profile_by_profile_id(OldId, NewId) ->
    GroupIds = get_ids_with_profile_id(OldId),
    Results = lists:map(
        fun(GroupId) ->
            update(GroupId, #{<<"profile_id">> => NewId})
        end,
        GroupIds
    ),
    Results.

-spec replace_profile_by_profile_id(OldId :: binary(), NewId :: binary(), ProfileName :: iolist()) ->
    list().
replace_profile_by_profile_id(OldId, NewId, ProfileName) ->
    ?LOG_DEBUG("OldId: ~p, NewId: ~p, ProfileName: ~p", [OldId, NewId, ProfileName]),
    GroupIds = get_ids_with_profile_id(OldId),
    Results = lists:map(
        fun(GroupId) ->
            update(GroupId, #{<<"profile_id">> => NewId, <<"profile_name">> => ProfileName})
        end,
        GroupIds
    ),
    Results.

-spec get_ids_with_profile_id(Id :: binary()) -> iolist().
get_ids_with_profile_id(Id) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:order_by(X, <<"profile_id">>, #{index => <<"profile_id">>}),
            reql:pluck(X, [<<"id">>, <<"profile_id">>, <<"profile_version">>])
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Groups =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    Ids = [maps:get(<<"id">>, X) || X <- Groups, maps:get(<<"profile_id">>, X) == Id],
    Ids.

-spec get_schema() -> binary().
get_schema() ->
    dog_json_schema:get_file(?VALIDATION_TYPE).

-spec get_all_inbound_ports_by_protocol(GroupName :: string()) -> ProtocolPorts :: list().
get_all_inbound_ports_by_protocol(GroupName) ->
    case get_profile_by_name(GroupName) of
        {error, _Error} ->
            ?LOG_INFO("No profile associated with group: ~p", [GroupName]),
            throw(profile_not_found);
        {ok, ProfileJson} ->
            dog_profile:get_all_inbound_ports_by_protocol(ProfileJson)
    end.

-spec get_ppps_inbound_ec2(Group :: map(), Region :: string()) -> list().
get_ppps_inbound_ec2(Group, Region) ->
    Ec2SecurityGroupList = maps:get(<<"ec2_security_group_ids">>, Group, []),
    Ec2SecurityGroupMap = dog_common:lmm(Ec2SecurityGroupList, <<"region">>),
    Ec2Sg = maps:get(Region, Ec2SecurityGroupMap, []),
    case Ec2Sg of
        [] ->
            [];
        _ ->
            ProfileId = maps:get(<<"profile_id">>, Group),
            {ok, ProfileJson} = dog_profile:get_by_id(ProfileId),
            dog_profile:get_ppps_inbound_ec2(ProfileJson, Region)
    end.

-spec get_ppps_outbound_ec2(Group :: map(), Region :: string()) -> list().
get_ppps_outbound_ec2(Group, Region) ->
    Ec2SecurityGroupList = maps:get(<<"ec2_security_group_ids">>, Group, []),
    Ec2SecurityGroupMap = dog_common:lmm(Ec2SecurityGroupList, <<"region">>),
    Ec2Sg = maps:get(Region, Ec2SecurityGroupMap, []),
    case Ec2Sg of
        [] ->
            [];
        _ ->
            ProfileId = maps:get(<<"profile_id">>, Group),
            {ok, ProfileJson} = dog_profile:get_by_id(ProfileId),
            dog_profile:get_ppps_outbound_ec2(ProfileJson, Region)
    end.

-spec get_all_internal_ec2_security_group_ids() -> IdsByGroup :: map().
get_all_internal_ec2_security_group_ids() ->
    {ok, AllGroups} = get_all(),
    IdsByGroup = lists:map(
        fun(Group) ->
            GroupName = maps:get(<<"name">>, Group),
            case maps:get(<<"ec2_security_group_ids">>, Group, []) of
                [] ->
                    {GroupName, []};
                RegionGroups ->
                    {GroupName, RegionGroups}
            end
        end,
        AllGroups
    ),
    IdsByGroupMap = maps:from_list(IdsByGroup),
    IdsByGroupMap.

%GROUP BASED EC2 INFO
get_internal_ec2_security_group_ids_by_id(GroupId) ->
    {ok, Group} = get_by_id(GroupId),
    case maps:get(<<"ec2_security_group_ids">>, Group, []) of
        [] ->
            [];
        RegionGroups ->
            RegionGroups
    end.
%TODO
get_ec2_security_group_ids_by_name(GroupName) ->
    case get_id_by_name(GroupName) of
        {ok, GroupId} ->
            case get_internal_ec2_security_group_ids_by_id(GroupId) of
                [] ->
                    AllActiveUnionEc2Sgs = dog_external:get_all_active_union_ec2_sgs(),
                    case maps:get(GroupName, AllActiveUnionEc2Sgs, []) of
                        [] ->
                            [];
                        SgIds ->
                            SgIds
                    end;
                SgIds ->
                    SgIds
            end;
        {error, notfound} ->
            AllActiveUnionEc2Sgs = dog_external:get_all_active_union_ec2_sgs(),
            case maps:get(GroupName, AllActiveUnionEc2Sgs, []) of
                [] ->
                    [];
                SgIds ->
                    SgIds
            end
    end.

%HOST BASED EC2 INFO
get_ec2_security_group_ids_from_members(GroupName) ->
    {ok, GroupId} = get_id_by_name(GroupName),
    {ok, HostList} = get_hosts_by_id(GroupId),
    Ec2SecurityGroupList = lists:map(
        fun(Host) ->
            {ok, DogHost} = dog_host:get_by_id(maps:get(<<"id">>, Host)),
            Ec2SecurityGroupIds = maps:get(<<"ec2_security_group_ids">>, DogHost, []),
            Ec2AvailablityZone = maps:get(<<"ec2_availability_zone">>, DogHost, []),
            Ec2Region =
                case Ec2AvailablityZone of
                    [] ->
                        [];
                    _ ->
                        binary:list_to_bin(lists:droplast(binary:bin_to_list(Ec2AvailablityZone)))
                end,
            lists:map(
                fun(Ec2SecurityGroupId) ->
                    {Ec2Region, Ec2SecurityGroupId}
                end,
                Ec2SecurityGroupIds
            )
        end,
        HostList
    ),
    sets:to_list(sets:from_list(lists:flatten(Ec2SecurityGroupList))).

all_ec2_sg_mappings() ->
    lists:filter(
        fun(X) -> element(2, X) =/= [] end,
        ([
            {G, dog_group:get_ec2_security_group_ids_from_members(G)}
         || G <- get_group_names(), G =/= <<"all-active">>
        ])
    ).

set_ec2_group_mappings_from_members() ->
    Ec2SgMappings = all_ec2_sg_mappings(),
    lists:map(
        fun({GroupName, SgList}) ->
            set_ec2_group_mappings_from_members(GroupName, SgList)
        end,
        Ec2SgMappings
    ).

set_ec2_group_mappings_from_members(GroupName, SgList) ->
    {ok, GroupId} = dog_group:get_id_by_name(GroupName),
    io:format("GroupId: ~p, SgList: ~p~n", [GroupId, maps:from_list(SgList)]),
    {ok, CurrentGroupMap} = dog_group:get_by_id(GroupId),
    SgListOfMaps = lists:map(
        fun({Region, SgId}) ->
            #{
                <<"region">> => Region,
                <<"sgid">> => SgId
            }
        end,
        SgList
    ),
    UpdateMap = maps:merge(
        CurrentGroupMap,
        #{
            <<"ec2_security_group_ids">> =>
                SgListOfMaps
        }
    ),
    dog_group:replace(GroupId, UpdateMap).

-spec where_ec2_sg_id_used(SgId :: binary()) -> {ok, list()}.
where_ec2_sg_id_used(SgId) ->
    {ok, R} = dog_rethink:run(fun(X) ->
        reql:db(X, dog),
        reql:table(X, group),
        reql:has_fields(X, [<<"ec2_security_group_ids">>]),
        reql:filter(X, fun(Y) ->
            reql:match(
                reql:to_json_string(
                    reql:bracket(Y, <<"ec2_security_group_ids">>)
                ),
                SgId
            )
        end),
        reql:get_field(X, <<"id">>)
    end),
    {ok, Result} = rethink_cursor:all(R),
    Groups =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Groups}.

-spec update_group_ec2_security_groups(GroupZoneIdentifier :: binary(), GroupType :: binary()) ->
    'ok'.
update_group_ec2_security_groups(GroupZoneIdentifier, GroupType) ->
    ?LOG_INFO("GroupZoneIdentifier: ~p", [GroupZoneIdentifier]),
    Groups =
        case GroupType of
            G when G =:= <<"role">>; G =:= <<"group">> ->
                {ok, G} = dog_group:role_group_effects_groups(GroupZoneIdentifier),
                G;
            <<"zone">> ->
                %TODO: Zone support in Ec2
                %{ok, G} = dog_group:zone_group_effects_groups(GroupZoneIdentifier),
                []
        end,
    AllActiveUnionEc2Sgs = dog_external:get_all_active_union_ec2_sgs(),
    GroupsWithEc2SgIds = lists:filter(
        fun(Group) ->
            case dog_group:get_ec2_security_group_ids_by_name(Group) of
                [] ->
                    case maps:get(Group, AllActiveUnionEc2Sgs, []) of
                        [] ->
                            false;
                        _ ->
                            true
                    end;
                _ ->
                    true
            end
        end,
        lists:flatten(Groups)
    ),
    ?LOG_INFO("Effected Groups: ~p", [GroupsWithEc2SgIds]),
    lists:foreach(
        fun(Group) ->
            dog_ec2_sg:publish_ec2_sg_by_name(Group)
        end,
        GroupsWithEc2SgIds
    ),
    ok.


-spec group_alert_active(GroupName :: string()) -> boolean.
group_alert_active(GroupName) ->
    GroupMap = get_by_name(GroupName),
    case maps:get(<<"alert_enable">>, GroupMap, true) of
        true -> true;
        false -> false
    end.
