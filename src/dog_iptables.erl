-module(dog_iptables).

-include("dog_trainer.hrl").

-export([
    publish_to_queue/7,
    update_all_iptables/0,
    update_group_iptables/2,
    update_group_ec2_sgs/1
]).

-export([
    chunk_list/1,
    chunk_list/2
]).

-spec update_group_iptables(GroupZoneName :: binary(), GroupType :: binary()) -> 'ok'.
update_group_iptables(GroupZoneName, GroupType) ->
    ?LOG_INFO("GroupZoneName: ~p", [GroupZoneName]),
    Groups =
        case application:get_env(dog_trainer, generate_unset_tables, true) of
            true ->
                {ok, GroupsList} =
                    case GroupType of
                        G when G =:= <<"role">>; G =:= <<"group">> ->
                            dog_group:role_group_effects_groups(GroupZoneName);
                        <<"zone">> ->
                            dog_group:zone_group_effects_groups(GroupZoneName)
                    end,
                GroupsList;
            false ->
                {ok, GroupsList} =
                    case GroupType of
                        G when G =:= <<"role">>; G =:= <<"group">> ->
                            {ok, [GroupZoneName]};
                        <<"zone">> ->
                            case dog_zone:get_by_name(GroupZoneName) of
                                {ok, Zone} ->
                                    ZoneId = maps:get(<<"id">>, Zone),
                                    dog_group:zone_group_effects_groups(ZoneId);
                                _ ->
                                    {ok, []}
                            end
                    end,
                GroupsList
        end,
    ?LOG_INFO("Effected Groups: ~p", [Groups]),
    ?LOG_INFO("add_to_queue: ~p", [Groups]),
    dog_profile_update_agent:add_to_queue(Groups),
    ok.

-spec update_group_ec2_sgs(GroupZoneName :: binary()) -> 'ok'.
update_group_ec2_sgs(GroupZoneName) ->
    {ok, GroupList} = dog_group:role_group_effects_groups(GroupZoneName),
    ?LOG_DEBUG("GroupList: ~p~n", [GroupList]),
    plists:map(
        fun(Group) ->
            dog_ec2_sg:publish_ec2_sg_by_name(Group)
        end,
        GroupList
    ).

-spec update_all_iptables() -> 'ok'.
update_all_iptables() ->
    ?LOG_DEBUG("update_all_iptables:start"),
    {ok, Groups} = dog_group:get_active_groups(),
    GroupNames = [maps:get(<<"name">>, Group) || Group <- Groups],
    ChunkedGroupNames = chunk_list(GroupNames, 2),
    lists:foreach(
        fun(GroupName) ->
            dog_profile_update_agent:add_to_queue(GroupName),
            timer:sleep(1000)
        end,
        ChunkedGroupNames
    ),
    ?LOG_DEBUG("update_all_iptables:end"),
    ok.

chunk_list(List) ->
    chunk_list(List, 2).

chunk_list([], _) ->
    [];
chunk_list(List, Len) when Len > length(List) ->
    [List];
chunk_list(List, Len) ->
    {Head, Tail} = lists:split(Len, List),
    [Head | chunk_list(Tail, Len)].


-spec publish_to_queue(
    RoutingKey :: binary(),
    Group :: binary(),
    R4IpsetsIptablesRuleset :: list() | boolean(),
    R6IpsetsIptablesRuleset :: list() | boolean(),
    R4IptablesIptablesRuleset :: list() | boolean(),
    R6IptablesIptablesRuleset :: list() | boolean(),
    Ipsets :: list()
) -> any().
publish_to_queue(
    RoutingKey,
    Group,
    R4IpsetsIptablesRuleset,
    R6IpsetsIptablesRuleset,
    R4IptablesIptablesRuleset,
    R6IptablesIptablesRuleset,
    Ipsets
) ->
    ?LOG_INFO("RoutingKey: ~p", [RoutingKey]),
    UserData = #{
        ruleset4_ipset => R4IpsetsIptablesRuleset,
        ruleset6_ipset => R6IpsetsIptablesRuleset,
        ruleset4_iptables => R4IptablesIptablesRuleset,
        ruleset6_iptables => R6IptablesIptablesRuleset,
        ipsets => Ipsets
    },
    Count = 1,
    Pid = erlang:self(),
    Message = term_to_binary([
        {count, Count}, {local_time, calendar:local_time()}, {pid, Pid}, {user_data, UserData}
    ]),
    Response = turtle:publish(
        iptables_publisher,
        <<"iptables">>,
        RoutingKey,
        <<"text/json">>,
        Message,
        #{delivery_mode => persistent}
    ),
    imetrics:add(iptables_publish),
    imetrics:add_m(iptables_publish_group, Group),
    Response.
