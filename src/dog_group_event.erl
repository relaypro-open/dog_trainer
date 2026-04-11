-module(dog_group_event).

-include("dog_trainer.hrl").

-export([
    on_create/1,
    on_update/2,
    on_delete/1
]).

-spec on_create(NewVal :: map()) -> {ok, any()} | {error, any()}.
on_create(NewVal) ->
    handle_change(NewVal).

-spec on_update(OldVal :: map(), NewVal :: map()) -> {ok, any()} | {error, any()}.
on_update(_OldVal, NewVal) ->
    handle_change(NewVal).

-spec on_delete(OldVal :: map()) -> {ok, any()} | {error, any()}.
on_delete(OldVal) ->
    GroupName = maps:get(<<"name">>, OldVal),
    imetrics:add_m(event, group_update),
    GroupType = <<"role">>,
    ?LOG_INFO(#{groupname => GroupName}, #{domain => [dog_trainer]}),
    dog_iptables:update_group_iptables(GroupName, GroupType),
    dog_iptables:update_group_ec2_sgs(GroupName),
    ?LOG_INFO(#{message => "dog_ipset_update_agent:queue_add()"}, #{domain => [dog_trainer]}),
    dog_ipset_update_agent:queue_add(
        dog_common:concat([<<"group-">>, GroupName], binary)
    ),
    ok.

handle_change(Val) ->
    GroupName = maps:get(<<"name">>, Val),
    imetrics:add_m(event, group_update),
    GroupType = <<"role">>,
    ?LOG_INFO(#{groupname => GroupName}, #{domain => [dog_trainer]}),
    dog_iptables:update_group_iptables(GroupName, GroupType),
    Ec2Result = dog_iptables:update_group_ec2_sgs(GroupName),
    {ok, R4IpsetsIptablesRuleset} = dog_iptables_ruleset:read_iptables_ruleset_set_v4_from_file(
        GroupName
    ),
    {ok, R6IpsetsIptablesRuleset} = dog_iptables_ruleset:read_iptables_ruleset_set_v6_from_file(
        GroupName
    ),
    {ok, R4IptablesIptablesRuleset} = dog_iptables_ruleset:read_iptables_ruleset_unset_v4_from_file(
        GroupName
    ),
    {ok, R6IptablesIptablesRuleset} = dog_iptables_ruleset:read_iptables_ruleset_unset_v6_from_file(
        GroupName
    ),
    Hash4Ipsets = dog_profile:create_hash(R4IpsetsIptablesRuleset),
    Hash6Ipsets = dog_profile:create_hash(R6IpsetsIptablesRuleset),
    Hash4Iptables = dog_profile:create_hash(R4IptablesIptablesRuleset),
    Hash6Iptables = dog_profile:create_hash(R6IptablesIptablesRuleset),
    {ok, _} = dog_group:set_hash4_ipsets(GroupName, Hash4Ipsets),
    {ok, _} = dog_group:set_hash6_ipsets(GroupName, Hash6Ipsets),
    {ok, _} = dog_group:set_hash4_iptables(GroupName, Hash4Iptables),
    {ok, _} = dog_group:set_hash6_iptables(GroupName, Hash6Iptables),
    ?LOG_INFO(#{message => "dog_ipset_update_agent:queue_add()"}, #{domain => [dog_trainer]}),
    dog_ipset_update_agent:queue_add(
        dog_common:concat([<<"group-">>, GroupName], binary)
    ),
    Ec2Result.
