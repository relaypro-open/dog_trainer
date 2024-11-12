-module(dog_ec2_sg).

-include("dog_trainer.hrl").
-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_ec2.hrl").

-export([
    config/1,
    publish_ec2_sg/1,
    publish_ec2_sg_by_name/1
]).

-export([
    diff_sg_egress/3,
    diff_sg_ingress/3,
    ip_permissions_egress/2,
    ip_permissions_ingress/2
]).

-spec config(Region :: binary()) -> tuple().
config(Region) ->
    {ok, Key} = application:get_env(dog_trainer, aws_key),
    {ok, Secret} = application:get_env(dog_trainer, aws_secret),
    Url = "ec2." ++ binary:bin_to_list(Region) ++ ".amazonaws.com",
    ?LOG_DEBUG(#{"url" => Url}),
    erlcloud_ec2:new(
        Key,
        Secret,
        Url
    ).

default_ingress_spps_rules(Ec2SecurityGroupId) ->
    [
        {'-1', 0, 0, {group_id, binary:bin_to_list(Ec2SecurityGroupId)}}
    ].

default_egress_spps_rules() ->
    [].

publish_ec2_sg_by_name(DogGroupName) ->
    case dog_group:get_by_name(DogGroupName) of
        {ok, DogGroup} ->
            publish_ec2_sgs(DogGroup);
        _ ->
            ?LOG_INFO(#{"dog_group_name" => DogGroupName, "message" => "Group not found"}),
            []
    end.

-spec publish_ec2_sgs(DogGroup :: map()) -> {ok | error, DetailedResults :: list()}.
publish_ec2_sgs(DogGroup) ->
    Ec2SecurityGroupList = maps:get(<<"ec2_security_group_ids">>, DogGroup, []),
    DogGroupName = maps:get(<<"name">>, DogGroup),
    Results = plists:map(
        fun(Ec2Sg) ->
            Region = maps:get(<<"region">>, Ec2Sg),
            SgId = maps:get(<<"sgid">>, Ec2Sg),
            ?LOG_DEBUG(#{"region" => Region, "sg_id" => " SgId"}),
            Ec2SgIds = dog_ec2_update_agent:ec2_security_group_ids(Region),
            case lists:member(binary:bin_to_list(SgId), Ec2SgIds) of
                true ->
                    {publish_ec2_sg({DogGroup, Region, SgId}), DogGroupName, Region, SgId};
                false ->
                    {{error, <<"ec2 security group not found">>}, DogGroupName, Region, SgId}
            end
        end,
        Ec2SecurityGroupList
    ),
    Results.

-spec publish_ec2_sg({DogGroup :: map(), Region :: string(), SgId :: string()}) ->
    {ok | error, DetailedResults :: list()}.
publish_ec2_sg({DogGroup, Region, SgId}) ->
    DogGroupId = maps:get(<<"id">>, DogGroup),
    AddRemoveMapIngress = diff_sg_ingress(SgId, Region, DogGroupId),
    ResultsIngress =
        {update_sg_ingress(
            SgId,
            Region,
            AddRemoveMapIngress
        )},
    ?LOG_DEBUG(#{"message" => "Ingress Results", "results_ingress" => ResultsIngress}),
    AddRemoveMapEgress = diff_sg_egress(SgId, Region, DogGroupId),
    ResultsEgress =
        {update_sg_egress(
            SgId,
            Region,
            AddRemoveMapEgress
        )},
    ?LOG_DEBUG(#{"results_egress" => ResultsEgress}),
    [ResultsIngress, ResultsEgress].

diff_sg_ingress(Ec2SecurityGroupId, Region, DogGroupId) ->
    {ok, DogGroup} = dog_group:get_by_id(DogGroupId),
    Ppps = dog_group:get_ppps_inbound_ec2(DogGroup, Region),
    DefaultPpps = default_ingress_spps_rules(Ec2SecurityGroupId),
    IngressRulesPpps = ordsets:to_list(ordsets:from_list(Ppps ++ DefaultPpps)),
    ?LOG_DEBUG(#{"ingress_rules_ppps" => IngressRulesPpps}),
    IngressRulesSpps = ppps_to_spps_ingress(IngressRulesPpps),
    ?LOG_DEBUG(#{"ingress_rules_spps" => IngressRulesSpps}),
    case dog_ec2_update_agent:ec2_security_group(Ec2SecurityGroupId, Region) of
        {error, Reason} ->
            ?LOG_ERROR(#{"ec2_security_group_id" => Ec2SecurityGroupId, "message" => "Ec2SecurityGroupId doesn't exist"}),
            {error, Reason};
        _ ->
            ExistingRulesSpps = ip_permissions_ingress(Region, Ec2SecurityGroupId),
            ?LOG_DEBUG(#{"existing_rules_spps" => ExistingRulesSpps}),
            ExistingRulesPpps = ingress_records_to_ppps(ExistingRulesSpps),
            NewAddVpcIngressPpps = ordsets:subtract(
                ordsets:from_list(IngressRulesPpps), ordsets:from_list(ExistingRulesPpps)
            ),
            ?LOG_DEBUG(#{"existing_rules_ppps" => ExistingRulesPpps}),
            RemoveVpcIngressPpps = ordsets:subtract(
                ordsets:from_list(ExistingRulesPpps), ordsets:from_list(IngressRulesPpps)
            ),
            ?LOG_DEBUG(#{"new_add_vpc_ingress_ppps" => NewAddVpcIngressPpps}),
            ?LOG_DEBUG(#{"remove_vpc_ingress_ppps" => RemoveVpcIngressPpps}),
            NewAddVpcIngressSpps = ppps_to_spps_ingress(NewAddVpcIngressPpps),
            RemoveVpcIngressSpps = ppps_to_spps_ingress(RemoveVpcIngressPpps),
            SgDiff = #{
                <<"Add">> => NewAddVpcIngressSpps,
                <<"Remove">> => RemoveVpcIngressSpps
            },
            ?LOG_DEBUG(#{"sg_diff" => SgDiff}),
            SgDiff
    end.

-spec update_sg_ingress(Ec2SecurityGroupId :: string(), Region :: string(), AddRemoveMap :: map()) ->
    {ok, tuple()} | {error, tuple()}.
update_sg_ingress(Ec2SecurityGroupId, Region, AddRemoveMap) ->
    Ec2SecurityGroupIdList = binary:bin_to_list(Ec2SecurityGroupId),
    Config = config(Region),
    NewAddVpcIngressSpecs = maps:get(<<"Add">>, AddRemoveMap),
    ?LOG_DEBUG(#{"new_add_vpc_ingress_specs" => NewAddVpcIngressSpecs}),
    AddResults =
        case NewAddVpcIngressSpecs of
            [] ->
                [];
            _ ->
                parse_authorize_response(
                    erlcloud_ec2:authorize_security_group_ingress(
                        Ec2SecurityGroupIdList, NewAddVpcIngressSpecs, Config
                    )
                )
        end,
    ?LOG_DEBUG(#{"add_results" => AddResults}),
    RemoveVpcIngressSpecs = maps:get(<<"Remove">>, AddRemoveMap),
    RemoveResults =
        case RemoveVpcIngressSpecs of
            [] ->
                [];
            _ ->
                parse_authorize_response(
                    erlcloud_ec2:revoke_security_group_ingress(
                        Ec2SecurityGroupIdList, RemoveVpcIngressSpecs, Config
                    )
                )
        end,
    AllResults = [AddResults, RemoveResults],
    ?LOG_DEBUG(#{"all_results" => AllResults}),
    AllResultTrueFalse = lists:all(fun(X) -> (X == ok) or (X == []) end, AllResults),
    AllResult =
        case AllResultTrueFalse of
            true -> ok;
            false -> error
        end,
    {AllResult, ingress, {{add_results, AddResults}, {remove_results, RemoveResults}}}.

diff_sg_egress(Ec2SecurityGroupId, Region, DogGroupId) ->
    {ok, DogGroup} = dog_group:get_by_id(DogGroupId),
    Ppps = dog_group:get_ppps_outbound_ec2(DogGroup, Region),
    DefaultPpps = default_egress_spps_rules(),
    EgressRulesPpps = ordsets:to_list(ordsets:from_list(Ppps ++ DefaultPpps)),
    EgressRulesSpps = ppps_to_spps_egress(EgressRulesPpps),
    ?LOG_DEBUG(#{"egress_rules_spps" => EgressRulesSpps}),
    case dog_ec2_update_agent:ec2_security_group(Ec2SecurityGroupId, Region) of
        {error, Reason} ->
            ?LOG_ERROR(#{"ec2_security_group_id" => Ec2SecurityGroupId, "message" => "Ec2SecurityGroupId doesn't exist"}),
            {error, Reason};
        _ ->
            ExistingRulesSpps = ip_permissions_egress(Region, Ec2SecurityGroupId),
            ?LOG_DEBUG(#{"existing_rules_spps" => ExistingRulesSpps}),
            ExistingRulesPpps = egress_records_to_ppps(ExistingRulesSpps),
            NewAddVpcEgressPpps = ordsets:subtract(
                ordsets:from_list(EgressRulesPpps), ordsets:from_list(ExistingRulesPpps)
            ),
            ?LOG_DEBUG(#{"existing_rules_ppps" => ExistingRulesPpps}),
            RemoveVpcEgressPpps = ordsets:subtract(
                ordsets:from_list(ExistingRulesPpps), ordsets:from_list(EgressRulesPpps)
            ),
            ?LOG_DEBUG(#{"new_add_vpc_egress_ppps" => NewAddVpcEgressPpps}),
            ?LOG_DEBUG(#{"remove_vpc_egress_ppps" => RemoveVpcEgressPpps}),
            NewAddVpcEgressSpps = ppps_to_spps_egress(NewAddVpcEgressPpps),
            RemoveVpcEgressSpps = ppps_to_spps_egress(RemoveVpcEgressPpps),
            SgDiff = #{
                <<"Add">> => NewAddVpcEgressSpps,
                <<"Remove">> => RemoveVpcEgressSpps
            },
            ?LOG_DEBUG(#{"sg_diff" => SgDiff}),
            SgDiff
    end.

-spec update_sg_egress(Ec2SecurityGroupId :: string(), Region :: string(), AddRemoveMap :: map()) ->
    {ok, tuple()} | {error, tuple()}.
update_sg_egress(Ec2SecurityGroupId, Region, AddRemoveMap) ->
    Ec2SecurityGroupIdList = binary:bin_to_list(Ec2SecurityGroupId),
    Config = config(Region),
    NewAddVpcIngressSpecs = maps:get(<<"Add">>, AddRemoveMap),
    ?LOG_DEBUG(#{"new_add_vpc_ingress_specs" => NewAddVpcIngressSpecs}),
    AddResults =
        case NewAddVpcIngressSpecs of
            [] ->
                [];
            _ ->
                parse_authorize_response(
                    erlcloud_ec2:authorize_security_group_egress(
                        Ec2SecurityGroupIdList, NewAddVpcIngressSpecs, Config
                    )
                )
        end,
    ?LOG_DEBUG(#{"add_results" => AddResults}),
    RemoveVpcIngressSpecs = maps:get(<<"Remove">>, AddRemoveMap),
    RemoveResults =
        case RemoveVpcIngressSpecs of
            [] ->
                [];
            _ ->
                parse_authorize_response(
                    erlcloud_ec2:revoke_security_group_egress(
                        Ec2SecurityGroupIdList, RemoveVpcIngressSpecs, Config
                    )
                )
        end,
    AllResults = [AddResults, RemoveResults],
    ?LOG_DEBUG(#{"all_results" => AllResults}),
    AllResultTrueFalse = lists:all(fun(X) -> (X == ok) or (X == []) end, AllResults),
    AllResult =
        case AllResultTrueFalse of
            true -> ok;
            false -> error
        end,
    {AllResult, egress, {{add_results, AddResults}, {remove_results, RemoveResults}}}.

-spec parse_authorize_response(AuthorizeResponse :: tuple()) -> ok | string().
parse_authorize_response(AuthorizeResponse) ->
    ?LOG_DEBUG(#{"authorize_response" => AuthorizeResponse}),
    case AuthorizeResponse of
        {error, {_ErrorType, _ErrorCode, _ErrorHeader, ErrorDescription}} ->
            Xml = element(2, (erlsom:simple_form(ErrorDescription))),
            {"Response", [], [
                {"Errors", [], [
                    {"Error", [], [
                        {"Code", [], [Code]},
                        {"Message", [], [Message]}
                    ]}
                ]},
                {"RequestID", [], [_RequestId]}
            ]} = Xml,
            ?LOG_ERROR(#{"message" => AuthorizeResponse}),
            case Code of
                %Ignore duplicate entry error
                "InvalidPermission.Duplicate" ->
                    ok;
                _ ->
                    {Code, Message}
            end;
        Other ->
            Other
    end.

ppps_to_spps_ingress(Ppps) ->
    Function = fun tuple_to_ingress_records/1,
    Accum = ppps_to_spps(Ppps, #{}),
    L = maps:to_list(Accum),
    lists:map(fun(E) -> Function(element(2, E)) end, L).

ppps_to_spps_egress(Ppps) ->
    Function = fun tuple_to_egress_records/1,
    Accum = ppps_to_spps(Ppps, #{}),
    L = maps:to_list(Accum),
    lists:map(fun(E) -> Function(element(2, E)) end, L).

ppps_to_spps([], Accum) ->
    Accum;
ppps_to_spps(Ppps, Accum) ->
    [Head | Rest] = Ppps,
    {Protocol, FromPort, ToPort, SourceDest} = Head,
    Key = {Protocol, FromPort, ToPort},
    Value = maps:get(Key, Accum, []),
    NewValue =
        case Value of
            [] ->
                case SourceDest of
                    {group_id, SgId} ->
                        [
                            {ip_protocol, Protocol},
                            {from_port, FromPort},
                            {to_port, ToPort},
                            {groups, [SgId]},
                            {ip_ranges, []}
                        ];
                    {cidr_ip, Cidr} ->
                        [
                            {ip_protocol, Protocol},
                            {from_port, FromPort},
                            {to_port, ToPort},
                            {groups, []},
                            {ip_ranges, [Cidr]}
                        ]
                end;
            _ ->
                case SourceDest of
                    {group_id, SgId} ->
                        ExistingIpRanges = proplists:get_value(ip_ranges, Value, []),
                        ExistingGroups = proplists:get_value(groups, Value, []),
                        NewGroups = ordsets:to_list(ordsets:from_list(ExistingGroups ++ [SgId])),
                        [
                            {ip_protocol, Protocol},
                            {from_port, FromPort},
                            {to_port, ToPort},
                            {groups, NewGroups},
                            {ip_ranges, ExistingIpRanges}
                        ];
                    {cidr_ip, Cidr} ->
                        ExistingGroups = proplists:get_value(groups, Value, []),
                        ExistingIpRanges = proplists:get_value(ip_ranges, Value, []),
                        NewIpRanges = ordsets:to_list(
                            ordsets:from_list(ExistingIpRanges ++ [Cidr])
                        ),
                        [
                            {ip_protocol, Protocol},
                            {from_port, FromPort},
                            {to_port, ToPort},
                            {groups, ExistingGroups},
                            {ip_ranges, NewIpRanges}
                        ]
                end
        end,
    AccumNew = maps:put(Key, NewValue, Accum),
    ppps_to_spps(Rest, AccumNew).

-spec ip_permissions_ingress(Ec2Region :: string(), Ec2SecurityGroupId :: string()) ->
    IpPermisions :: list().
ip_permissions_ingress(Ec2Region, Ec2SecurityGroupId) ->
    Config = config(Ec2Region),
    case erlcloud_ec2:describe_security_groups([Ec2SecurityGroupId], [], [], Config) of
        {ok, Permissions} ->
            IpPermissions = maps:get(ip_permissions, maps:from_list(hd(Permissions))),
            IpPermissionSpecs = [from_describe_tuple_to_ingress_records(T) || T <- IpPermissions],
            lists:flatten(IpPermissionSpecs);
        _ ->
            []
    end.

-spec ip_permissions_egress(Ec2Region :: string(), Ec2SecurityGroupId :: string()) ->
    IpPermisions :: list().
ip_permissions_egress(Ec2Region, Ec2SecurityGroupId) ->
    Config = config(Ec2Region),
    case erlcloud_ec2:describe_security_groups([Ec2SecurityGroupId], [], [], Config) of
        {ok, Permissions} ->
            IpPermissions = maps:get(ip_permissions_egress, maps:from_list(hd(Permissions))),
            IpPermissionSpecs = [from_describe_tuple_to_egress_records(T) || T <- IpPermissions],
            lists:flatten(IpPermissionSpecs);
        _ ->
            []
    end.

%Only creates ingress_specs for rules with ip_ranges, so doesn't create/delete rules with SGs as source
tuple_to_ingress_records(Keyvalpairs) ->
    IpRanges = proplists:get_value(ip_ranges, Keyvalpairs, []),
    Groups = proplists:get_value(groups, Keyvalpairs, []),
    Keyvalpairs1 = proplists:delete(groups, Keyvalpairs),
    Keyvalpairs2 = proplists:delete(ip_ranges, Keyvalpairs1),
    Keyvalpairs3 = Keyvalpairs2 ++ [{cidr_ip, IpRanges}],
    Keyvalpairs4 = Keyvalpairs3 ++ [{group_id, Groups}],
    Foorecord = list_to_tuple([
        vpc_ingress_spec
        | [
            proplists:get_value(X, Keyvalpairs4)
         || X <- record_info(fields, vpc_ingress_spec)
        ]
    ]),
    Foorecord.

tuple_to_egress_records(Keyvalpairs) ->
    IpRanges = proplists:get_value(ip_ranges, Keyvalpairs, []),
    Groups = proplists:get_value(groups, Keyvalpairs, []),
    Keyvalpairs1 = proplists:delete(groups, Keyvalpairs),
    Keyvalpairs2 = proplists:delete(ip_ranges, Keyvalpairs1),
    Keyvalpairs3 = Keyvalpairs2 ++ [{cidr_ip, IpRanges}],
    Keyvalpairs4 = Keyvalpairs3 ++ [{group_id, Groups}],
    Foorecord = list_to_tuple([
        vpc_egress_spec
        | [
            proplists:get_value(X, Keyvalpairs4)
         || X <- record_info(fields, vpc_egress_spec)
        ]
    ]),
    Foorecord.

ingress_records_to_ppps(IngressRecords) ->
    lists:flatten([ingress_record_to_ppps(Record) || Record <- IngressRecords]).

ingress_record_to_ppps(IpPermissionSpecs) ->
    Protocol = IpPermissionSpecs#vpc_ingress_spec.ip_protocol,
    FromPort = IpPermissionSpecs#vpc_ingress_spec.from_port,
    ToPort = IpPermissionSpecs#vpc_ingress_spec.to_port,
    GroupIds = IpPermissionSpecs#vpc_ingress_spec.group_id,
    CidrIps = IpPermissionSpecs#vpc_ingress_spec.cidr_ip,
    GroupIdsList = lists:map(
        fun(GroupId) ->
            {Protocol, FromPort, ToPort, {group_id, GroupId}}
        end,
        GroupIds
    ),
    CidrIpsList = lists:map(
        fun(CidrIp) ->
            {Protocol, FromPort, ToPort, {cidr_ip, CidrIp}}
        end,
        CidrIps
    ),
    lists:flatten(GroupIdsList ++ CidrIpsList).

egress_records_to_ppps(IngressRecords) ->
    lists:flatten([egress_record_to_ppps(Record) || Record <- IngressRecords]).

egress_record_to_ppps(IpPermissionSpecs) ->
    Protocol = IpPermissionSpecs#vpc_egress_spec.ip_protocol,
    FromPort = IpPermissionSpecs#vpc_egress_spec.from_port,
    ToPort = IpPermissionSpecs#vpc_egress_spec.to_port,
    GroupIds = IpPermissionSpecs#vpc_egress_spec.group_id,
    CidrIps = IpPermissionSpecs#vpc_egress_spec.cidr_ip,
    GroupIdsList = lists:map(
        fun(GroupId) ->
            {Protocol, FromPort, ToPort, {group_id, GroupId}}
        end,
        GroupIds
    ),
    CidrIpsList = lists:map(
        fun(CidrIp) ->
            {Protocol, FromPort, ToPort, {cidr_ip, CidrIp}}
        end,
        CidrIps
    ),
    lists:flatten(GroupIdsList ++ CidrIpsList).

%erlcoud describe_security_groups returns more info on group_id, must simplify to compare to add specs.
from_describe_tuple_to_ingress_records(Keyvalpairs) ->
    IpRanges = proplists:get_value(ip_ranges, Keyvalpairs),
    Groups = proplists:get_value(groups, Keyvalpairs),
    Keyvalpairs1 = proplists:delete(groups, Keyvalpairs),
    Keyvalpairs2 = proplists:delete(ip_ranges, Keyvalpairs1),
    Keyvalpairs3 = Keyvalpairs2 ++ [{cidr_ip, IpRanges}],
    GroupIds =
        case Groups of
            [] ->
                [];
            _ ->
                lists:map(
                    fun(Id) ->
                        proplists:get_value(group_id, Id)
                    end,
                    Groups
                )
        end,
    Keyvalpairs4 = Keyvalpairs3 ++ [{group_id, GroupIds}],
    Foorecord = list_to_tuple([
        vpc_ingress_spec
        | [
            proplists:get_value(X, Keyvalpairs4)
         || X <- record_info(fields, vpc_ingress_spec)
        ]
    ]),
    Foorecord.

from_describe_tuple_to_egress_records(Keyvalpairs) ->
    IpRanges = proplists:get_value(ip_ranges, Keyvalpairs),
    Groups = proplists:get_value(groups, Keyvalpairs),
    Keyvalpairs1 = proplists:delete(groups, Keyvalpairs),
    Keyvalpairs2 = proplists:delete(ip_ranges, Keyvalpairs1),
    Keyvalpairs3 = Keyvalpairs2 ++ [{cidr_ip, IpRanges}],
    GroupIds =
        case Groups of
            [] ->
                [];
            _ ->
                lists:map(
                    fun(Id) ->
                        proplists:get_value(group_id, Id)
                    end,
                    Groups
                )
        end,
    Keyvalpairs4 = Keyvalpairs3 ++ [{group_id, GroupIds}],
    Foorecord = list_to_tuple([
        vpc_egress_spec
        | [
            proplists:get_value(X, Keyvalpairs4)
         || X <- record_info(fields, vpc_egress_spec)
        ]
    ]),
    Foorecord.
