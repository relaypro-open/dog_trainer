-module(dog_ec2_sg).

-include("dog_trainer.hrl").
-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_ec2.hrl").

-export([
        ip_permissions/2,
        publish_ec2_sg_by_id/1,
        publish_ec2_sg_by_name/1,
        publish_ec2_sg/1,
        publish_ec2_sgs/1,
        update_sg/3
        ]).
%test
-export([
        config/1,
        compare_sg/3,
        create_ingress_rules/1,
        create_ingress_port_rules/4,
        create_port_anywhere_ingress_rules/1,
        create_port_anywhere_ingress_rules_by_id/1,
        ingress_record_to_ppps/1,
        ingress_records_to_ppps/1,
        ppps_to_spps/1,
        tuple_to_ingress_records/1
        ]).

-spec config(Region :: binary()) -> tuple().
config(Region) ->
    {ok, Key} = application:get_env(dog_trainer, aws_key),
    {ok,Secret} = application:get_env(dog_trainer, aws_secret),
    Url = "ec2." ++ binary:bin_to_list(Region) ++ ".amazonaws.com",
    lager:debug("Url: ~s~n",[Url]),
    erlcloud_ec2:new(Key,
                     Secret, Url).

default_spps_rules(Ec2SecurityGroupId) ->
    [
    {icmp,8,8,{cidr_ip,"0.0.0.0/0"}},
    {tcp,0,65535,{group_id,binary:bin_to_list(Ec2SecurityGroupId)}},
    {udp,0,65535,{group_id,binary:bin_to_list(Ec2SecurityGroupId)}}
     ].

publish_ec2_sg_by_name(DogGroupName) ->
    UpdateEc2SgResults = case dog_group:get_by_name(DogGroupName) of
        {ok,DogGroup} ->
            publish_ec2_sgs(DogGroup);
        _ ->
            []
    end,
    lager:info("UpdateEc2SgResults: ~p~n",[UpdateEc2SgResults]),
    UpdateEc2SgResults.

-spec publish_ec2_sg_by_id(DogGroupId :: string()) -> {ok|error,DetailedResults :: list()}.
publish_ec2_sg_by_id(DogGroupId) ->
    case dog_group:get_by_id(DogGroupId) of
        {ok,DogGroup} ->
            publish_ec2_sgs(DogGroup);
        _ ->
            []
    end.
   
-spec publish_ec2_sgs(DogGroup :: map()) -> {ok|error,DetailedResults :: list()}.
publish_ec2_sgs(DogGroup) ->
    Ec2SecurityGroupList = maps:get(<<"ec2_security_group_ids">>,DogGroup,[]),
    DogGroupName = maps:get(<<"name">>,DogGroup),
    Results = plists:map(fun(Ec2Sg) ->
                      Region = maps:get(<<"region">>,Ec2Sg),
                      SgId = maps:get(<<"sgid">>,Ec2Sg),
                      lager:debug("Region, SgId: ~p, ~p~n",[Region,SgId]),
                      Ec2SgIds = dog_ec2_update_agent:ec2_security_group_ids(Region),
                      case lists:member(binary:bin_to_list(SgId),Ec2SgIds) of
                          true ->
                              {DogGroupName,Region,SgId,dog_ec2_sg:publish_ec2_sg({DogGroup, Region, SgId})};
                          false ->
                              {DogGroupName,Region,SgId,{error,<<"ec2 security group not found">>}}
                      end
              end, Ec2SecurityGroupList),
    Results.

-spec publish_ec2_sg({DogGroup :: map(), Region :: string(), SgId :: string()} ) -> {ok|error,DetailedResults :: list()}.
publish_ec2_sg({DogGroup, Region, SgId}) ->
            DogGroupId = maps:get(<<"id">>,DogGroup),
            AddRemoveMap = compare_sg(SgId, Region, DogGroupId),
            Results = {update_sg(
                  SgId,
                  Region,
                  AddRemoveMap
                  )},
            lager:debug("Results: ~p~n",[Results]),
            Results.

compare_sg(Ec2SecurityGroupId, Region, DogGroupId) ->
    {ok,DogGroup} = dog_group:get_by_id(DogGroupId),
    Ppps = dog_group:get_ppps_inbound_ec2(DogGroup,Region),
    DefaultPpps = default_spps_rules(Ec2SecurityGroupId),
    IngressRulesPpps = Ppps ++ DefaultPpps,
    IngressRulesSpps = ppps_to_spps(IngressRulesPpps),
    lager:debug("IngressRulesSpecs: ~p~n",[IngressRulesSpps]),
    case dog_ec2_update_agent:ec2_security_group(Ec2SecurityGroupId,Region) of
        {error,Reason} ->
            lager:error("Ec2SecurityGroupId doesn't exist: ~p~n",[Ec2SecurityGroupId]),
            {error,Reason};
        _ ->
            ExistingRulesSpps = ip_permissions(Region, Ec2SecurityGroupId),
            lager:debug("ExistingRulesSpps: ~p~n",[ExistingRulesSpps]),
            ExistingRulesPpps = ingress_records_to_ppps(ExistingRulesSpps),
            NewAddVpcIngressPpps = ordsets:subtract(ordsets:from_list(IngressRulesPpps),ordsets:from_list(ExistingRulesPpps)), 
            lager:debug("ExistingRulesPpps: ~p~n",[ExistingRulesPpps]),
            RemoveVpcIngressPpps = ordsets:subtract(ordsets:from_list(ExistingRulesPpps),ordsets:from_list(IngressRulesPpps)), 
            lager:debug("NewAddVpcIngressPpps: ~p~n",[NewAddVpcIngressPpps]),
            lager:debug("RemoveVpcIngressPpps: ~p~n",[RemoveVpcIngressPpps]),
            NewAddVpcIngressSpps = ppps_to_spps(NewAddVpcIngressPpps),
            RemoveVpcIngressSpps = ppps_to_spps(RemoveVpcIngressPpps),
            %RemoveVpcIngressSpps = ordsets:subtract(ordsets:from_list(ExistingRulesSpps),ordsets:from_list(IngressRulesSpps)), 
            %NewAddVpcIngressSpps = ordsets:subtract(ordsets:from_list(IngressRulesSpps),ordsets:from_list(ExistingRulesSpps)), 
            %RemoveVpcIngressSpps = ordsets:subtract(ordsets:from_list(ExistingRulesSpps),ordsets:from_list(IngressRulesSpps)), 
            SgCompare = #{<<"Add">> => NewAddVpcIngressSpps, 
              <<"Remove">> => RemoveVpcIngressSpps},
            lager:debug("SgCompare: ~p~n",[SgCompare]),
            SgCompare
    end.

-spec update_sg(Ec2SecurityGroupId :: string(), Region :: string(), AddRemoveMap :: map()) -> {ok,tuple()} | {error, tuple()}.
update_sg(Ec2SecurityGroupId, Region, AddRemoveMap) ->
    Ec2SecurityGroupIdList = binary:bin_to_list(Ec2SecurityGroupId),
    Config = config(Region),
    NewAddVpcIngressSpecs = maps:get(<<"Add">>,AddRemoveMap),
    lager:debug("NewAddVpcIngressSpecs: ~p~n",[NewAddVpcIngressSpecs]),
    AddResults = case NewAddVpcIngressSpecs of
                  [] ->
                      [];
                  _ ->
                    parse_authorize_response(erlcloud_ec2:authorize_security_group_ingress(Ec2SecurityGroupIdList, NewAddVpcIngressSpecs, Config))
              end,
    lager:debug("~p~n",[AddResults]),
    RemoveVpcIngressSpecs = maps:get(<<"Remove">>,AddRemoveMap),
    RemoveResults = case RemoveVpcIngressSpecs of
                  [] ->
                      [];
                  _ ->
                    parse_authorize_response(erlcloud_ec2:revoke_security_group_ingress(Ec2SecurityGroupIdList, RemoveVpcIngressSpecs, Config))
              end,
    AllResults = [AddResults,RemoveResults],
    lager:debug("AllResults: ~p~n",[AllResults]),
    AllResultTrueFalse = lists:all(fun(X) -> (X == ok) or (X == []) end, AllResults),
    AllResult = case AllResultTrueFalse of
        true -> ok;
        false -> error
    end,
    {AllResult,{{add_results,AddResults},{remove_results,RemoveResults}}}.

-spec parse_authorize_response(AuthorizeResponse :: tuple()) -> ok | string().
parse_authorize_response(AuthorizeResponse) ->
    lager:debug("AuthorizeResponse: ~p~n",[AuthorizeResponse]),
		case AuthorizeResponse of
            {error,{_ErrorType,_ErrorCode,_ErrorHeader,ErrorDescription}} ->
			%{error,Error} ->
                %{error,{http_error,400,"Bad Request",<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Response><Errors><Error><Code>InvalidPermission.Duplicate</Code><Message>the specified rule \"peer: sg-0d741a6be4fa9691d, UDP, from port: 0, to port: 65535, ALLOW\" already exists</Message></Error></Errors><RequestID>3cbe6e0d-179d-4481-8589-34eea28bfc65</RequestID></Response>">>}}
                %{error,{http_error,503,"Service Unavailable",<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Response><Errors><Error><Code>Unavailable</Code><Message>The service is unavailable. Please try again shortly.</Message></Error></Errors><RequestID>c7413e5f-800a-4bdc-8f96-24c69ea1ad9e</RequestID></Response>">>}}}
                Xml = element(2,(erlsom:simple_form(ErrorDescription))),
                %{"Response",[],
                % [{"Errors",[],
                %    [{"Error",[],
                %         [{"Code",[],["InvalidPermission.Duplicate"]},
                %               {"Message",[],
                %                      ["the specified rule \"peer: sg-0d741  a6be4fa9691d, UDP, from port: 0, to port: 65535, ALLOW\" already exists"]}]}]},
                %                        {"RequestID",[],["3cbe6e0d-179d-4481-8589-34eea28bfc65"]}]}     
                {"Response",[],
                 [{"Errors",[],
                   [{"Error",[],
                     [{"Code",[],[Code]},
                      {"Message",[],
                       [Message]}]}]},
                  {"RequestID",[],[_RequestId]}]} = Xml,
               case Code of
                   %Ignore duplicate entry error
                   "InvalidPermission.Duplicate" -> 
                       ok;
                   _ ->
                       {Code,Message}
               end;
			Other ->
				Other
        end.

create_port_anywhere_ingress_rule(Protocol, Ports) ->
    lists:map(fun(Port) ->
                      {From,To} = case string:split(Port,":") of
                                      [F,T] ->
                                          {F,T};
                                      [F] ->
                                          {F,F}
                                  end,
                      #vpc_ingress_spec{
                         ip_protocol = binary_to_atom(Protocol),
                         from_port = binary_to_integer(From),
                         to_port = binary_to_integer(To),
                         cidr_ip= ["0.0.0.0/0"]
                        }
              end, Ports).

create_port_anywhere_ingress_rules(DogGroupName) ->
    ProtocolPorts = dog_group:get_all_inbound_ports_by_protocol(DogGroupName),
    AnywhereIngressRules = lists:map(fun({Protocol, Ports}) ->
                                             create_port_anywhere_ingress_rule(Protocol,Ports)
              end, ProtocolPorts),
    lists:flatten(AnywhereIngressRules).

create_port_anywhere_ingress_rules_by_id(DogGroupId) ->
    {ok, DogGroup} = dog_group:get_by_id(DogGroupId),
    DogGroupName = maps:get(<<"name">>,DogGroup),
    create_port_anywhere_ingress_rules(DogGroupName).

-spec create_ingress_rules(Spps :: list() ) -> Rules :: list().
create_ingress_rules(Spps) ->
    Rules = lists:map(fun({{SourceType,Source},Protocol,Ports}) ->
                              create_ingress_port_rules(SourceType,Source,Protocol,Ports)
                      end, Spps),
    lists:flatten(Rules).
                      
-spec create_ingress_port_rules(SourceType :: string(),Source :: string(),Protocol :: string(), Ports :: list()) -> list().
create_ingress_port_rules(SourceType,Source,Protocol,Ports) ->
    PortRule = lists:map(fun(Port) ->
                                 {From,To} = case string:split(Port,":") of
                                                 [F,T] ->
                                                     {F,T};
                                                 [F] ->
                                                     {F,F}
                                             end,
                                 case SourceType of
                                     cidr_ip ->
                                         #vpc_ingress_spec{
                                            ip_protocol = binary_to_atom(Protocol),
                                            from_port = binary_to_integer(From),
                                            to_port = binary_to_integer(To),
                                            cidr_ip = [Source]
                                           };
                                     group_id ->
                                         #vpc_ingress_spec{
                                            ip_protocol = binary_to_atom(Protocol),
                                            from_port = binary_to_integer(From),
                                            to_port = binary_to_integer(To),
                                            group_id = [binary_to_list(Source)]
                                           }
                                 end
                         end, Ports),
    lists:flatten(PortRule).

ppps_to_spps(Ppps) ->
    ppps_to_spps(Ppps,#{}).

ppps_to_spps([],Accum) ->
    L = maps:to_list(Accum),
    lists:map(fun(E) -> tuple_to_ingress_records(element(2,E)) end, L);
ppps_to_spps(Ppps,Accum) ->
    [Head|Rest] = Ppps,
    {Protocol,FromPort,ToPort,Source} = Head,
    Key = {Protocol,FromPort,ToPort},
    Value = maps:get(Key,Accum,[]),
    NewValue = case Value of
        [] ->
            case Source of
                {group_id,SgId} ->
                    [
                     {ip_protocol,Protocol},
                     {from_port,FromPort},
                     {to_port,ToPort},
                     {groups,[SgId]},
                     {ip_ranges,[]}
                    ];
                {cidr_ip,Cidr} ->
                    [
                     {ip_protocol,Protocol},
                     {from_port,FromPort},
                     {to_port,ToPort},
                     {groups,[]},
                     {ip_ranges,[Cidr]}
                    ]
            end;
        _ ->
            case Source of
                {group_id,SgId} ->
                    ExistingIpRanges = proplists:get_value(ip_ranges,Value,[]),
                    ExistingGroups = proplists:get_value(groups,Value,[]),
                    NewGroups = ordsets:to_list(ordsets:from_list(ExistingGroups ++ [SgId])),
                    [
                     {ip_protocol,Protocol},
                     {from_port,FromPort},
                     {to_port,ToPort},
                     {groups,NewGroups},
                     {ip_ranges,ExistingIpRanges}
                    ];
                {cidr_ip,Cidr} ->
                    ExistingGroups = proplists:get_value(groups,Value,[]),
                    ExistingIpRanges = proplists:get_value(ip_ranges,Value,[]),
                    NewIpRanges = ordsets:to_list(ordsets:from_list(ExistingIpRanges ++ [Cidr])),
                    [
                     {ip_protocol,Protocol},
                     {from_port,FromPort},
                     {to_port,ToPort},
                     {groups,ExistingGroups},
                     {ip_ranges,NewIpRanges}
                    ]
            end
               end,
    AccumNew = maps:put(Key,NewValue,Accum),
    ppps_to_spps(Rest,AccumNew).

%-record(vpc_ingress_spec, {
%          ip_protocol::tcp|udp|icmp,
%          from_port::-1 | 0..65535,
%          to_port::-1 | 0..65535,
%          user_id::undefined|[string()],
%          group_name::undefined|[string()],
%          group_id::undefined|[string()],
%          cidr_ip::undefined|[string()]
%         }).

-spec ip_permissions(Ec2Region :: string(), Ec2SecurityGroupId :: string()) -> IpPermisions :: list().
ip_permissions(Ec2Region, Ec2SecurityGroupId) ->
    Config = config(Ec2Region),
    case erlcloud_ec2:describe_security_groups([Ec2SecurityGroupId],[],[],Config) of
        {ok, Permissions} ->
            IpPermissions = maps:get(ip_permissions, maps:from_list(hd(Permissions))),
            IpPermissionSpecs = [from_describe_tuple_to_ingress_records(T) || T <- IpPermissions],
            lists:flatten(IpPermissionSpecs);
        _ ->
            []
    end.

%Only creates ingress_specs for rules with ip_ranges, so doesn't create/delete rules with SGs as source
tuple_to_ingress_records(Keyvalpairs) ->
    IpRanges = proplists:get_value(ip_ranges,Keyvalpairs,[]),
    Groups = proplists:get_value(groups, Keyvalpairs,[]),
    Keyvalpairs1 = proplists:delete(groups,Keyvalpairs),
    Keyvalpairs2 = proplists:delete(ip_ranges,Keyvalpairs1),
    Keyvalpairs3 = Keyvalpairs2 ++ [{cidr_ip,IpRanges}],
    Keyvalpairs4 = Keyvalpairs3 ++ [{group_id,Groups}],
    Foorecord = list_to_tuple([vpc_ingress_spec|[proplists:get_value(X, Keyvalpairs4)
                                                            || X <- record_info(fields, vpc_ingress_spec)]]),
    Foorecord.

%-record(vpc_ingress_spec, {
%          ip_protocol::tcp|udp|icmp,
%          from_port::-1 | 0..65535,
%          to_port::-1 | 0..65535,
%          user_id::undefined|[string()],
%          group_name::undefined|[string()],
%          group_id::undefined|[string()],
%          cidr_ip::undefined|[string()]
%         }).

ingress_records_to_ppps(IngressRecords) ->
   lists:flatten([ingress_record_to_ppps(Record) || Record <- IngressRecords]).

ingress_record_to_ppps(IpPermissionSpecs) ->
    Protocol = IpPermissionSpecs#vpc_ingress_spec.ip_protocol,
    FromPort = IpPermissionSpecs#vpc_ingress_spec.from_port,
    ToPort = IpPermissionSpecs#vpc_ingress_spec.to_port,
    GroupIds = IpPermissionSpecs#vpc_ingress_spec.group_id,
    CidrIps = IpPermissionSpecs#vpc_ingress_spec.cidr_ip,
    GroupIdsList = lists:map(fun(GroupId) ->
                       {Protocol,FromPort,ToPort,{group_id, GroupId}}
              end, GroupIds),
    CidrIpsList = lists:map(fun(CidrIp) ->
                       {Protocol,FromPort,ToPort,{cidr_ip, CidrIp}}
              end, CidrIps),
    lists:flatten(GroupIdsList ++ CidrIpsList).
                       
%erlcoud describe_security_groups returns more info on group_id, must simplify to compare to add specs.
from_describe_tuple_to_ingress_records(Keyvalpairs) ->
    IpRanges = proplists:get_value(ip_ranges,Keyvalpairs),
    Groups = proplists:get_value(groups, Keyvalpairs),
    Keyvalpairs1 = proplists:delete(groups,Keyvalpairs),
    Keyvalpairs2 = proplists:delete(ip_ranges,Keyvalpairs1),
    Keyvalpairs3 = Keyvalpairs2 ++ [{cidr_ip,IpRanges}],
    GroupIds = case Groups of
                   [] ->
                       [];
                   _ ->
                       lists:map(fun(Id) ->
                                         proplists:get_value(group_id,Id)
                                 end,Groups)
               end,
    Keyvalpairs4 = Keyvalpairs3 ++ [{group_id,GroupIds}],
    Foorecord = list_to_tuple([vpc_ingress_spec|[proplists:get_value(X, Keyvalpairs4)
                                                            || X <- record_info(fields, vpc_ingress_spec)]]),
    Foorecord.
