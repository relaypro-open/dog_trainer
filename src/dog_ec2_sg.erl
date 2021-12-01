-module(dog_ec2_sg).

-include("dog_trainer.hrl").
-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_ec2.hrl").

-export([
        ip_permissions/2,
        publish_ec2_sg_by_id/1,
        publish_ec2_sg_by_name/1,
        publish_ec2_sg/3,
        publish_ec2_sgs/1,
        update_sg/3
        ]).
%test
-export([
        %default_ingress_rules/1,
        config/1,
        compare_sg/3,
        create_ingress_rules/1,
        create_ingress_port_rules/4,
        create_port_anywhere_ingress_rules/1,
        create_port_anywhere_ingress_rules_by_id/1,
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

%-record(vpc_ingress_spec, {
%          ip_protocol::tcp|udp|icmp,
%          from_port::-1 | 0..65535,
%          to_port::-1 | 0..65535,
%          user_id::undefined|[string()],
%          group_name::undefined|[string()],
%          group_id::undefined|[string()],
%          cidr_ip::undefined|[string()]
%         }).

default_ingress_rules(Ec2SecurityGroupId) ->	
    [
		#vpc_ingress_spec{
          ip_protocol = icmp,
          from_port = 8,
          to_port = -1,
          cidr_ip= ["0.0.0.0/0"],
          group_id = []
         },
		#vpc_ingress_spec{
          ip_protocol = tcp,
          from_port = 0,
          to_port = 65535, 
          group_id = [binary:bin_to_list(Ec2SecurityGroupId)],
          cidr_ip = []
         },
		#vpc_ingress_spec{
          ip_protocol = udp,
          from_port = 0,
          to_port = 65535, 
          group_id = [binary:bin_to_list(Ec2SecurityGroupId)],
          cidr_ip = []
         }
    ].

default_spps_rules(Ec2SecurityGroupId) ->
    [
    {icmp,8,8,{cidr_ip,"0.0.0.0/0"}},
    {tcp,0,65535,{group_id,binary:bin_to_list(Ec2SecurityGroupId)}},
    {udp,0,65535,{group_id,binary:bin_to_list(Ec2SecurityGroupId)}}
     ].


%default_remove_ingress_rules() ->	
%    [
%		#vpc_ingress_spec{
%          ip_protocol = tcp,
%          from_port = 0,
%          to_port = 65535, 
%          cidr_ip= ["0.0.0.0/0"]
%         },
%		#vpc_ingress_spec{
%          ip_protocol = udp,
%          from_port = 0,
%          to_port = 65535, 
%          cidr_ip= ["0.0.0.0/0"]
%         }
%    ].
%
%TODO: Trigger when dependent Groups change ec2_instances_ids
publish_ec2_sg_by_name(DogGroupName) ->
    case dog_group:get_by_name(DogGroupName) of
        {ok,DogGroup} ->
            publish_ec2_sgs(DogGroup);
        _ ->
            []
    end.

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
    Results = lists:map(fun(Ec2Sg) ->
                      Region = maps:get(<<"region">>,Ec2Sg),
                      SgId = maps:get(<<"sgid">>,Ec2Sg),
                      {DogGroupName,Region,SgId,publish_ec2_sg(DogGroup, Region, SgId)}
              end, Ec2SecurityGroupList),
    AllResultTrueFalse = lists:all(fun({_DogGroupName,_Region,_SgId,{{Result,_Details}}}) -> Result == ok end, Results),
    AllResult = case AllResultTrueFalse of
        true -> ok;
        false -> error
    end,
    {AllResult,Results}.


-spec publish_ec2_sg(DogGroup :: map(), Region :: string(), SgId :: string() ) -> {ok|error,DetailedResults :: list()}.
publish_ec2_sg(DogGroup, Region, SgId) ->
            DogGroupId = maps:get(<<"id">>,DogGroup),
            AddRemoveMap = compare_sg(SgId, Region, DogGroupId),
            Results = {update_sg(
                  SgId,
                  Region,
                  AddRemoveMap
                  )},
            lager:debug("Results: ~p~n",[Results]),
            Results.

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

-spec parse_authorize_response(AuthorizeResponse :: tuple()) -> ok | string().
parse_authorize_response(AuthorizeResponse) ->
    lager:debug("AuthorizeResponse: ~p~n",[AuthorizeResponse]),
		case AuthorizeResponse of
            {error,{_ErrorType,_ErrorCode,_ErrorHeader,ErrorDescription}} ->
			%{error,Error} ->
                %{error,{http_error,400,"Bad Request",<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Response><Errors><Error><Code>InvalidPermission.Duplicate</Code><Message>the specified rule \"peer: sg-0d741a6be4fa9691d, UDP, from port: 0, to port: 65535, ALLOW\" already exists</Message></Error></Errors><RequestID>3cbe6e0d-179d-4481-8589-34eea28bfc65</RequestID></Response>">>}}
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
                       [_Message]}]}]},
                  {"RequestID",[],[_RequestId]}]} = Xml,
               case Code of
                   %Ignore duplicate entry error
                   "InvalidPermission.Duplicate" -> 
                       ok;
                   _ ->
                       Code
               end;
			Other ->
				Other
        end.
        
create_ingress_rule(Ec2SecurityGroupId, RuleSpec, Config) ->
    Ec2SecurityGroupIdList = binary:bin_to_list(Ec2SecurityGroupId),
        AuthorizeResponse = erlcloud_ec2:authorize_security_group_ingress(Ec2SecurityGroupIdList, [RuleSpec], Config),
        lager:debug("AuthorizeResponse: ~p~n",[AuthorizeResponse]),
        AnywhereRuleSpec = #vpc_ingress_spec{
           ip_protocol = RuleSpec#vpc_ingress_spec.ip_protocol,
           from_port = RuleSpec#vpc_ingress_spec.from_port,
           to_port = RuleSpec#vpc_ingress_spec.to_port,
           cidr_ip= ["0.0.0.0/0"]
           },
        case parse_authorize_response(AuthorizeResponse) of
            "InvalidGroup.NotFound" ->
                lager:debug("AnywhereRuleSpec: ~p~n",[AnywhereRuleSpec]),
                AnywhereRuleResponse = erlcloud_ec2:authorize_security_group_ingress(Ec2SecurityGroupIdList, [AnywhereRuleSpec], Config),
                parse_authorize_response(AnywhereRuleResponse);
            Response ->
                %erlcloud_ec2:revoke_security_group_ingress(Ec2SecurityGroupIdList, [AnywhereRuleSpec], Config),
                Response
        end.

compare_sg(Ec2SecurityGroupId, Region, DogGroupId) ->
    Config = config(Region),
    {ok,DogGroup} = dog_group:get_by_id(DogGroupId),
    Ppps = dog_group:get_ppps_inbound_ec2(DogGroup,Region),
    DefaultPpps = default_spps_rules(Ec2SecurityGroupId),
    IngressRulesSpecs = ppps_to_spps(Ppps ++ DefaultPpps),
    %IngressRulesSpecs = create_ingress_rules(Spps),
    lager:debug("IngressRulesSpecs: ~p~n",[IngressRulesSpecs]),
    case erlcloud_ec2:describe_security_groups([Ec2SecurityGroupId],[],[],Config) of
        {error,Reason} ->
            lager:error("Ec2SecurityGroupId doesn't exist: ~p~n",[Ec2SecurityGroupId]),
            {error,Reason};
        _ ->
            ExistingRules = ip_permissions(Region, Ec2SecurityGroupId),
            lager:debug("ExistingRules: ~p~n",[ExistingRules]),
            %DefaultIngressRules = default_ingress_rules(Ec2SecurityGroupId),
            %lager:debug("DefaultIngressRules: ~p~n",[DefaultIngressRules]),
            %AddVpcIngressSpecs = ordsets:to_list(
            %                       ordsets:union(
            %                         ordsets:from_list(DefaultIngressRules),
            %                         ordsets:from_list(IngressRulesSpecs) 
            %                        )
            %                      ),
            %lager:debug("AddVpcIngressSpecs: ~p~n",[AddVpcIngressSpecs]),
            NewAddVpcIngressSpecs = ordsets:subtract(ordsets:from_list(IngressRulesSpecs),ordsets:from_list(ExistingRules)), 
            lager:debug("NewAddVpcIngressSpecs: ~p~n",[NewAddVpcIngressSpecs]),
            RemoveVpcIngressSpecs = ordsets:subtract(ordsets:from_list(ExistingRules),ordsets:from_list(IngressRulesSpecs)), 
            lager:debug("RemoveVpcIngressSpecs: ~p~n",[RemoveVpcIngressSpecs]),
            #{<<"Add">> => NewAddVpcIngressSpecs, 
              <<"Remove">> => RemoveVpcIngressSpecs}
    end.

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

%spps_to_specs(Spps) ->
%    lists:map(fun(Proplist) ->
%                    NewProplist = [
%                     {ip_protocol, proplists:get_value(ip_protocol,Proplist)},
%                     {from_port, proplists:get_value(from_port,Proplist)},
%                     {to_port,proplists:get_value(to_port,Proplist)}
%                    ],
%                      Groups = proplists:get_value(groups,Proplist,[]),
%                      lists:map(fun(Group) ->
%
%                                end,Groups),
%                      IpRanges = proplists:get_value(ip_ranges,Proplist,[]),
%                      lists:map(fun(IpRange) ->
%                                end,IpRanges)
%              end,Spps).


-spec update_sg(Ec2SecurityGroupId :: string(), Region :: string(), AddRemoveMap :: map()) -> {ok,tuple()} | {error, tuple()}.
update_sg(Ec2SecurityGroupId, Region, AddRemoveMap) ->
    Ec2SecurityGroupIdList = binary:bin_to_list(Ec2SecurityGroupId),
    Config = config(Region),
    case erlcloud_ec2:describe_security_groups([Ec2SecurityGroupIdList],[],[],Config) of
        {error,Reason} ->
            lager:error("Ec2SecurityGroupId doesn't exist: ~p~n",[Ec2SecurityGroupId]),
            {error,Reason};
        _ ->
            lager:debug("~p~n",[erlcloud_ec2:describe_security_groups([Ec2SecurityGroupIdList],[],[],Config)]),
            NewAddVpcIngressSpecs = maps:get(<<"Add">>,AddRemoveMap),
            Results = lists:map(fun(RuleSpec) ->
                create_ingress_rule(Ec2SecurityGroupId,RuleSpec, Config)
            end, NewAddVpcIngressSpecs),
            RequestResults = lists:zip(NewAddVpcIngressSpecs,Results),
            lists:foreach(fun({Rule,Result}) ->
                lager:debug("~p~n~p~n",[Rule,Result])
            end, RequestResults),
            RemoveVpcIngressSpecs = maps:get(<<"Remove">>,AddRemoveMap),

            RemoveResults = lists:map(fun(RuleSpec) ->
                AuthorizeResponse = erlcloud_ec2:revoke_security_group_ingress(Ec2SecurityGroupIdList, [RuleSpec], Config),
                parse_authorize_response(AuthorizeResponse)
            end, RemoveVpcIngressSpecs),
            AllResults = lists:flatten([Results ++ RemoveResults]),
            lager:debug("AllResults: ~p~n",[AllResults]),
            AllResultTrueFalse = lists:all(fun(X) -> X == ok end, AllResults),
            AllResult = case AllResultTrueFalse of
                true -> ok;
                false -> error
            end,
            {AllResult,{{add_results,Results},{remove_results,RemoveResults}}}
    end.

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
