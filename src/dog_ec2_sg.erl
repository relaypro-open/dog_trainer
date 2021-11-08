-module(dog_ec2_sg).

-include("dog_trainer.hrl").
-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_ec2.hrl").

-export([
        %create_port_anywhere_ingress_rules/1,
        ip_permissions/2,
        publish_ec2_sg/1,
        update_sg/3
        ]).
%test
-export([
        %default_ingress_rules/1,
        tuple_to_ingress_records/1
        ]).

config(Region) ->
    {ok, Key} = application:get_env(dog_trainer, aws_key),
    {ok,Secret} = application:get_env(dog_trainer, aws_secret),
    Url = "ec2." ++ Region ++ ".amazonaws.com",
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
          cidr_ip= ["0.0.0.0/0"]
         },
		#vpc_ingress_spec{
          ip_protocol = tcp,
          from_port = 0,
          to_port = 65535, 
          group_id = [Ec2SecurityGroupId]
         },
		#vpc_ingress_spec{
          ip_protocol = udp,
          from_port = 0,
          to_port = 65535, 
          group_id = [Ec2SecurityGroupId]
         }
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

-spec publish_ec2_sg(DogGroupName :: string()) -> {ok|error,DetailedResults :: list()}.
publish_ec2_sg(DogGroupName) ->
    %{ok,DogGroupId} = dog_group:get_id_by_name(DogGroupName),
    AnywhereIngressRules = create_port_anywhere_ingress_rules(DogGroupName),
    lager:debug("AnywhereIngressRules: ~p~n",[AnywhereIngressRules]),
    Ec2SecurityGroupInfo = dog_group:get_ec2_security_group_ids(DogGroupName),
    Results = lists:map(fun(Group) ->
                      lager:info("DogGroup: ~p, SecurityGroup: ~p",[DogGroupName,Group]),
                      Result = {update_sg(
                            binary:bin_to_list(maps:get(<<"sgid">>,Group)),
                            binary:bin_to_list(maps:get(<<"region">>,Group)),
                            AnywhereIngressRules)},
                      case Result of
                          {{error,_}} ->
                              lager:error("Result: ~p",[Result]);
                          {{ok,_}} ->
                              lager:info("Result: ~p",[Result])
                      end, 
                      Result
                  end,Ec2SecurityGroupInfo),
    AllResultTrueFalse = lists:all(fun({{Result,_Details}}) -> Result == ok end, Results),
    AllResult = case AllResultTrueFalse of
        true -> ok;
        false -> error
    end,
    {AllResult,Results}.

create_port_anywhere_ingress_rules(DogGroupName) ->
    ProtocolPorts = dog_group:get_all_inbound_ports_by_protocol(DogGroupName),
    AnywhereIngressRules = lists:map(fun({Protocol, Ports}) ->
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
                                end, Ports)
              end, ProtocolPorts),
    lists:flatten(AnywhereIngressRules).

-spec parse_authorize_response(AuthorizeResponse :: tuple()) -> ok | string().
parse_authorize_response(AuthorizeResponse) ->
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


-spec update_sg(Ec2SecurityGroupId :: string(), Region :: string(), AnywhereIngressRules :: list()) -> {ok,tuple()} | {error, tuple()}.
update_sg(Ec2SecurityGroupId, Region, AnywhereIngressRules) ->
    ExistingRules = ip_permissions(Region, Ec2SecurityGroupId),
    AddVpcIngressSpecs = ordsets:to_list(ordsets:from_list(default_ingress_rules(Ec2SecurityGroupId) ++ AnywhereIngressRules)),
    lager:debug("AddVpcIngressSpecs: ~p~n",[AddVpcIngressSpecs]),
    NewAddVpcIngressSpecs = ordsets:subtract(ordsets:from_list(AddVpcIngressSpecs),ordsets:from_list(ExistingRules)), 
    lager:debug("NewAddVpcIngressSpecs: ~p~n",[NewAddVpcIngressSpecs]),
    Config = config(Region),
    lager:debug("~p~n",[erlcloud_ec2:describe_security_groups([Ec2SecurityGroupId],[],[],Config)]),
    Results = lists:map(fun(RuleSpec) ->
        AuthorizeResponse = erlcloud_ec2:authorize_security_group_ingress(Ec2SecurityGroupId, [RuleSpec], Config),
        lager:debug("AuthorizeResponse: ~p~n",[AuthorizeResponse]),
        parse_authorize_response(AuthorizeResponse)
    end, NewAddVpcIngressSpecs),
    RequestResults = lists:zip(NewAddVpcIngressSpecs,Results),
    lists:foreach(fun({Rule,Result}) ->
        lager:debug("~p~n~p~n",[Rule,Result])
    end, RequestResults),
    %TODO: Remove all rules not just added
    RemoveVpcIngressSpecs = ordsets:subtract(ordsets:from_list(ExistingRules),ordsets:from_list(AddVpcIngressSpecs)), 
    lager:debug("RemoveVpcIngressSpecs: ~p~n",[RemoveVpcIngressSpecs]),
    %RemoveVpcIngressSpecs = default_remove_ingress_rules(),
    RemoveResults = lists:map(fun(RuleSpec) ->
		AuthorizeResponse = erlcloud_ec2:revoke_security_group_ingress(Ec2SecurityGroupId, [RuleSpec], Config),
        parse_authorize_response(AuthorizeResponse)
    end, RemoveVpcIngressSpecs),
    RemoveRequestResults = lists:zip(RemoveVpcIngressSpecs,RemoveResults),
    lists:foreach(fun({Rule,Result}) ->
        lager:debug("~p~n~p~n",[Rule,Result])
    end, RemoveRequestResults),
    AllResultTrueFalse = lists:all(fun(X) -> lists:member(X,[ok,[]]) end, lists:flatten([Results ++ RemoveResults])),
    AllResult = case AllResultTrueFalse of
        true -> ok;
        false -> error
    end,
    {AllResult,{{add_results,Results},{remove_results,RemoveResults}}}.
    %lager:debug("~p~n",[erlcloud_ec2:describe_security_groups([Ec2SecurityGroupId],[],[],Config)]).

%-spec get_instance_id(InstanceName :: string()) -> InstanceId :: string().
%get_instance_id(InstanceName) ->
%    erlcloud_ec2:describe_instances().
%
%-spec get_server_ecg(ServerName :: string(), Region :: string() ) -> Ec2SecurityGroupId :: string().
%get_server_ecg(ServerName, Region) ->
%    Config = config(Region),
%    {ok, Response} = describe_instances(InstanceIDs::ec2_instances_ids(), Filter::filter_list(), Config::aws_config()) -> ok_error([proplist()])

-spec ip_permissions(Ec2Region :: string(), Ec2SecurityGroupId :: string()) -> IpPermisions :: list().
ip_permissions(Ec2Region, Ec2SecurityGroupId) ->
    Config = config(Ec2Region),
    {ok, Permissions} = erlcloud_ec2:describe_security_groups([Ec2SecurityGroupId],[],[],Config),
    %lager:debug("Permissions: ~p~n",[Permissions]),
    IpPermissions = maps:get(ip_permissions, maps:from_list(hd(Permissions))),
    %IpPermissions.
    IpPermissionSpecs = [tuple_to_ingress_records(T) || T <- IpPermissions],
    lists:flatten(IpPermissionSpecs).

tuple_to_ingress_records(Keyvalpairs) ->
    IpRanges = proplists:get_value(ip_ranges,Keyvalpairs),
    Keyvalpairs1 = proplists:delete(ip_ranges,Keyvalpairs),
    IngressRecords = lists:map(fun(IpRange) ->
        Keyvalpairs2 = Keyvalpairs1 ++ [{cidr_ip, [IpRange]}],
        Foorecord = list_to_tuple([vpc_ingress_spec|[proplists:get_value(X, Keyvalpairs2)
                                                    || X <- record_info(fields, vpc_ingress_spec)]]),
        Foorecord
              end, IpRanges),
    IngressRecords.
