-module(dog_ec2_sg).

-include("dog_trainer.hrl").
-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_ec2.hrl").

-export([
        create_port_anywhere_ingress_rules/1,
        ip_permissions/2,
        publish_ec2_sg/1,
        update_sg/3
        ]).
%test
-export([
        default_ingress_rules/1,
        tuple_to_ingress_records/1
        ]).

config(Region) ->
    {ok, Key} = application:get_env(dog_trainer, aws_key),
    {ok,Secret} = application:get_env(dog_trainer, aws_secret),
    Url = "ec2." ++ Region ++ ".amazonaws.com",
    io:format("Url: ~s~n",[Url]),
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

default_remove_ingress_rules() ->	
    [
		#vpc_ingress_spec{
          ip_protocol = tcp,
          from_port = 0,
          to_port = 65535, 
          cidr_ip= ["0.0.0.0/0"]
         },
		#vpc_ingress_spec{
          ip_protocol = udp,
          from_port = 0,
          to_port = 65535, 
          cidr_ip= ["0.0.0.0/0"]
         }
    ].

publish_ec2_sg(DogGroupName) ->
    %{ok,DogGroupId} = dog_group:get_id_by_name(DogGroupName),
    AnywhereIngressRules = create_port_anywhere_ingress_rules(DogGroupName),
    io:format("AnywhereIngressRules: ~p~n",[AnywhereIngressRules]),
    Ec2SecurityGroupInfo = dog_group:get_ec2_security_group_ids(DogGroupName),
    lists:map(fun({Ec2Region, Ec2SecurityGroupId}) ->
                          update_sg(
                            binary:bin_to_list(Ec2SecurityGroupId),
                            Ec2Region,
                            AnywhereIngressRules)
                  end,Ec2SecurityGroupInfo).

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

-spec update_sg(Ec2SecurityGroupId :: string(), Region :: string(), AnywhereIngressRules :: list()) -> ok | error.
update_sg(Ec2SecurityGroupId, Region, AnywhereIngressRules) ->
    AddVpcIngressSpecs = default_ingress_rules(Ec2SecurityGroupId) ++ AnywhereIngressRules,
    io:format("AddVpcIngressSpecs: ~p~n",[AddVpcIngressSpecs]),
    Config = config(Region),
    io:format("~p~n",[erlcloud_ec2:describe_security_groups([Ec2SecurityGroupId],[],[],Config)]),
    Results = lists:map(fun(RuleSpec) ->
        AuthorizeResponse = erlcloud_ec2:authorize_security_group_ingress(Ec2SecurityGroupId, [RuleSpec], Config),
        io:format("AuthorizeResponse: ~p~n",[AuthorizeResponse]),
		case AuthorizeResponse of
			{error,{_ErrorDescription,_ErrorCode,_ShortDescription,Description}} ->
				{error,erlsom:simple_form(Description)};
			Other ->
				Other
        end
    end, AddVpcIngressSpecs),
    RequestResults = lists:zip(AddVpcIngressSpecs,Results),
    lists:foreach(fun({Rule,Result}) ->
        io:format("~p~n~p~n",[Rule,Result])
    end, RequestResults),
    %TODO: Remove all rules not just added
    ExistingRules = ip_permissions(Region, Ec2SecurityGroupId),
    RemoveVpcIngressSpecs = ordsets:subtract(ordsets:from_list(ExistingRules),ordsets:from_list(AddVpcIngressSpecs)), 
    io:format("RemoveVpcIngressSpecs: ~p~n",[RemoveVpcIngressSpecs]),
    %RemoveVpcIngressSpecs = default_remove_ingress_rules(),
    RemoveResults = lists:map(fun(RuleSpec) ->
		case erlcloud_ec2:revoke_security_group_ingress(Ec2SecurityGroupId, [RuleSpec], Config) of
			{error,{_ErrorDescription,_ErrorCode,_ShortDescription,Description}} ->
				{error,erlsom:simple_form(Description)};
			Other ->
				Other
        end
    end, RemoveVpcIngressSpecs),
    RemoveRequestResults = lists:zip(RemoveVpcIngressSpecs,RemoveResults),
    lists:foreach(fun({Rule,Result}) ->
        io:format("~p~n~p~n",[Rule,Result])
    end, RemoveRequestResults),
    io:format("~p~n",[erlcloud_ec2:describe_security_groups([Ec2SecurityGroupId],[],[],Config)]).

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
    %io:format("Permissions: ~p~n",[Permissions]),
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
