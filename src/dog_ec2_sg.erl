-module(dog_ec2_sg).

-include("dog_trainer.hrl").
-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_ec2.hrl").

-export([
        create_port_anywhere_ingress_rules/1,
        publish_group/1,
        update_sg/3
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

get_ec2_security_groups(_GroupName) ->
    %TODO DB lookup
    [ 
       #{
           ec2_security_group_id => "sg-0eda39c42c4de2717",
           region => "us-east-1"
        }
    ].

publish_group(DogGroupName) ->
    AnywhereIngressRules = create_port_anywhere_ingress_rules(DogGroupName),
    io:format("AnywhereIngressRules: ~p~n",[AnywhereIngressRules]),
    lists:foreach(fun(Ec2SecurityGroup) ->
                          update_sg(
                            maps:get(ec2_security_group_id,Ec2SecurityGroup),
                            maps:get(region, Ec2SecurityGroup),
                            AnywhereIngressRules)
                  end,get_ec2_security_groups(DogGroupName)).

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
                             ip_protocol = Protocol,
                             from_port = From,
                             to_port = To,
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
		case erlcloud_ec2:authorize_security_group_ingress(Ec2SecurityGroupId, [RuleSpec], Config) of
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
    RemoveVpcIngressSpecs = default_remove_ingress_rules(),
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
