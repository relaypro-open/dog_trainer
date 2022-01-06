-module(dog_ruleset).

-include("dog_trainer.hrl").

-export([
         %cidr_netmask/1,
         %cidr_network/1,
         generate_ruleset/10,
         generate_ruleset/3,
         %netmask_cidr/1,
         read_ruleset_set_v4_from_file/1,
         read_ruleset_set_v6_from_file/1,
         read_ruleset_unset_v4_from_file/1,
         read_ruleset_unset_v6_from_file/1,
         write_ruleset_set_v4_to_file/2,
         write_ruleset_set_v6_to_file/2,
         write_ruleset_unset_v4_to_file/2,
         write_ruleset_unset_v6_to_file/2
        ]).

-spec generate_ruleset(ProfileJson :: map(), Type :: atom(), Version :: binary() ) -> {'ok', iolist()}. 
generate_ruleset(ProfileJson,Type,Version) ->
  {Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap} = dog_ipset:id_maps(),
  generate_ruleset(ProfileJson,Type,Version,Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,
                   ServiceIdMap).

-spec generate_ruleset(ProfileJson :: map(), Type :: atom(), Version :: binary(), Ipv4RoleMap :: map(), 
                       Ipv6RoleMap :: map(), Ipv4ZoneMap :: map(), Ipv6ZoneMap :: map(), ZoneIdMap :: map(), 
                       GroupIdMap :: map(), ServiceIdMap :: map()
                      ) -> {'ok', iolist()}. 
generate_ruleset(ProfileJson,Type,Version,Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,
                 ServiceIdMap) ->
  try 
    Docker = get_docker(maps:get(<<"docker">>,ProfileJson, <<"undefined">>)),
    lager:debug("Docker: ~p",[Docker]),
    ProfileId = maps:get(<<"id">>,ProfileJson),
    ProfileName = maps:get(<<"name">>,ProfileJson),
    InboundJson = maps:get(<<"inbound">>,maps:get(<<"rules">>,ProfileJson)),
    %lager:info("InboundJson: ~p",[InboundJson]),
    InboundJsonWithIndex = lists:zip(lists:seq(1, length(InboundJson)), InboundJson),
    InboundRules = lists:map(fun({Index,L}) ->
                                 try
                                   Rules = json_to_rules(L, inbound, false, Type, Version, Ipv4RoleMap, Ipv6RoleMap, Ipv4ZoneMap, 
                                                               Ipv6ZoneMap, ServiceIdMap, GroupIdMap, ZoneIdMap),
                                   Rules
                                 catch
                                   Exception:Reason:Stacktrace ->
                                     lager:error("Error in profile ~p (~p) rule number: ~p",[ProfileName, ProfileId, Index]),
                                     lager:error("Exception: ~p, Reason: ~p, Stacktrace: ~p",[Exception,Reason,Stacktrace]),
                                     throw(Exception)
                                 end
                             end, InboundJsonWithIndex),
    %Non-Basic rules are put in INPUT only
    InboundJsonBasicOnly = lists:filter(fun(Rule) ->
                                            maps:get(<<"type">>,Rule) == <<"BASIC">>
                                        end, InboundJson),
    InboundJsonBasicOnlyWithIndex = lists:zip(lists:seq(1, length(InboundJsonBasicOnly)), InboundJsonBasicOnly),
    InboundSymmetricRules = lists:map(fun({Index, L}) -> 
                                          case maps:get(<<"action">>,L) of 
                                            <<"DROP">> ->
                                              [];
                                            _ ->
                                              try
                                                Rules = json_to_rules(L, outbound, true, Type, Version, Ipv4RoleMap, Ipv6RoleMap, 
                                                                            Ipv4ZoneMap, Ipv6ZoneMap, ServiceIdMap, GroupIdMap, 
                                                                            ZoneIdMap),
                                                Rules
                                              catch
                                                Exception:Reason:Stacktrace ->
                                                  lager:error("Error in profile ~p (~p) rule number: ~p",[ProfileName, ProfileId, Index]),
                                                  lager:error("Exception: ~p, Reason: ~p, Stacktrace: ~p",[Exception,Reason,Stacktrace]),
                                                  throw(Exception)
                                              end
                                          end 
                                      end, InboundJsonBasicOnlyWithIndex), 
    OutboundJson = maps:get(<<"outbound">>,maps:get(<<"rules">>,ProfileJson)),
    OutboundJsonWithIndex = lists:zip(lists:seq(1, length(OutboundJson)), OutboundJson),
    OutboundRules = lists:map(fun({Index, L}) -> 
                                  try
                                    Rules = json_to_rules(L, outbound, false, Type, Version, Ipv4RoleMap, Ipv6RoleMap, 
                                                                Ipv4ZoneMap, Ipv6ZoneMap, ServiceIdMap, GroupIdMap, 
                                                                ZoneIdMap),
                                    Rules
                                  catch
                                    Exception:Reason:Stacktrace ->
                                      lager:error("Error in profile ~p (~p) rule number: ~p",[ProfileName, ProfileId, Index]),
                                      lager:error("Exception: ~p, Reason: ~p, Stacktrace: ~p",[Exception,Reason,Stacktrace]),
                                      throw(Exception)
                                  end
                              end, OutboundJsonWithIndex),
    Ruleset = case Version of
                <<"v4">> ->
                  case Docker of
                    <<"undefined">> ->
                      lists:flatten([header_v4(),inbound_header_v4(),InboundRules,forward_header_v4(),outbound_header_v4(),
                                 InboundSymmetricRules,OutboundRules,footer_v4()]);
                    <<"false">> ->
                      lists:flatten([header_v4(),inbound_header_v4(),InboundRules,forward_header_v4(),outbound_header_v4(),
                                 InboundSymmetricRules,OutboundRules,footer_v4()]);
                    <<"true">> -> 
                      lists:flatten([header_docker_v4(),inbound_header_v4(),InboundRules,forward_header_docker_v4(),outbound_header_v4(),
                                 InboundSymmetricRules,OutboundRules,footer_docker_v4()])
                  end;
                <<"v6">> ->
                  lists:flatten([header_v6(),inbound_header_v6(),InboundRules,forward_header_v6(),outbound_header_v6(),
                                 InboundSymmetricRules,OutboundRules,footer_v6()])
              end,
    imetrics:add_m(generate_ruleset,"ok"),
    {ok, Ruleset}
  catch
    Exception:Reason:Stacktrace ->
      imetrics:add_m(generate_ruleset,"error"),
      lager:error("Exception: ~p, Reason: ~p, Stacktrace: ~p",[Exception,Reason,Stacktrace]),
      throw(Reason)
  after
    {ok, []}
  end.

-spec header_v4() -> iolist(). 
header_v4() -> 
  %"# Generated by dog_trainer v.0.0.1 on " ++ date_string() ++ "\n" ++
"*filter
:INPUT ACCEPT [0:0]
:FORWARD ACCEPT [0:0]
:OUTPUT ACCEPT [0:0]
".

-spec header_docker_v4() -> iolist(). 
header_docker_v4() -> 
  %"# Generated by dog_trainer v.0.0.1 on " ++ date_string() ++ "\n" ++
"*filter
:INPUT ACCEPT [0:0]
:FORWARD ACCEPT [0:0]
:OUTPUT ACCEPT [0:0]
:DOCKER - [0:0]
:DOCKER-ISOLATION-STAGE-1 - [0:0]
:DOCKER-ISOLATION-STAGE-2 - [0:0]
:DOCKER-USER - [0:0]
".

-spec inbound_header_v4() -> iolist().
inbound_header_v4() ->
"-A INPUT -m state --state RELATED,ESTABLISHED -j ACCEPT
-A INPUT -i lo -p tcp -m tcp --dport 4371 -j ACCEPT
".

-spec forward_header_v4() -> iolist().
forward_header_v4() ->
"-A FORWARD -j REJECT --reject-with icmp-port-unreachable
".

-spec forward_header_docker_v4() -> iolist().
forward_header_docker_v4() ->
"-A FORWARD -j DOCKER-USER
-A FORWARD -j DOCKER-ISOLATION-STAGE-1
-A FORWARD -o docker0 -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
-A FORWARD -o docker0 -j DOCKER
-A FORWARD -i docker0 ! -o docker0 -j ACCEPT
-A FORWARD -i docker0 -o docker0 -j ACCEPT
-A FORWARD -j REJECT --reject-with icmp-port-unreachable
".

-spec outbound_header_v4() -> iolist(). 
outbound_header_v4() -> 
"-A OUTPUT -p tcp -m tcp --sport 4371 -m state --state RELATED,ESTABLISHED -j ACCEPT
".

%-spec forward() -> string().
%forward() ->
%"-A FORWARD -j REJECT --reject-with icmp-port-unreachable
%".

-spec footer_v4() -> iolist().
footer_v4() ->
"COMMIT
".

-spec footer_docker_v4() -> iolist().
footer_docker_v4() ->
"-A DOCKER-ISOLATION-STAGE-1 -i docker0 ! -o docker0 -j DOCKER-ISOLATION-STAGE-2
-A DOCKER-ISOLATION-STAGE-1 -j RETURN
-A DOCKER-ISOLATION-STAGE-2 -o docker0 -j DROP
-A DOCKER-ISOLATION-STAGE-2 -j RETURN
-A DOCKER-USER -j RETURN
COMMIT
".
%# Completed on " ++ date_string() ++ ".".
%
%-spec docker_nat_table_v4() -> iolist().
%docker_nat_table_v4() ->
%"*nat
%:PREROUTING ACCEPT [0:0]
%:INPUT ACCEPT [0:0]
%:OUTPUT ACCEPT [0:0]
%:POSTROUTING ACCEPT [0:0]
%:DOCKER - [0:0]
%-A PREROUTING -m addrtype --dst-type LOCAL -j DOCKER
%-A OUTPUT ! -d 127.0.0.0/8 -m addrtype --dst-type LOCAL -j DOCKER
%-A POSTROUTING -s 172.17.0.0/16 ! -o docker0 -j MASQUERADE
%-A DOCKER -i docker0 -j RETURN
%COMMIT
%".

%-spec blank_nat_table_v4() -> iolist().
%blank_nat_table_v4() ->
%"*nat
%:PREROUTING ACCEPT [0:0]
%:INPUT ACCEPT [0:0]
%:OUTPUT ACCEPT [0:0]
%:POSTROUTING ACCEPT [0:0]
%:DOCKER - [0:0]
%COMMIT
%".

-spec header_v6() -> iolist(). 
header_v6() -> 
  %"# Generated by dog_trainer v.0.0.1 on " ++ date_string() ++ "\n" ++
"*filter
:INPUT DROP [0:0]
:FORWARD DROP [0:0]
:OUTPUT DROP [0:0]
:ICMPFLOOD - [0:0]
:SSHBRUTE - [0:0]
".

-spec inbound_header_v6() -> iolist().
inbound_header_v6() ->
  %# Don't attempt to firewall internal traffic on the loopback device."
"-A INPUT -i lo -j ACCEPT" ++ "\n" ++

  %# Continue connections that are already established or related to an established 
  %# connection.
"-A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT" ++ "\n" ++

  %# Drop non-conforming packets, such as malformed headers, etc.
"-A INPUT -m conntrack --ctstate INVALID -j DROP" ++ "\n" ++

  %# Block remote packets claiming to be from a loopback address.
"-A INPUT -s ::1/128 ! -i lo -j DROP" ++ "\n" ++

  %# Permit needed ICMP packet types for IPv6 per RFC 4890.
"-A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 1 -j ACCEPT
-A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 2 -j ACCEPT
-A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 3 -j ACCEPT
-A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 4 -j ACCEPT
-A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 133 -j ACCEPT
-A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 134 -j ACCEPT
-A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 135 -j ACCEPT
-A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 136 -j ACCEPT
-A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 137 -j ACCEPT
-A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 141 -j ACCEPT
-A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 142 -j ACCEPT
-A INPUT -s fe80::/10 -p ipv6-icmp -m icmp6 --icmpv6-type 130 -j ACCEPT
-A INPUT -s fe80::/10 -p ipv6-icmp -m icmp6 --icmpv6-type 131 -j ACCEPT
-A INPUT -s fe80::/10 -p ipv6-icmp -m icmp6 --icmpv6-type 132 -j ACCEPT
-A INPUT -s fe80::/10 -p ipv6-icmp -m icmp6 --icmpv6-type 143 -j ACCEPT
-A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 148 -j ACCEPT
-A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 149 -j ACCEPT
-A INPUT -s fe80::/10 -p ipv6-icmp -m icmp6 --icmpv6-type 151 -j ACCEPT
-A INPUT -s fe80::/10 -p ipv6-icmp -m icmp6 --icmpv6-type 152 -j ACCEPT
-A INPUT -s fe80::/10 -p ipv6-icmp -m icmp6 --icmpv6-type 153 -j ACCEPT
".

-spec forward_header_v6() -> iolist().
forward_header_v6() ->
"-A FORWARD -j REJECT --reject-with icmp6-port-unreachable
".

-spec outbound_header_v6() -> iolist(). 
outbound_header_v6() -> 
"-A OUTPUT -p udp -m udp --dport 53 -j ACCEPT
-A OUTPUT -p tcp -m tcp --dport 53 -j ACCEPT
".

%-spec forward() -> string().
%forward() ->
%"-A FORWARD -j REJECT --reject-with icmp-port-unreachable
%".

-spec footer_v6() -> iolist().
footer_v6() ->
  %# Chain for preventing ping flooding - up to 6 pings per second from a single 
  %# source, again with log limiting. Also prevents us from ICMP REPLY flooding 
  %# some victim when replying to ICMP ECHO from a spoofed source.
  %"-A ICMPFLOOD -m recent --set --name ICMP --mask ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff --rsource
  %-A ICMPFLOOD -m recent --update --seconds 1 --hitcount 6 --rttl --name ICMP --mask ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff --rsource -m limit --limit 1/sec --limit-burst 1 -j LOG --log-prefix \"iptables[ICMP-flood]: \"
  %-A ICMPFLOOD -m recent --update --seconds 1 --hitcount 6 --rttl --name ICMP --mask ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff --rsource -j DROP
  %-A ICMPFLOOD -j ACCEPT" ++ "\n" ++

  %# Chain for preventing SSH brute-force attacks.
  %# Permits 10 new connections within 5 minutes from a single host then drops 
  %# incomming connections from that host. Beyond a burst of 100 connections we 
  %# log at up 1 attempt per second to prevent filling of logs.
  %"-A SSHBRUTE -m recent --set --name SSH --mask ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff --rsource
  %-A SSHBRUTE -m recent --update --seconds 300 --hitcount 10 --name SSH --mask ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff --rsource -m limit --limit 1/sec --limit-burst 100 -j LOG --log-prefix \"iptables[SSH-brute]: \"
  %-A SSHBRUT:w
  %E -m recent --update --seconds 300 --hitcount 10 --name SSH --mask ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff --rsource -j DROP
  %-A SSHBRUTE -j ACCEPT
"COMMIT
".


-spec json_to_rules(map(), 'inbound' | 'outbound', Symmetric :: boolean(), Type :: 'ipsets' | 'iptables', 
                    Version :: iolist(), Ipv4RoleMap :: map(), Ipv6RoleMap :: map(), Ipv4ZoneMap :: map(), 
                    Ipv6ZoneMap :: map(), ServiceIdMap :: map(), GroupIdMap :: map(), ZoneIdMap :: map()
                    ) -> iolist().
json_to_rules(Json, Direction, Symmetric, Type, Version, Ipv4RoleMap, Ipv6RoleMap, Ipv4ZoneMap, Ipv6ZoneMap, 
              ServiceIdMap, GroupIdMap, ZoneIdMap) ->
  case Json of 
    [] ->
      throw(json_empty_list);
    _ ->
      case get_service(maps:get(<<"service">>,Json)) of 
        %this first case would be empty on brand new profile creation
        %not sure if we want to create some better error handling here or just return empty ruleset
        <<" ">> -> 
          [];
        ServiceId ->
          %lager:info("ServiceId: ~p",[ServiceId]),
          %ServiceName = case bin_uppercase(dog_service:get_name_by_id(ServiceId)) of
          ServiceName = get_service_name(ServiceId, ServiceIdMap),
          %{ok, ServiceDefinition} = get_service_by_id(ServiceId),
          ServiceDefinition = maps:get(ServiceId, ServiceIdMap),
          Services = maps:get(<<"services">>,ServiceDefinition),
          Rules = json_to_rule(Json, ServiceName, Services, Direction, Symmetric, Type, Version, Ipv4RoleMap,
                               Ipv6RoleMap, Ipv4ZoneMap, Ipv6ZoneMap, GroupIdMap, ZoneIdMap),
          lager:debug("Rules: ~p~n" , [Rules]),
          Rules
      end
  end.

-spec json_to_rule(map(), ServiceName :: binary(), Services :: list(),'inbound' | 'outbound', Symmetric :: boolean(),
                   Type :: 'ipsets' | 'iptables', Version :: iolist(), Ipv4RoleMap :: map(), Ipv6RoleMap :: map(),
                   Ipv4ZoneMap :: map(), Ipv6ZoneMap :: map(), GroupIdMap :: map(),
                   ZoneIdMap :: map()) -> iolist().
json_to_rule(Json, ServiceName, Services, Direction, Symmetric, Type, Version, Ipv4RoleMap, Ipv6RoleMap, 
             Ipv4ZoneMap, Ipv6ZoneMap, GroupIdMap, ZoneIdMap) ->
  lists:map(fun(Service) ->
                Environment = get_environment(maps:get(<<"environment">>,Json,<<"local">>)), 
                Group = case Environment of
                  <<"local">> ->
                    get_group(maps:get(<<"group">>,Json));
                  _ ->
                    LocalGroup = get_group(maps:get(<<"group">>,Json)),
                    EnvSep = <<"#">>,
                    <<Environment/bitstring,EnvSep/bitstring,LocalGroup/bitstring>>
                        end,
                %lager:info("Group: ~p",[Group]),
                GroupType = maps:get(<<"group_type">>,Json),
                %lager:info("GroupType: ~p",[GroupType]),
                %{ok, GroupName} = case GroupType of
                GroupName =  case Environment of
                               <<"local">> ->
                                 get_group_name(GroupType, Group, GroupIdMap, ZoneIdMap);
                               _ ->
                                 Group
                             end,
                %lager:info("GroupName: ~p",[GroupName]),
                Sep = <<"_">>,
                %TODO: fix ipsetname to match dog_ipset module name 
                IpsetName = get_ipset_name(GroupName,GroupType,Sep,Version),
                Protocol = get_protocol(maps:get(<<"protocol">>,Service),Version),
                Ports = get_ports(maps:get(<<"ports">>,Service)),
                %ICMP ping request input becomes ICMP ping response output
                Ports@1 = get_ports_list(Direction,Protocol,Ports),
                _Order = get_order(maps:get(<<"order">>,Json)),
                States = get_states(maps:get(<<"states">>,Json), Symmetric),
                _Log = get_log(maps:get(<<"log">>,Json)), 
                _LogPrefix = get_log_prefix(maps:get(<<"log_prefix">>,Json)), 
                RuleType = get_type(maps:get(<<"type">>,Json)),
                Chain = get_chain(Direction),
                ProtocolString = get_protcol_string(Protocol),
                MultiplePorts = get_multiple_ports(Ports),
                ProtocolModule = get_protocol_module(MultiplePorts, Protocol),
                PortParameter = get_port_parameter(Protocol,MultiplePorts,Direction,ProtocolModule),
                SourceParameter = get_source_parameter(Direction),
                InterfaceString = get_interface(maps:get(<<"interface">>,Json)),
                %lager:debug("InterfaceString: ~p~n",[InterfaceString]),
                Interface = get_interface(InterfaceString, Direction),
                Action = get_action(maps:get(<<"action">>,Json),Version,RuleType), 
                CommentJson = get_comment(maps:get(<<"comment">>,Json)),
                Comment = get_comment_json(CommentJson),
                Active = get_active(maps:get(<<"active">>,Json)),
                {RoleMap, ZoneMap} = case Version of
                                       <<"v4">> -> {Ipv4RoleMap, Ipv4ZoneMap};
                                       <<"v6">> -> {Ipv6RoleMap, Ipv6ZoneMap}
                                     end,
                Rule = case Active of
                         false -> 
                           "";
                         true -> 
                           case Type of
                             ipsets ->
                               case RuleType of
                                 basic ->
                                   generate_basic_rule_set(Group,Chain,SourceParameter,Interface,ProtocolString,PortParameter,
                                                           Ports@1,States,Action,Comment,GroupType,ServiceName,IpsetName);
                                 %TODO conversion between ipv4 and ipv6 masks
                                 connlimit ->
                                   ConnLimitAbove = maps:get(<<"conn_limit_above">>,Json),
                                   ConnLimitMask = maps:get(<<"conn_limit_mask">>,Json,[]),
																	 ConnLimitString = get_connlimit(ConnLimitAbove,ConnLimitMask,Version),
																	 generate_connlimit_rule_set(Chain,Interface,ProtocolString,PortParameter,Ports,
                                                               States,Action,Comment,ServiceName,IpsetName,ConnLimitString);
                                 recent ->
                                   RecentName = maps:get(<<"recent_name">>,Json,<<"DEFAULT">>),
                                   RecentMask = maps:get(<<"recent_mask">>,Json,[]),
                                   Seconds = maps:get(<<"seconds">>,Json),
                                   HitCount = maps:get(<<"hit_count">>,Json),
                                   RecentString = get_recent(RecentName,RecentMask,Seconds,HitCount,Version),
																	 RecentRule = generate_recent_rule_set(Chain,Interface,ProtocolString,PortParameter,Ports,
                                                                         States,Action,Comment,ServiceName,IpsetName,RecentString),
                                   RecentSetString = get_recent_set(RecentName),
																	 RecentSetRule = generate_recent_rule_set_set(Chain,Interface,ProtocolString,PortParameter,
                                                                                Ports,States,Comment,ServiceName,IpsetName,
                                                                                RecentSetString),
                                   io_lib:format("~s~n~s",[RecentRule,RecentSetRule])
                               end;
                             iptables ->
                               generate_basic_rule_unset(Group,Chain,SourceParameter,Interface,ProtocolString,PortParameter,
                                                         Ports@1,States,Action,Comment,GroupType,ServiceName,IpsetName,
                                                         RoleMap,ZoneMap)
                           end
                       end,
                Rule
            end, Services).

generate_basic_rule_unset(Group,Chain,SourceParameter,Interface,ProtocolString,PortParameter,Ports,States,Action,Comment,
                          GroupType,ServiceName,_IpsetName,IpRoleMap,IpZoneMap) ->
  Rule = case {GroupType, ServiceName} of
           {<<"ANY">>,<<"ANY">>} ->
             io_lib:format("-A ~s~s~s~s -j ~s~n",[Chain,Interface,States,Comment,Action]);
           {<<"ANY">>,_} ->
             io_lib:format("-A ~s~s~s~s ~s~s~s -j ~s~n",[Chain,Interface,ProtocolString,PortParameter,Ports,States,Comment,Action]);
           {<<"ZONE">>,<<"ANY">>} ->
             Addresses = maps:get(Group,IpZoneMap,[]),
             ZoneAddresses = case Chain of
                               <<"INPUT">> ->
                                 %{ok, Addresses} = dog_zone:get_all_ipv4s_by_id(Group),
                                 Addresses;
                               <<"OUTPUT">> ->
                                 %{ok, Addresses} = dog_zone:get_all_ipv4s_by_id(Group),
                                 lists:reverse(Addresses)
                             end,
             lists:map(fun(X) ->
                           io_lib:format("-A ~s ~s ~s~s~s~s~s -j ~s~n",[Chain,SourceParameter,X,Interface,ProtocolString,
                                                                        States,Comment,Action])
                       end, ZoneAddresses);
           {<<"ZONE">>,_} ->
             Addresses = maps:get(Group,IpZoneMap,[]),
             ZoneAddresses = case Chain of
                               <<"INPUT">> ->
                                 %{ok, Addresses} = dog_zone:get_all_ipv4s_by_id(Group),
                                 Addresses;
                               <<"OUTPUT">> ->
                                 %{ok, Addresses} = dog_zone:get_all_ipv4s_by_id(Group),
                                 lists:reverse(Addresses)
                             end,
             lists:map(fun(X) ->
                           io_lib:format("-A ~s ~s ~s~s~s~s ~s~s~s -j ~s~n",[Chain,SourceParameter,X,Interface,ProtocolString,
                                                                             PortParameter,Ports,States,Comment,Action])
                       end, ZoneAddresses);
           {<<"ROLE">>,<<"ANY">>} ->
             Addresses = maps:get(Group,IpRoleMap,[]),
             RoleAddresses = case Chain of
                               <<"INPUT">> ->
                                 %{ok, Addresses} = dog_group:get_all_ipv4s_by_id(Group),
                                 Addresses;
                               <<"OUTPUT">> ->
                                 %{ok, Addresses} = dog_group:get_all_ipv4s_by_id(Group),
                                 lists:reverse(Addresses)
                             end,
             lists:map(fun(X) ->
                           io_lib:format("-A ~s ~s ~s~s~s~s~s -j ~s~n",[Chain,SourceParameter,X,Interface,ProtocolString,
                                                                        States,Comment,Action])
                       end, RoleAddresses);
           {<<"ROLE">>,_} ->
             Addresses = maps:get(Group,IpRoleMap,[]),
             RoleAddresses = case Chain of
                               <<"INPUT">> ->
                                 %{ok, Addresses} = dog_group:get_all_ipv4s_by_id(Group),
                                 Addresses;
                               <<"OUTPUT">> ->
                                 %{ok, Addresses} = dog_group:get_all_ipv4s_by_id(Group),
                                 lists:reverse(Addresses)
                             end,
             %lager:info("Group: ~p",[Group]),
             %lager:info("RoleAddresses: ~p",[RoleAddresses]),
             lists:map(fun(X) ->
                           io_lib:format("-A ~s ~s ~s~s~s~s ~s~s~s -j ~s~n",[Chain,SourceParameter,X,Interface,
                                                                             ProtocolString,PortParameter,Ports,States,
                                                                             Comment,Action])
                       end, RoleAddresses)
         end,
  %lager:info("Rule: ~p",[Rule]),
  Rule.
%BAD:
%-A OUTPUT -m set --match-set all-active_g_v4 src -p tcp -m tcp --sport 8301 -m state --state RELATED,ESTABLISHED -m comment --comment serf_raft_tcp 
%GOOD:
%-A OUTPUT -p tcp -m set --match-set all-active_g_v4 src -m tcp --sport 8301 -m state --state RELATED,ESTABLISHED -m comment --comment serf_raft_tcp 
generate_basic_rule_set(_Group,Chain,_SourceParameter,Interface,ProtocolString,PortParameter,Ports,States,
                        Action,Comment,_GroupType,ServiceName,IpsetName) ->
  case {IpsetName, ServiceName} of
    {<<"ANY">>,<<"ANY">>} ->
      io_lib:format("-A ~s~s~s~s -j ~s~n",[Chain,Interface,States,Comment,Action]);
    {_,<<"ANY">>} ->
      io_lib:format("-A ~s~s -m set --match-set ~s src~s~s -j ~s~n",[Chain,Interface,IpsetName,States,Comment,Action]);
    {<<"ANY">>,_} ->
      io_lib:format("-A ~s~s~s~s ~s~s~s -j ~s~n",[Chain,Interface,ProtocolString,PortParameter,Ports,States,Comment,Action]);
    {_,_} ->
      io_lib:format("-A ~s~s~s -m set --match-set ~s src~s ~s~s~s -j ~s~n",[Chain,Interface,ProtocolString,IpsetName,
                                                                            PortParameter,Ports,States,Comment,Action])
  end.

%iptables -A INPUT -p tcp --syn --dport 80 -m connlimit --connlimit-above 20 --connlimit-mask 24 -j REJECT --reject-with tcp-reset                                
generate_connlimit_rule_set(Chain,Interface,ProtocolString,PortParameter,Ports,States,Action,Comment,ServiceName,
                            IpsetName,ConnLimitString) ->
  case {IpsetName, ServiceName} of
    {<<"ANY">>,<<"ANY">>} ->
      io_lib:format("-A ~s~s~s~s~s -j ~s~n",[Chain,Interface,States,ConnLimitString,Comment,Action]);
    {_,<<"ANY">>} ->
      io_lib:format("-A ~s~s -m set --match-set ~s src~s~s~s -j ~s~n",[Chain,Interface,IpsetName,States,ConnLimitString,
                                                                       Comment,Action]);
    {<<"ANY">>,_} ->
      io_lib:format("-A ~s~s~s~s ~s~s~s~s -j ~s~n",[Chain,Interface,ProtocolString,PortParameter,Ports,States,ConnLimitString,
                                                    Comment,Action]);
    {_,_} ->
      io_lib:format("-A ~s~s~s -m set --match-set ~s src~s ~s~s~s~s -j ~s~n",[Chain,Interface,ProtocolString,IpsetName,
                                                                              PortParameter,Ports,States,ConnLimitString,
                                                                              Comment,Action])
  end.

%iptables -I INPUT 1 -p tcp --dport 22 -m state --state NEW -m recent --name ssh \
% --update --seconds 60 --hitcount 6 -j REJECT
%iptables -I INPUT 2 -p tcp --dport 22 -m state --state NEW -m recent --name ssh --set
generate_recent_rule_set(Chain,Interface,ProtocolString,PortParameter,Ports,States,Action,Comment,ServiceName,IpsetName,RecentString) ->
  case {IpsetName, ServiceName} of
    {<<"ANY">>,<<"ANY">>} ->
      io_lib:format("-A ~s~s~s~s~s -j ~s~n",[Chain,Interface,States,RecentString,Comment,Action]);
    {_,<<"ANY">>} ->
      io_lib:format("-A ~s~s -m set --match-set ~s src~s~s~s -j ~s~n",[Chain,Interface,IpsetName,States,RecentString,Comment,Action]);
    {<<"ANY">>,_} ->
      io_lib:format("-A ~s~s~s~s ~s~s~s~s -j ~s~n",[Chain,Interface,ProtocolString,PortParameter,Ports,States,RecentString,
                                                    Comment,Action]);
    {_,_} ->
      io_lib:format("-A ~s~s~s -m set --match-set ~s src~s ~s~s~s~s -j ~s~n",[Chain,Interface,ProtocolString,IpsetName,PortParameter,
                                                                              Ports,States,RecentString,Comment,Action])
  end.

%-A OUTPUT -p tcp -m set --match-set db_validation_qa_g_v4 src -m tcp --sport 2777 -m state --state RELATED,ESTABLISHED -m recent --update --seconds 60 --hitcount 100 --name stunnel --mask 255.255.255.255 --rsource -m comment --comment "stunnel - riak_stats_port recent" -j ACCEPT
get_recent(RecentName,RecentMask,Seconds,HitCount,Version) ->
  case RecentMask of
  [] ->
    DefaultRecentMask = case Version of
      <<"v4">> -> <<"255.255.255.255">>;
      <<"v6">> -> <<"ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff">>
    end,
    io_lib:format(" -m recent --update --seconds ~B --hitcount ~B --name ~s --mask ~s",[Seconds,HitCount,RecentName,DefaultRecentMask]);
  _ ->                                                           
    io_lib:format(" -m recent --update --seconds ~B --hitcount ~B --name ~s --mask ~s",[Seconds,HitCount,RecentName,RecentMask])
  end.

generate_recent_rule_set_set(Chain,Interface,ProtocolString,PortParameter,Ports,States,Comment,ServiceName,IpsetName,RecentString) ->
  case {IpsetName, ServiceName} of
    {<<"ANY">>,<<"ANY">>} ->
      io_lib:format("-A ~s~s~s~s~s~n",[Chain,Interface,States,RecentString,Comment]);
    {_,<<"ANY">>} ->
      io_lib:format("-A ~s~s -m set --match-set ~s src~s~s~s~n",[Chain,Interface,IpsetName,States,RecentString,Comment]);
    {<<"ANY">>,_} ->
      io_lib:format("-A ~s~s~s~s ~s~s~s~s~n",[Chain,Interface,ProtocolString,PortParameter,Ports,States,RecentString,Comment]);
    {_,_} ->
      io_lib:format("-A ~s~s~s -m set --match-set ~s src~s ~s~s~s~s~n",[Chain,Interface,ProtocolString,IpsetName,PortParameter,
                                                                        Ports,States,RecentString,Comment])
  end.
  
%-A OUTPUT -p tcp -m set --match-set db_validation_qa_g_v4 src -m tcp --sport 2777 -m state --state RELATED,ESTABLISHED -m recent --set --name stunnel --mask 255.255.255.255 --rsource -m comment --comment "stunnel - riak_stats_port recent" -j ACCEPT
get_recent_set(RecentName) ->
  io_lib:format(" -m recent --set --name ~s",[RecentName]).

-spec get_connlimit(ConnLimitAbove :: binary(), ConnLimitMask :: binary(), Version :: binary()) -> iolist().
get_connlimit(ConnLimitAbove, ConnLimitMask, Version) ->
	case ConnLimitMask of
		[] ->
			DefaultConnLimitMask = case Version of
				<<"v4">> -> <<"32">>;
				<<"v6">> -> <<"128">>
			end,
			io_lib:format(" -m connlimit --connlimit-above ~B --connlimit-mask ~B --connlimit-saddr",[ConnLimitAbove,DefaultConnLimitMask]);
		_ ->
			io_lib:format(" -m connlimit --connlimit-above ~B --connlimit-mask ~B --connlimit-saddr",[ConnLimitAbove,ConnLimitMask])
	end.

-spec bin_uppercase(Bin :: binary()) -> binary().
bin_uppercase(Bin) ->
  binary:list_to_bin(string:uppercase(binary:bin_to_list(Bin))).

-spec get_chain(Direction :: atom()) -> binary().
get_chain(Direction) ->
  case Direction of
    inbound -> <<"INPUT">>;
    outbound -> <<"OUTPUT">>
  end.

-spec get_protcol_string(Protocol :: binary()) -> iolist().
get_protcol_string(Protocol) ->
  case Protocol of
    <<"any">> ->
      [];
    _ ->
      io_lib:format(" -p ~s",[Protocol])
  end.

-spec get_multiple_ports(Ports :: list() ) -> boolean().
get_multiple_ports(Ports) ->
  case re:run(Ports,"[,:]") of
    {match,_} ->
      true;
    _ ->
      false
  end.

-spec get_protocol_module(MultiplePorts :: boolean(), Protocol :: binary()) -> iolist().
get_protocol_module(MultiplePorts, Protocol) ->
  case MultiplePorts of
    true ->
      [];
    false ->
      io_lib:format(" -m ~s",[Protocol])
  end.

-spec get_source_parameter(Direction :: atom()) -> iolist().
get_source_parameter(Direction) ->
  case Direction of
    inbound -> "-s";
    outbound -> "-d"
  end.

-spec get_service_name(ServiceId :: binary(), ServiceIdMap :: map()) -> { error, atom()} | binary().
get_service_name(ServiceId, ServiceIdMap) ->
  case maps:get(<<"name">>,maps:get(ServiceId,ServiceIdMap)) of
    {error, notfound } ->
      throw(service_not_found);
    Name -> 
      bin_uppercase(Name)
  end.

-spec get_group_name(GroupType :: binary(), Group :: binary(), GroupIdMap :: map(), ZoneIdMap :: map()) -> binary().
get_group_name(GroupType, Group, GroupIdMap, ZoneIdMap) ->
  case GroupType of
    <<"ROLE">> -> 
      %get_group_name_by_id(Group);
                  maps:get(<<"name">>,maps:get(Group,GroupIdMap));
    <<"ZONE">> -> 
      %get_zone_name_by_id(Group);
                  maps:get(<<"name">>,maps:get(Group,ZoneIdMap));
    <<"ANY">> -> 
      %{ok, <<"ANY">>}
                  <<"ANY">>
  end.

-spec get_ipset_name(GroupName :: binary(),GroupType :: binary(), Sep :: binary(), Version :: binary()) -> binary().
get_ipset_name(GroupName,GroupType,Sep,Version) ->
  LocalName = case bin_uppercase(GroupName) of
    <<"ANY">> -> 
      <<"ANY">>;
    _ -> 
      case GroupType of
        <<"ANY">> -> <<"ANY">>;
        <<"ROLE">> -> <<GroupName/bitstring,Sep/bitstring,<<"g">>/bitstring,Version/bitstring>>;
        <<"ZONE">> -> <<GroupName/bitstring,Sep/bitstring,<<"z">>/bitstring,Version/bitstring>>
      end
  end,
  LocalName.

-spec get_ports_list(Direction :: atom(), Protocol :: binary(), Ports :: list()) -> list().
get_ports_list(Direction,Protocol,Ports) ->
  case Direction of 
    outbound ->
      case Protocol of
        <<"icmp">> ->
          case Ports of
            ["8"] ->
              ["0"];
            _ ->
              Ports
          end;
        _ ->
          Ports
      end;
    _ ->
      Ports
  end.

-spec get_port_parameter(Protocol :: binary(), MultiplePorts :: boolean(), Direction :: atom(), ProtocolModule :: iolist()) -> iolist().
get_port_parameter(Protocol,MultiplePorts,Direction,ProtocolModule) ->
  case lists:member(Protocol,[<<"udp">>,<<"tcp">>,<<"icmp">>,<<"ipv6-icmp">>,<<"udplite">>,<<"esp">>,<<"ah">>,<<"sctp">>]) of
    false ->
      lager:debug("Protocol: ~p not in list",[Protocol]),
      io_lib:format("",[]);
    true ->
      lager:debug("Protocol: ~p is in list",[Protocol]),
      case Protocol of
        <<"icmp">> -> 
          " -m icmp --icmp-type";
        <<"ipv6-icmp">> -> 
          " -m icmp6 --icmpv6-type";
        _ ->  
          case MultiplePorts of
            true ->
              DirectionPorts = case Direction of
                                 inbound ->
                                   " --dports";
                                 outbound -> 
                                   " --sports"
                               end,
              io_lib:format("~s -m multiport~s",[ProtocolModule,DirectionPorts]);
            false ->
              DirectionPorts = case Direction of
                                 inbound ->
                                   " --dport";
                                 outbound -> 
                                   " --sport"
                               end,
              io_lib:format("~s~s",[ProtocolModule,DirectionPorts])
          end
      end
  end.

-spec get_interface(InterfaceString :: binary(), Direction :: atom()) -> iolist(). 
get_interface(InterfaceString, Direction) -> 
  case InterfaceString of
    <<>> -> "";
    <<"ANY">> -> "";
    _ -> case Direction of
           inbound -> io_lib:format(" -i ~s",[InterfaceString]);
           outbound -> io_lib:format(" -o ~s",[InterfaceString])
         end
  end.

-spec get_comment_json(CommentJson :: binary() ) -> iolist().
get_comment_json(CommentJson) ->
  case CommentJson of
    <<>> -> ""; 
    Other ->
      case length(string:split(CommentJson," ")) of
        1 -> 
          io_lib:format(" -m comment --comment ~s",[Other]);
        _ ->
          io_lib:format(" -m comment --comment \"~s\"",[Other])
      end
  end.

-spec get_order(integer()) -> string(). 
get_order(Order) ->
  integer_to_list(Order).

-spec get_action(Action :: binary(), Version :: binary(), RuleType :: atom() ) -> string() | atom(). 
get_action(Action, Version, RuleType) ->
	case RuleType of
		connlimit ->
			<<"REJECT --reject-with tcp-reset">>;
		_ ->
			case bin_uppercase(Action) of
				<<"DROP">> -> string:to_upper(binary_to_list(Action));
				<<"ACCEPT">> -> string:to_upper(binary_to_list(Action));
				<<"REJECT">> ->
							case Version of
								<<"v4">> ->
									<<"REJECT --reject-with icmp-port-unreachable">>;
								<<"v6">> ->
									<<"REJECT --reject-with icmp6-port-unreachable">>
							end;
				_ -> error
			end
	end.

-spec get_active(boolean()) -> 'error' | 'false' | 'true'.
get_active(Active) ->
  case Active of
    true -> true;
    false -> false;
    _ -> error
  end.

-spec get_states(States :: iolist(), Symmetric :: boolean() ) -> 'error' | string().
get_states(States, Symmetric) ->
  lager:debug("States: ~p~n", [States]),
  case States of
    [] -> 
      case Symmetric of 
        false -> 
          "";
        true -> 
          io_lib:format(" -m state --state ~s", ["RELATED,ESTABLISHED"])
      end;
    _ ->
      case Symmetric of 
        false -> 
          S = lists:map(fun(X) ->
                            io_lib:format("~s", [binary_to_list(X)]) end, States),
          io_lib:format(" -m state --state ~s", [string:join(S,",")]);
        true -> 
          io_lib:format(" -m state --state ~s", ["RELATED,ESTABLISHED"])
      end
  end.

-spec get_log(boolean()) -> boolean().
get_log(Log) -> 
  case Log of
    true -> true;
    false -> false
  end.

-spec get_log_prefix(iolist()) -> iolist().
get_log_prefix(LogPrefix) ->
  LogPrefix.

-spec get_type(binary()) -> atom().
get_type(Type) ->
  case bin_uppercase(Type) of
    <<"BASIC">> -> basic;
    <<"CONNLIMIT">> -> connlimit;
    <<"RECENT">> -> recent;
    _ -> throw(invalid_rule_type)
  end.

-spec get_group(binary()) -> binary().
get_group(Source) ->
  Source.

-spec get_service(binary()) -> binary().
get_service(Service) ->
  Service.

-spec get_ports(iolist()) -> iolist().
get_ports(Ports) ->
  %lager:info("Ports: ~p",[Ports]),
  lists:join(",",[binary_to_list(X) || X <- Ports]).

-spec get_interface(binary()) -> binary().
get_interface(Interface) ->
  Interface.

-spec get_environment(iolist()) -> any(). 
get_environment(Environment) ->
  case Environment of
    <<"">> -> <<"local">>;
    [] -> <<"local">>;
    Else -> Else
  end.

-spec get_comment(binary()) -> binary().
get_comment(Comment) ->
  Comment.

%-A INPUT -p ipv6-icmp -m icmp6 --icmpv6-type 8  -m comment --comment any -j ACCEPT  
-spec get_protocol( ProtocolDef :: binary(), Version :: binary() ) -> binary(). 
get_protocol(ProtocolDef, Version) ->
  case Version of
    <<"v6">> ->
      case ProtocolDef of
        <<"icmp">> -> 
          <<"ipv6-icmp">>;
        _ -> ProtocolDef
      end;
    <<"v4">> ->
      ProtocolDef
  end.

-spec get_docker(Docker :: binary() ) -> binary().
get_docker(Docker) ->
  case Docker of
    <<"false">> -> 
      <<"false">>;
    <<"true">> -> 
      <<"true">>;
    <<"undefined">> ->
      <<"undefined">>;
    _ ->
      <<"error">>
  end.

write_ruleset_set_v4_to_file(Ruleset, GroupName) ->
  FileName = ?RUNDIR ++ "/ruleset4_ipsets." ++ binary_to_list(GroupName) ++ ".txt",
  ok = file:write_file(FileName, Ruleset).
write_ruleset_set_v6_to_file(Ruleset, GroupName) ->
  FileName = ?RUNDIR ++ "/ruleset6_ipsets." ++ binary_to_list(GroupName) ++ ".txt",
  ok = file:write_file(FileName, Ruleset).
write_ruleset_unset_v4_to_file(Ruleset, GroupName) ->
  FileName = ?RUNDIR ++ "/ruleset4_iptables." ++ binary_to_list(GroupName) ++ ".txt",
  ok = file:write_file(FileName, Ruleset).
write_ruleset_unset_v6_to_file(Ruleset, GroupName) ->
  FileName = ?RUNDIR ++ "/ruleset6_iptables." ++ binary_to_list(GroupName) ++ ".txt",
  ok = file:write_file(FileName, Ruleset).

-spec read_ruleset_set_v4_from_file(GroupName :: binary()) -> {ok,binary()}.
read_ruleset_set_v4_from_file(GroupName) ->
  FileName = ?RUNDIR ++ "/ruleset4_ipsets." ++ binary_to_list(GroupName) ++ ".txt",
  {ok, Ruleset} = dog_file:read_file(FileName),
  {ok, Ruleset}.
-spec read_ruleset_set_v6_from_file(GroupName :: binary()) -> {ok,binary()}.
read_ruleset_set_v6_from_file(GroupName) ->
  FileName = ?RUNDIR ++ "/ruleset6_ipsets." ++ binary_to_list(GroupName) ++ ".txt",
  {ok, Ruleset} = dog_file:read_file(FileName),
  {ok, Ruleset}.
-spec read_ruleset_unset_v4_from_file(GroupName :: binary()) -> {ok,binary()}.
read_ruleset_unset_v4_from_file(GroupName) ->
  FileName = ?RUNDIR ++ "/ruleset4_iptables." ++ binary_to_list(GroupName) ++ ".txt",
  {ok, Ruleset} = dog_file:read_file(FileName),
  {ok, Ruleset}.
-spec read_ruleset_unset_v6_from_file(GroupName :: binary()) -> {ok,binary()}.
read_ruleset_unset_v6_from_file(GroupName) ->
  FileName = ?RUNDIR ++ "/ruleset6_iptables." ++ binary_to_list(GroupName) ++ ".txt",
  {ok, Ruleset} = dog_file:read_file(FileName),
  {ok, Ruleset}.

%%%--------------------------------------------------------------------
%%% @spec cidr_netmask(Bits :: integer()) -> ipv4()
%%% @doc  Return the netmask corresponding to the network bits received in CIDR 
%%%       format.
%%%--------------------------------------------------------------------
%cidr_netmask(Bits) when is_integer(Bits) andalso Bits =< 32 ->
%  ZeroBits = 8 - (Bits rem 8),
%  Last = (16#ff bsr ZeroBits) bsl ZeroBits,
%
%  case (Bits div 8) of
%    0 ->
%      {(255 band Last), 0, 0, 0};
%    1 ->
%      {255, (255 band Last), 0, 0};
%    2 ->
%      {255, 255, (255 band Last), 0};
%    3 ->
%      {255, 255, 255, (255 band Last)};
%    4 ->
%      {255, 255, 255, 255}
%  end.
%
%%%--------------------------------------------------------------------
%%% @spec cidr_network({Addr :: ipv4(), Bits :: integer()}) -> ipv4()
%%% @doc  Return the subnet corresponding the the IP address and network bits 
%%%       received in CIDR format.
%%%--------------------------------------------------------------------
%cidr_network({{I1, I2, I3, I4}, Bits}) when is_integer(Bits) andalso Bits =< 32 ->
%  ZeroBits = 8 - (Bits rem 8),
%  Last = (16#ff bsr ZeroBits) bsl ZeroBits,
%
%  case (Bits div 8) of
%    0 ->
%      {(I1 band Last), 0, 0, 0};
%    1 ->
%      {I1, (I2 band Last), 0, 0};
%    2 ->
%      {I1, I2, (I3 band Last), 0};
%    3 ->
%      {I1, I2, I3, (I4 band Last)};
%    4 ->
%      {I1, I2, I3, I4}
%  end.
%
%binary_ones(OnesZeros) when is_list(OnesZeros) ->
%  length(lists:filter(fun(X) -> 
%                          integer_to_binary(X) == <<"49">> end, 
%                      OnesZeros)).
%
%integer_to_binary_list(Integer) when is_integer(Integer) ->
%  io_lib:format("~8..0B",[list_to_integer(binary_to_list(integer_to_binary(Integer,2)))]).
%
%netmask_cidr({I1,I2,I3,I4}) ->
%  binary_ones(
%  lists:flatten([
%   integer_to_binary_list(I1),
%   integer_to_binary_list(I2),
%   integer_to_binary_list(I3),
%   integer_to_binary_list(I4)
%  ])).
