-module(dog_ipset).

-include("dog_trainer.hrl").

-export([
        create/1%,
        %delete/1,
        %replace/2,
        %update/2
        ]).

-export([
         create_hash/1,
         create_ipset/4,
         create_ipsets/0,
         create_ipsets/2,
         delete_old/0,
         force_update_ipsets/0,
         get_hashes/0,
         hash_check/1,
         id_maps/0,
         latest_hash/0,
         %ipsets_map/4,
         normalize_ipset/1,
         persist_ipset/0,
         publish_to_outbound_exchanges/1,
         publish_to_outbound_exchange/2,
         publish_to_external/1,
         read_current_ipset/0,
         read_hash/0,
         %set_hash/1,
         %update_ipsets/0,
         update_ipsets/1
        ]).

-export([
       create_internal_groups/0,
        merge_groups/2,
        create_internal_ipsets/1,
        create_merged_ipsets/1,
        merge_ipsets/2
        ]).

-spec create_ipsets(IpsetMap :: map(), Version :: binary()) -> iolist().
create_ipsets(IpsetMap,Version) ->
    Zones = maps:to_list(maps:get(<<"zones">>,IpsetMap,[])),
    Groups = maps:to_list(maps:get(<<"groups">>,IpsetMap,[])),
    ZonesIpsetCreate = lists:flatten(accum_create_ipsets(Zones, <<"z">>, Version)),
    GroupsIpsetCreate = lists:flatten(accum_create_ipsets(Groups, <<"g">>, Version)),
    lists:flatten(string:join([ZonesIpsetCreate, GroupsIpsetCreate],"\n")).

-spec flush_ipsets(IpsetMap :: map(), Version :: binary()) -> iolist().
flush_ipsets(IpsetMap,Version) ->
    Zones = maps:to_list(maps:get(<<"zones">>,IpsetMap,[])),
    Groups = maps:to_list(maps:get(<<"groups">>,IpsetMap,[])),
    ZonesIpsetDestroy = lists:flatten(accum_flush_ipsets(Zones, <<"z">>, Version)),
    GroupsIpsetDestroy = lists:flatten(accum_flush_ipsets(Groups, <<"g">>, Version)),
    lists:flatten(string:join([ZonesIpsetDestroy, GroupsIpsetDestroy],"\n")).

-spec destroy_ipsets(IpsetMap :: map(), Version :: binary()) -> iolist().
destroy_ipsets(IpsetMap,Version) ->
    Zones = maps:to_list(maps:get(<<"zones">>,IpsetMap,[])),
    Groups = maps:to_list(maps:get(<<"groups">>,IpsetMap,[])),
    ZonesIpsetDestroy = lists:flatten(accum_destroy_ipsets(Zones, <<"z">>, Version)),
    GroupsIpsetDestroy = lists:flatten(accum_destroy_ipsets(Groups, <<"g">>, Version)),
    lists:flatten(string:join([ZonesIpsetDestroy, GroupsIpsetDestroy],"\n")).

-spec accum_create_ipsets(IpsetList :: [tuple()], Type :: binary(), Version :: binary()) -> list().
accum_create_ipsets(IpsetList,Type,Version) ->
    lists:map(fun({NameBase, Ips}) ->
                      Name = name(NameBase,Type,Version),
                      NewName = new_name(NameBase,Type,Version),
                      create_ipset(Name,NewName,Ips,Version)
              end, IpsetList).

-spec accum_flush_ipsets(IpsetList :: [tuple()], Type :: binary(), Version :: binary()) -> list().
accum_flush_ipsets(IpsetList,Type,Version) ->
    lists:map(fun({NameBase, _Ips}) ->
                      NewName = new_name(NameBase,Type,Version),
                      flush_ipset(NewName)
              end, IpsetList).

-spec accum_destroy_ipsets(IpsetList :: [tuple()], Type :: binary(), Version :: binary()) -> list().
accum_destroy_ipsets(IpsetList,Type,Version) ->
    lists:map(fun({NameBase, _Ips}) ->
                      NewName = new_name(NameBase,Type,Version),
                      destroy_ipset(NewName)
              end, IpsetList).

-spec new_name(NameBase :: binary(), Type :: binary(), Version :: binary()) -> NewName :: binary().
new_name(NameBase,Type,Version) ->
    Sep = <<"_">>,
    NewSuffix = <<"n">>,
    NewName = <<NameBase/bitstring,Sep/bitstring,Type/bitstring,Version/bitstring,NewSuffix/bitstring>>,
    NewName.

-spec name(NameBase :: binary(), Type :: binary(), Version :: binary()) -> NewName :: binary().
name(NameBase,Type,Version) ->
    Sep = <<"_">>,
    Name = <<NameBase/bitstring,Sep/bitstring,Type/bitstring,Version/bitstring>>,
    Name.

-spec create_ipset(Name :: binary(), NewName :: binary(),Ips :: list(), Version :: binary() ) -> list().
create_ipset(Name,NewName,Ips,Version) ->
    Inet = case Version of
        <<"v4">> -> "inet";
        <<"v6">> -> "inet6"
    end,
    Create = "create " ++ binary_to_list(Name) ++ " hash:net family " ++ Inet ++ " -exist",
    CreateNew = "create " ++ binary_to_list(NewName) ++ " hash:net family " ++ Inet ++ " -exist",
    Adds = string:join(lists:map(fun(Ip) ->
                                        FormatedIp = ip_ipset_format(Ip),
                                         add_to_ipset(NewName, FormatedIp)
                                 end , Ips),"\n"),
    Swap = "swap " ++ binary_to_list(NewName) ++ " " ++ binary_to_list(Name),
    Ipset= io_lib:format("~s~n~s~n~s~n~s~n",[Create,CreateNew,Adds,Swap]),
    Ipset.

-spec flush_ipset(NewName :: binary()) -> list().
flush_ipset(NewName) ->
    Flush = "flush " ++ binary_to_list(NewName),
    Ipset= io_lib:format("~s~n",[Flush]),
    Ipset.

-spec destroy_ipset(NewName :: binary()) -> list().
destroy_ipset(NewName) ->
    Destroy = "destroy " ++ binary_to_list(NewName),
    Ipset= io_lib:format("~s~n",[Destroy]),
    Ipset.

-spec write_ipsets_to_file(IpSet :: iolist()) -> 'ok'.
write_ipsets_to_file(IpSet) ->
    TempFile = ?RUNDIR ++ "/ipset.txt",
    ok = file:write_file(TempFile, IpSet).

-spec add_to_ipset(Name :: binary(), Ip :: binary()) -> iolist().
add_to_ipset(Name, Ip) ->
    lager:debug("add_to_ipset(Name,Ip): ~p, ~p",[Name, Ip]),
    Add = "add " ++ binary_to_list(Name) ++ " " ++ binary_to_list(Ip),
    Add.

-spec ip_ipset_format(Ip :: iolist()) -> binary().
ip_ipset_format(Ip) ->
    case Ip of
        <<"0.0.0.0/0">> ->
            <<"0.0.0.0">>;
        _ ->
            Ip
    end.

-spec get_hashes() -> {ok, binary()}.
get_hashes() ->
    Now =  erlang:system_time(second),
    IpsetHashValidSeconds = application:get_env(dog_trainer,ipset_hash_valid_seconds,600),
    TimeCutoff = Now - IpsetHashValidSeconds,
    {ok, R} = dog_rethink:run(
    fun(X) ->
        reql:db(X, dog),
        reql:table(X, ipset),
        reql:order_by(X,reql:desc(<<"timestamp">>))
    end),
    Result = R,
    Hashes = lists:flatten(Result),
    LastHash = maps:get(<<"hash">>,hd(Hashes)),
    ValidIpsets = lists:filter(fun(X) -> 
                            TimeStamp = maps:get(<<"timestamp">>,X,0),
                            TimeStamp > TimeCutoff
                    end, Hashes),
    ValidHashes = [maps:get(<<"hash">>,Hash) || Hash <- ValidIpsets, maps:get(<<"hash">>,Hash) =/= <<"initial">>],
    UniqueValidHashes = sets:to_list(sets:from_list(ValidHashes)),
    case UniqueValidHashes of
        [] -> 
            {ok, [LastHash] };
        _ -> 
            {ok, UniqueValidHashes }
    end.

-spec latest_hash() -> {ok, binary()}.
latest_hash() ->
    {ok, R} = dog_rethink:run(
    fun(X) ->
        reql:db(X, dog),
        reql:table(X, ipset),
        reql:order_by(X, reql:desc(<<"timestamp">>)),
        %reql:order_by(X,<<"timestamp">>,#{index => <<"timestamp">>}),
        %reql:desc(X),
        reql:nth(X,0)
    end),
    {ok, maps:get(<<"hash">>,R)}.

-spec create(IpsetHash :: binary()) -> {ok, pid()}.
create(Hash) ->
    lager:info("hash: ~p",[Hash]),
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    Timestamp = dog_time:timestamp(),
    {ok, R} = dog_rethink:run(
    fun(X) ->
        reql:db(X, dog),
        reql:table(X, ipset),
        %reql:get_all(X, <<"global">>, #{index => <<"name">>}),
        reql:insert(X, 
                    #{
                      <<"hash">> => Hash,
                      <<"timestamp">> => Timestamp
                     })
    end),
    {ok, R}.

-spec delete_old() -> ok.
delete_old() ->
    %r.db('dog').table('ipset').orderBy({index: r.desc("timestamp")}).slice(10).delete()
    IpsetNumberToKeep = application:get_env(dog_trainer,ipset_number_to_keep,60),
    {ok, _R} = dog_rethink:run(
    fun(X) ->
        reql:db(X, dog),
        reql:table(X, ipset),
        reql:order_by(X, reql:desc(<<"timestamp">>)),
        %reql:order_by(X,<<"timestamp">>,#{index => <<"timestamp">>}),
        reql:slice(X,IpsetNumberToKeep),
        reql:delete(X)
    end),
    ok.

-spec create_hash(Ipset :: string()) -> any().
create_hash(Ipset) ->
    base16:encode(crypto:hash(sha256, Ipset)).

-spec create_ipsets() -> {MergedIpsets :: list(), InternalIpsets :: map()}.
create_ipsets() ->
  {ExternalUnionGroups, ExternalPrefixGroups} = dog_external:grouped_by_ipset_name(),
  InternalGroups = create_internal_groups(),
  InternalIpsetsMap = create_internal_ipsets(InternalGroups),
  MergedInternalGroups = dog_ipset:merge_ipsets(InternalGroups, ExternalUnionGroups),
  %MergedInternalGroups = InternalGroups,
  MergedGroups = merge_groups(MergedInternalGroups, ExternalPrefixGroups),
  MergedIpsetsList = create_merged_ipsets(MergedGroups),
  {MergedIpsetsList, InternalIpsetsMap}.

-spec id_maps() ->
  {Ipv4RoleMap :: map(),Ipv6RoleMap :: map(),Ipv4ZoneMap :: map(),Ipv6ZoneMap :: map(),ZoneIdMap :: map(),
   GroupIdMap :: map(),ServiceIdMap :: map()}.
id_maps() ->
  {{ExternalUnionGroupIpv4sGrouped,
   ExternalUnionGroupIpv6sGrouped, 
   ExternalUnionZoneIpv4sGrouped, 
   ExternalUnionZoneIpv6sGrouped},
  {ExternalPrefixGroupIpv4sGrouped,
   ExternalPrefixGroupIpv6sGrouped, 
   ExternalPrefixZoneIpv4sGrouped, 
   ExternalPrefixZoneIpv6sGrouped}} = dog_external:grouped_by_ipset_name(),

  InternalIpv4GroupMap = dog_group:all_ipv4s_grouped(),
  InternalIpv6GroupMap = dog_group:all_ipv6s_grouped(),
  InternalIpv4ZoneMap = dog_zone:all_ipv4s_grouped(),
  InternalIpv6ZoneMap = dog_zone:all_ipv6s_grouped(),

  MergedInternalIpv4GroupMap = merge_ipset(ExternalUnionGroupIpv4sGrouped,InternalIpv4GroupMap),
  MergedInternalIpv6GroupMap = merge_ipset(ExternalUnionGroupIpv6sGrouped,InternalIpv6GroupMap),
  MergedInternalIpv4ZoneMap = merge_ipset(ExternalUnionZoneIpv4sGrouped,InternalIpv4ZoneMap),
  MergedInternalIpv6ZoneMap = merge_ipset(ExternalUnionZoneIpv6sGrouped,InternalIpv6ZoneMap),

  Ipv4RoleMap = maps:merge(MergedInternalIpv4GroupMap, ExternalPrefixGroupIpv4sGrouped),
  Ipv6RoleMap = maps:merge(MergedInternalIpv6GroupMap, ExternalPrefixGroupIpv6sGrouped),
  Ipv4ZoneMap = maps:merge(MergedInternalIpv4ZoneMap, ExternalPrefixZoneIpv4sGrouped),
  Ipv6ZoneMap = maps:merge(MergedInternalIpv6ZoneMap, ExternalPrefixZoneIpv6sGrouped),
  
  ZoneIdMap = dog_zone:get_all_grouped_by_id(),
  GroupIdMap = dog_group:get_all_grouped_by_id(),
  ServiceAllMap = dog_service:get_all_grouped_by_id(),
  ServiceAny = case dog_service:get_by_id(<<"any">>) of 
                 {ok, Service} ->
                   Service;
                 {error, _} ->
                   throw(service_not_found)
               end,
  ServiceAnyMap = #{<<"any">> => ServiceAny},
  ServiceIdMap = maps:merge(ServiceAllMap,ServiceAnyMap),
  {Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap}.

-spec create_merged_ipsets(MergedGroups :: tuple()) -> iolist().
create_merged_ipsets(MergedGroups) ->
  {GroupIpv4sGrouped,
   GroupIpv6sGrouped,
   ZoneIpv4sGrouped,
   ZoneIpv6sGrouped} = MergedGroups,
  Ipv4s = #{<<"zones">> => ZoneIpv4sGrouped,
            <<"groups">> => GroupIpv4sGrouped},
  CreateIpsets4 = create_ipsets(Ipv4s,<<"v4">>),
  Ipv6s = #{<<"zones">> => ZoneIpv6sGrouped,
            <<"groups">> => GroupIpv6sGrouped},
  CreateIpsets6 = create_ipsets(Ipv6s,<<"v6">>),
  FlushIpsets4 = flush_ipsets(Ipv4s,<<"v4">>),
  FlushIpsets6 = flush_ipsets(Ipv6s,<<"v6">>),
  DestroyIpsets4 = destroy_ipsets(Ipv4s,<<"v4">>),
  DestroyIpsets6 = destroy_ipsets(Ipv6s,<<"v6">>),
  Ipsets = string:join([CreateIpsets4,CreateIpsets6,FlushIpsets4,FlushIpsets6,DestroyIpsets4,DestroyIpsets6],"\n"),
  lager:debug("Ipsets: ~s~n",[Ipsets]),
  Ipsets.

-spec merge_groups(InternalGroups :: tuple(), ExternalGroups :: tuple()) -> tuple().
merge_groups(
  {InternalGroupIpv4sGrouped,
   InternalGroupIpv6sGrouped,
   InternalZoneIpv4sGrouped,
   InternalZoneIpv6sGrouped},
  {ExternalGroupIpv4sGrouped,
   ExternalGroupIpv6sGrouped,
   ExternalZoneIpv4sGrouped,
   ExternalZoneIpv6sGrouped}) ->
  GroupIpv4sGrouped = maps:merge(InternalGroupIpv4sGrouped, ExternalGroupIpv4sGrouped),
  GroupIpv6sGrouped = maps:merge(InternalGroupIpv6sGrouped, ExternalGroupIpv6sGrouped),
  ZoneIpv4sGrouped = maps:merge(InternalZoneIpv4sGrouped, ExternalZoneIpv4sGrouped),
  ZoneIpv6sGrouped = maps:merge(InternalZoneIpv6sGrouped, ExternalZoneIpv6sGrouped),
  {GroupIpv4sGrouped, GroupIpv6sGrouped, ZoneIpv4sGrouped, ZoneIpv6sGrouped}.

-spec merge_ipsets(Groups1 :: tuple(), Groups2 :: tuple()) -> tuple().
merge_ipsets(
  {GroupIpv4sGrouped1,
   GroupIpv6sGrouped1,
   ZoneIpv4sGrouped1,
   ZoneIpv6sGrouped1},
  {GroupIpv4sGrouped2,
   GroupIpv6sGrouped2,
   ZoneIpv4sGrouped2,
   ZoneIpv6sGrouped2}) ->
  GroupIpv4sGrouped = merge_ipset(GroupIpv4sGrouped1, GroupIpv4sGrouped2),
  GroupIpv6sGrouped = merge_ipset(GroupIpv6sGrouped1, GroupIpv6sGrouped2),
  ZoneIpv4sGrouped = merge_ipset(ZoneIpv4sGrouped1, ZoneIpv4sGrouped2),
  ZoneIpv6sGrouped = merge_ipset(ZoneIpv6sGrouped1, ZoneIpv6sGrouped2),
  {GroupIpv4sGrouped, GroupIpv6sGrouped, ZoneIpv4sGrouped, ZoneIpv6sGrouped}.

-spec merge_ipset(Ipset1 :: map(), Ipset2 :: map()) -> map().
merge_ipset(Ipset1, Ipset2) ->
  Keys1 = maps:keys(Ipset1),
  Keys2 = maps:keys(Ipset2),
  Keys = lists:merge(lists:sort(Keys1),lists:sort(Keys2)),
  TupleList = lists:map(fun(Key) ->
                List1 = maps:get(Key,Ipset1,[]),
                List2 = maps:get(Key,Ipset2,[]),
                {Key, lists:usort(lists:merge(lists:sort(List1),lists:sort(List2)))}
            end, Keys),
  maps:from_list(TupleList).

-spec create_internal_groups() -> {InternalGroupIpv4sGrouped :: map(), InternalGroupIpv6sGrouped :: map(), 
                                   InternalZoneIpv4sGrouped :: map(), InternalZoneIpv6sGrouped :: map()}.
create_internal_groups() ->
  InternalGroupIpv4sGrouped = dog_group:all_ipv4s_grouped(),
  InternalGroupIpv6sGrouped = dog_group:all_ipv6s_grouped(),
  InternalZoneIpv4sGrouped = dog_zone:all_ipv4s_grouped(),
  InternalZoneIpv6sGrouped = dog_zone:all_ipv6s_grouped(),
  {InternalGroupIpv4sGrouped, InternalGroupIpv6sGrouped, InternalZoneIpv4sGrouped, InternalZoneIpv6sGrouped}.

-spec create_internal_ipsets(InternalGroups :: tuple()) -> InternalIpsetsMap :: map().
create_internal_ipsets({InternalGroupIpv4sGrouped,
                 InternalGroupIpv6sGrouped,
                 InternalZoneIpv4sGrouped,
                 InternalZoneIpv6sGrouped}) ->
  InternalIpv4s = #{<<"groups">> => InternalGroupIpv4sGrouped ,
            <<"zones">> => InternalZoneIpv4sGrouped },
  InternalIpv6s = #{<<"groups">> => InternalGroupIpv6sGrouped ,
            <<"zones">> => InternalZoneIpv6sGrouped },
  InternalIpsetsMap = #{<<"name">> => binary:list_to_bin(application:get_env(dog_trainer,env,"NOTSET")),
                       <<"v4">> => InternalIpv4s,
                       <<"v6">> => InternalIpv6s},
  InternalIpsetsMap.

-spec publish_to_external(InternalIpsetsMap :: map()) -> any().
publish_to_external(InternalIpsetsMap) ->
  lager:info("publishing to external"),
  publish_to_outbound_exchanges(InternalIpsetsMap).

-spec publish_to_queue(Ipsets :: list()) -> any().
publish_to_queue(Ipsets) ->
    lager:info("local publish"),
    lager:debug("Ipsets: ~p",[Ipsets]),
    UserData = #{
      ruleset4_ipset => false,
      ruleset6_ipset => false,
      ruleset4_iptables => false,
      ruleset6_iptables => false,
      ipsets => Ipsets
                },
    Count = 1,
    Pid = erlang:self(),
    Message = term_to_binary([{count, Count}, {local_time, calendar:local_time()}, {pid, Pid}, {user_data, UserData}]),
    Response = turtle:publish(ipset_publisher,
        <<"ipsets">>,
        <<"fanout">>,
        <<"text/json">>,
        Message,
        #{ delivery_mode => persistent }),
    imetrics:add(ipset_publish),
    Response.

-spec publish_to_outbound_exchanges(IpsetExternalMap :: map()) -> any().
publish_to_outbound_exchanges(IpsetExternalMap) ->
  {ok, ExternalEnvs} = dog_link:get_all_active_outbound(),
  IdsByGroup = dog_group:get_all_internal_ec2_security_group_ids(),
    %dog_common:merge_maps_of_lists([IdsByGroupMap,AllActiveUnionEc2Sgs]).
  lists:foreach(fun(Env) ->
    EnvName = maps:get(<<"name">>,Env),
    ExternalMap = maps:put(<<"ec2">>,IdsByGroup,IpsetExternalMap),
    %ExternalMap = maps:put(<<"ec2">>,jsx:encode(#{}),IpsetExternalMap),
    lager:debug("ExternalMap: ~p~n",[ExternalMap]),
    publish_to_outbound_exchange(EnvName,ExternalMap)
                    end, ExternalEnvs).

-spec publish_to_outbound_exchange(TargetEnvName :: binary(), IpsetExternalMap :: map()) -> any().
publish_to_outbound_exchange(TargetEnvName, IpsetExternalMap) ->
    lager:info("IpsetExternalMap: ~p",[IpsetExternalMap]),
    {ok,LocalEnvName} = application:get_env(dog_trainer,env),
    UserData = #{
      ipsets => jsx:encode(IpsetExternalMap),
      name => LocalEnvName
                },
    Count = 1,
    Pid = erlang:self(),
    Message = term_to_binary([
                              {count, Count},
                              {local_time, calendar:local_time()},
                              {pid, Pid},
                              {user_data, UserData}
                             ]),
    RoutingKey = binary:list_to_bin(LocalEnvName),
    BrokerConfigName = list_to_atom(binary:bin_to_list(TargetEnvName)),
    %thumper:start_link(BrokerConfigName),
    lager:info("~p, ~p, ~p, ~p",[BrokerConfigName, Message, <<"inbound">>, RoutingKey]),
    %Response = thumper:publish_to(BrokerConfigName, Message, <<"inbound">>, RoutingKey),
    PublisherName = erlang:binary_to_atom(<<TargetEnvName/binary, <<"_publisher">>/binary>>),
    Response = turtle:publish(
        PublisherName,
        <<"inbound">>,
        RoutingKey,
        <<"text/json">>,
        Message,
        #{ delivery_mode => persistent }),
    imetrics:add(ipset_outbound_publish),
    Response.

-spec hash_check(AgentIpsetHash :: binary()) -> boolean().
hash_check(AgentIpsetHash) ->
    %{ok, IpsetHashes} = get_hashes(),
    {ok,LatestHash} = latest_hash(),
    case AgentIpsetHash == LatestHash of
        false ->
            lager:info("Host IpsetHash ~p not equal to Latest IpsetHashes: ~p",[AgentIpsetHash,LatestHash]),
            false;
        true ->
            true
    end.

-spec update_ipsets(Env :: atom()) -> ok.
update_ipsets(Env) ->
    {ok,LatestHash} = latest_hash(),
    {MergedIpsetsList, InternalIpsetsMap} = create_ipsets(),
    write_ipsets_to_file(MergedIpsetsList),
    NormalizedIpset = normalize_ipset(MergedIpsetsList),
    NewIpsetHash = create_hash(NormalizedIpset),
    delete_old(),
    create(NewIpsetHash),
    lager:debug("LastestHash, NewIpsetHash: ~p, ~p",[LatestHash,NewIpsetHash]),
    case NewIpsetHash == LatestHash of
          false ->
            lager:debug("false"),
            publish_to_queue(MergedIpsetsList),
            case Env of 
              local_env ->
                lager:info("local_env"),
                pass;
              all_envs ->
                lager:info("all_envs"),
                publish_to_external(InternalIpsetsMap)
            end;
         true ->
            lager:debug("true"),
            pass
    end.

-spec force_update_ipsets() -> ok.
force_update_ipsets() ->
    lager:info("publishing: force_update_ipsets"),
    {MergedIpsets, InternalIpsets} = create_ipsets(),
    publish_to_queue(MergedIpsets),
    publish_to_external(InternalIpsets),
    ok.

-spec persist_ipset() -> ok | {error,list()}.
persist_ipset() ->
    PersistCmd = "sudo /sbin/ipset save | sudo tee /etc/iptables/rules.ipset",
    lager:info("PersistCmd: ~p", [PersistCmd]),
    case exec:run(PersistCmd, [sync, stderr]) of
        {error, [{PersistError,PersistCode},{stderr,CmdError}]} ->
            lager:error("PersistCmd: ~p", [PersistCmd]),
            lager:error("Error, Code, CmdError: ~p, ~p",[PersistError,PersistCode,CmdError]),
            {error, [{PersistError,PersistCode},{stderr,CmdError}]};
        {ok,PersistCmdResult} ->
            lager:info("PersistCmdResult: ~p", [PersistCmdResult]),
            ok
    end.

%-spec read_current_ipset() -> list() | {error,list(),{stderr,iolist()}}.
%read_current_ipset() ->
%    FileName = ?RUNDIR ++ "/ipset.txt",
%    case dog_file:read_file(FileName) of
%        {ok, Ipset} ->
%            binary:bin_to_list(Ipset);
%        {error, enoent} ->
%            []
%    end.
%
%-spec read_hash() -> binary().
%read_hash() ->
%    NormalizedIpset = normalize_ipset(read_current_ipset()),
%    IpsetHash = create_hash(NormalizedIpset),
%            {error, [{PersistError,PersistCode},{stderr,CmdError}]};
%        {ok,PersistCmdResult} ->
%            lager:info("PersistCmdResult: ~p", [PersistCmdResult]),
%            ok
%    end.

-spec read_current_ipset() -> list() | {error,list(),{stderr,iolist()}}.
read_current_ipset() ->
    FileName = ?RUNDIR ++ "/ipset.txt",
    case dog_file:read_file(FileName) of
        {ok, Ipset} ->
            binary:bin_to_list(Ipset);
        {error, enoent} ->
            []
    end.

-spec read_hash() -> binary().
read_hash() ->
    NormalizedIpset = normalize_ipset(read_current_ipset()),
    IpsetHash = create_hash(NormalizedIpset),
    lager:info("ipset hash: ~p",[IpsetHash]),
    IpsetHash.

-spec match_only_add(Line :: iolist()) -> boolean().
match_only_add(Line) ->
    case re:run(Line,"^add (.*)") of
        {match, _ } -> true;
        nomatch -> false
    end.

-spec normalize_ipset(Ipset :: iolist()) -> iolist().
normalize_ipset(Ipset) ->
    IpsetSplit = string:split(Ipset,"\n",all),
    IpsetSorted = lists:sort(IpsetSplit),
    IpsetAddOnly = lists:filter(fun(X) -> match_only_add(X) end, IpsetSorted),
    IpsetNotNew = [lists:flatten(string:replace(X,"n "," ",all)) || X <- IpsetAddOnly],
    IpsetNot32 = [lists:flatten(string:replace(X,"/32","",all)) || X <- IpsetNotNew],
    IpsetNot128 = [lists:flatten(string:replace(X,"/128","",all)) || X <- IpsetNot32],
    IpsetTrimmed = [string:trim(Line,trailing," ") || Line <- IpsetNot128],
    IpsetNormalized = lists:flatten(lists:join("\n",IpsetTrimmed)),
    IpsetNormalized.


