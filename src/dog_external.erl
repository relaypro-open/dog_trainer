-module(dog_external).

-include("dog_trainer.hrl").
%% ------------------------------------------------------------------

-define(VALIDATION_TYPE, <<"external">>).
-define(TYPE_TABLE, external).

%% API Function Exports
%% ------------------------------------------------------------------
-export([
        add_ipset_prefix/2,
        create/1,
        delete/1,
        do_nothing/2,
        dump_all/0,
        dump_all_active/0,
        empty_external/1,
        set_active_by_id/1,
        set_inactive_by_id/1,
        get_all_ips/0,
        get_by_id/1,
        get_by_name/1, 
        get_all/0,
        get_all_active/0,
        get_all_active_union_ec2_sgs/0,
        get_schema/0,
        get_thumper_spec/1,
        grouped_by_ipset_name/0,
        grouped_by_ipset_name/4,
        remove_external_broker_definition/1,
        setup_external_broker_connections/1,
        to_ipset_names/2,
        update_external_broker_definition/1,
        replace/2
        ]).

%% ------------------------------------------------------------------
%% Dev/Test Function Exports
%% ------------------------------------------------------------------
-export([
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec setup_external_broker_connections(LinkName :: binary()) -> ok. 
setup_external_broker_connections(EnvName) ->
  {ok,Link} = dog_link:get_by_name(EnvName),
  Thumper = proplists:get_value(brokers,application:get_all_env(thumper)),
  ThumperSpec = get_thumper_spec(Link),
  Thumper1 = lists:append(ThumperSpec,Thumper),
  application:set_env(thumper,brokers,Thumper1),
  LinkName = erlang:list_to_atom(
               binary:bin_to_list(
                 maps:get(<<"name">>,Link))), 
  thumper:start_link(LinkName),
  ok.

-spec update_external_broker_definition(LinkName :: binary()) -> ok. 
update_external_broker_definition(EnvName) ->
  {ok,Link} = dog_link:get_by_name(EnvName),
  Thumper = proplists:get_value(brokers,application:get_all_env(thumper)),
  ThumperSpec = get_thumper_spec(Link),
  Thumper1 = lists:delete(ThumperSpec,Thumper),
  Thumper2 = lists:append(ThumperSpec,Thumper1),
  application:set_env(thumper,brokers,Thumper2),
  ok.

-spec remove_external_broker_definition(LinkName :: binary()) -> ok. 
remove_external_broker_definition(EnvName) ->
  {ok,Link} = dog_link:get_by_name(EnvName),
  Thumper = proplists:get_value(brokers,application:get_all_env(thumper)),
  ThumperSpec = get_thumper_spec(Link),
  Thumper1 = lists:delete(ThumperSpec,Thumper),
  application:set_env(thumper,brokers,Thumper1),
  ok.

get_thumper_spec(Link) ->
  Connection = maps:get(<<"connection">>,Link),
  ThumperSpec = 
  [
   {erlang:list_to_atom(
      binary:bin_to_list(
        maps:get(<<"name">>,Link))), 
    [
     {rabbitmq_config,
      [
       {host, binary:bin_to_list(maps:get(<<"host">>,Connection))},
       {port, maps:get(<<"port">>,Connection)},
       {api_port, maps:get(<<"api_port">>,Connection)},
       {virtual_host, maps:get(<<"virtual_host">>,Connection)},
       {user, maps:get(<<"user">>,Connection)},
       {password, maps:get(<<"password">>,Connection)},
       {ssl_options, [{cacertfile, binary:bin_to_list(nested:get([<<"ssl_options">>,<<"cacertfile">>],Connection))},
                      {certfile, binary:bin_to_list(nested:get([<<"ssl_options">>,<<"certfile">>],Connection))},
                      {keyfile, binary:bin_to_list(nested:get([<<"ssl_options">>,<<"keyfile">>],Connection))},
                      {verify, erlang:list_to_atom(
                                 binary:bin_to_list(
                                   nested:get([<<"ssl_options">>,<<"verify">>],Connection)))},
                      {server_name_indication, erlang:list_to_atom(
                                                 binary:bin_to_list(
                                                   nested:get([<<"ssl_options">>,<<"server_name_indication">>],Connection)))},
                      {fail_if_no_peer_cert, nested:get([<<"ssl_options">>,<<"fail_if_no_peer_cert">>],Connection)}
                     ]}
      ]
     }
    ]
   }
  ],
  ThumperSpec.

-spec get_by_name(Name :: binary()) -> {ok, map()} | {error, atom()}.
get_by_name(Name) ->
    {ok, R} = dog_rethink:run(
                               fun(X) -> 
                                       reql:db(X, dog), 
                                       reql:table(X, ?TYPE_TABLE),
                                       reql:get_all(X, Name, #{index => <<"name">>})
                              end),
    {ok, R3} = rethink_cursor:all(R),
    Result = lists:flatten(R3),
    case Result of
        [] -> 
            {error, notfound};
        _ -> {ok, hd(Result)}
    end.

-spec get_by_id(Id :: binary()) -> {ok, map()} | {error, atom()}.
get_by_id(Id) ->
  {ok, R} = dog_rethink:run(
  fun(X) -> 
      reql:db(X, dog), 
      reql:table(X, ?TYPE_TABLE),
      reql:get(X, Id)
  end),
  case R of
     null -> 
          {error, notfound};
     _ -> 
      {ok, R}
  end.

-spec get_all() -> {ok, list()}.
get_all() ->
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, ?TYPE_TABLE),
                                      reql:pluck(X, [<<"name">>,<<"id">>])
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Externals = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    {ok, Externals}.

-spec get_all_active() -> {ok, []} | {ok, map()}.
get_all_active() ->
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, ?TYPE_TABLE),
                                      reql:filter(X,#{<<"state">> => <<"active">>}),
                                      reql:pluck(X, [<<"name">>,<<"id">>])
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Externals = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    {ok, Externals}.

-spec empty_external(EnvName :: binary()) -> map().
empty_external(EnvName) ->
  #{
   <<"name">> => EnvName,
   <<"state">> => <<"inactive">>,
   <<"v4">> => #{ <<"groups">> => #{}, <<"zones">> => #{} },
   <<"v6">> => #{ <<"groups">> => #{}, <<"zones">> => #{} },
   <<"address_handling">> => <<"union">>
  }.

-spec dump_all_active() -> {ok, list(), list()}.
dump_all_active() ->
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, external),
                                      reql:filter(X,#{<<"state">> => <<"active">>})
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Externals = case lists:flatten(Result) of
                [] -> [];
                Else -> 
                    %Else
                    lists:map(fun(E) ->
                      ExternalName = maps:get(<<"name">>,E),
                      lager:debug("ExternalName: ~p",[ExternalName]),
                      case dog_link:get_by_name(ExternalName) of
                               {ok, Map} -> 
                                  lager:debug("Map: ~p",[Map]),
                                    LinkAddressHandling = maps:get(<<"address_handling">>,Map),
                                    maps:put(<<"address_handling">>,LinkAddressHandling,E);
                               _ ->
                                 lager:debug("E: ~p",[E]),
                                 E
                             end
                    end, Else)
            end,
    {ExternalUnionEnvs, ExternalPrefixEnvs} = lists:splitwith(fun(E) -> maps:get(<<"address_handling">>,E) == <<"union">> end, Externals),
    {ok, ExternalUnionEnvs, ExternalPrefixEnvs}.

-spec dump_all() -> {ok, list(), list()}.
dump_all() ->
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, external)
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Externals = case lists:flatten(Result) of
                [] -> [];
                Else -> 
                    %Else
                    lists:map(fun(E) ->
                      ExternalName = maps:get(<<"name">>,E),
                      lager:debug("ExternalName: ~p",[ExternalName]),
                      case dog_link:get_by_name(ExternalName) of
                               {ok, Map} -> 
                                  lager:debug("Map: ~p",[Map]),
                                    LinkAddressHandling = maps:get(<<"address_handling">>,Map),
                                    maps:put(<<"address_handling">>,LinkAddressHandling,E);
                               _ ->
                                 lager:debug("E: ~p",[E]),
                                 E
                             end
                    end, Else)
            end,
    {ExternalUnionEnvs, ExternalPrefixEnvs} = lists:splitwith(fun(E) -> maps:get(<<"address_handling">>,E) == <<"union">> end, Externals),
    {ok, ExternalUnionEnvs, ExternalPrefixEnvs}.

-spec grouped_by_ipset_name() -> { 
                                               {ExternalUnionGroupIpv4sGrouped :: map(), ExternalUnionGroupIpv6sGrouped :: map(), 
                                               ExternalUnionZoneIpv4sGrouped :: map(), ExternalUnionZoneIpv6sGrouped :: map()},
                                               {ExternalPrefixGroupIpv4sGrouped :: map(), ExternalPrefixGroupIpv6sGrouped :: map(), 
                                               ExternalPrefixZoneIpv4sGrouped :: map(), ExternalPrefixZoneIpv6sGrouped :: map()}
                                               }.
grouped_by_ipset_name() ->
  {ok, ExternalUnionEnvs, ExternalPrefixEnvs} = dump_all_active(),
  ExternalUnionIpsets = grouped_by_ipset_name(ExternalUnionEnvs,fun do_nothing/2),
  ExternalPrefixIpsets = grouped_by_ipset_name(ExternalPrefixEnvs,fun add_ipset_prefix/2),
  {ExternalUnionIpsets, ExternalPrefixIpsets}.

-spec grouped_by_ipset_name(Envs :: list(), AddressHandlingFun :: fun()) -> 
  {ExternalGroupIpv4sGrouped :: map(), ExternalGroupIpv6sGrouped :: map(), 
   ExternalZoneIpv4sGrouped :: map(), ExternalZoneIpv6sGrouped :: map()}.
grouped_by_ipset_name(Envs, AddressHandlingFun) ->
  %TODO: union/prefix externals breaks down internal/external ipsets categories. must handle per-link/env.
  ExternalGroupIpv4sGrouped = grouped_by_ipset_name(Envs,<<"groups">>,<<"v4">>,AddressHandlingFun),
  ExternalGroupIpv6sGrouped = grouped_by_ipset_name(Envs,<<"groups">>,<<"v6">>,AddressHandlingFun),
  ExternalZoneIpv4sGrouped =  grouped_by_ipset_name(Envs,<<"zones">>,<<"v4">>,AddressHandlingFun),
  ExternalZoneIpv6sGrouped =  grouped_by_ipset_name(Envs,<<"zones">>,<<"v6">>,AddressHandlingFun),
  {ExternalGroupIpv4sGrouped, ExternalGroupIpv6sGrouped, ExternalZoneIpv4sGrouped, ExternalZoneIpv6sGrouped}.

-spec create(External :: map()) -> {ok | error, Key :: iolist() | name_exists }.
create(ExternalMap@0) ->
  Name = maps:get(<<"name">>, ExternalMap@0),
  {ok, ExistingExternals} = get_all(),
  ExistingNames = case ExistingExternals of
                    [] ->
                      [];
                    ExistingExternals ->
                      [maps:get(<<"name">>,External) || External <- ExistingExternals]
                  end,
  DefaultValuesExternalMap = #{
                               <<"state">> => <<"active">>
                              },
  MergedExternalMap = maps:merge(DefaultValuesExternalMap, ExternalMap@0),
  MergedExternalMap2  = dog_time:merge_timestamp(MergedExternalMap),
  case lists:member(Name, ExistingNames) of
    false ->
      {ok, R} = dog_rethink:run(
                  fun(X) -> 
                      reql:db(X, dog),
                      reql:table(X, ?TYPE_TABLE),
                      reql:insert(X, MergedExternalMap2)
                  end),
      Key = hd(maps:get(<<"generated_keys">>,R)),
      {ok, Key};
    true ->
      {error, name_exists}
  end.

-spec replace(Id :: binary(), UpdateMap :: map()) -> {true, iolist()} | {false, iolist()} | {false, no_replaced} | {validation_error, iolist()} .
replace(Id, UpdateMap) ->
    case get_by_id(Id) of
        {ok, OldExternal} ->
            NewExternal = maps:merge(OldExternal,UpdateMap),
            NewExternal2  = dog_time:merge_timestamp(NewExternal),
            NewExternal3 = maps:put(<<"id">>, Id, NewExternal2),
            case dog_json_schema:validate(?VALIDATION_TYPE,NewExternal3) of
                ok ->
                    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
                    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
                    {ok, R} = dog_rethink:run(
                          fun(X) -> 
                                  reql:db(X, dog),
                                  reql:table(X, ?TYPE_TABLE),
                                  reql:get(X, Id),
                                  reql:replace(X,NewExternal3)
                              end),
                    lager:debug("replaced R: ~p~n", [R]),
                    Replaced = maps:get(<<"replaced">>, R),
                    Unchanged = maps:get(<<"unchanged">>, R),
                    case {Replaced,Unchanged} of
                        {1,0} -> {true,Id};
                        {0,1} -> {false,Id};
                        _ -> {false, no_replaced}
                    end;
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {error, Error} ->
            {false, Error}
    end.

-spec delete(EnvName :: binary()) -> ok | error.
delete(EnvName) ->
  {ok, R} = dog_rethink:run(
                           fun(X) ->
                               reql:db(X, dog),
                               reql:table(X, ?TYPE_TABLE),
                               reql:get_all(X, EnvName, #{index => <<"name">>}),
                               reql:delete(X)
                           end),
  Deleted = maps:get(<<"deleted">>, R),
  Unchanged = maps:get(<<"unchanged">>, R),
  case {Deleted,Unchanged} of
      {1,0} -> ok;
      {0,1} -> ok;
      _ -> error
  end.

-spec grouped_by_ipset_name(Envs :: list(), Type :: binary(), Version :: binary(), AddressHandlingFun :: fun() ) -> map().
grouped_by_ipset_name(Envs,Type,Version,AddressHandlingFun) ->
  AllEnvs = lists:map(fun(Env) ->
    EnvName = maps:get(<<"name">>,Env),
    State = maps:get(<<"state">>,Env),
    lists:map(fun({Name,Addresses}) -> 
                  case State of
                    <<"active">> ->
                                 {AddressHandlingFun(EnvName,Name),Addresses};
                    _ ->
                                 {AddressHandlingFun(EnvName,Name),[]}
                  end
              end, maps:to_list(nested:get([Version,Type],Env)))
  end, Envs),
  maps:from_list(lists:flatten(AllEnvs)).

add_ipset_prefix(EnvName,LocalIpsetName) ->
  Separator = <<"#">>,
  <<EnvName/bitstring,Separator/bitstring,LocalIpsetName/bitstring>>.

do_nothing(_EnvName,LocalIpsetName) ->
  LocalIpsetName.

to_ipset_names(EnvName,Groups) ->
  maps:fold(
    fun(K, V, ok) ->
        NewKey = add_ipset_prefix(EnvName,K),
        #{NewKey => V}
    end, ok, Groups).

-spec set_active_by_id(ExtId :: binary() | none) -> ok.
set_active_by_id(ExtId ) when ExtId == none ->
  ok;
set_active_by_id(ExtId ) when is_binary(ExtId) ->
  {ok, Ext} = get_by_id(ExtId),
  NewExt = maps:merge(Ext,#{<<"state">> => <<"active">>}),
  replace(ExtId, NewExt),
  ok.

-spec set_inactive_by_id(ExtId :: binary() | none) -> ok.
set_inactive_by_id(ExtId ) when ExtId == none ->
  ok;
set_inactive_by_id(ExtId) when is_binary(ExtId) ->
  {ok, Ext} = get_by_id(ExtId),
  NewExt = maps:merge(Ext,#{<<"state">> => <<"inactive">>}),
  replace(ExtId, NewExt),
  ok.

%set_inactive_by_id/1,

%% ------------------------------------------------------------------
%% Dev/Test Definitions
%% ------------------------------------------------------------------
%%
%-spec publish_to_inbound_queue(IpsetExternalMap :: map()) -> any().
%publish_to_inbound_queue(IpsetExternalMap) ->
%    lager:debug("IpsetExternalMap: ~p",[IpsetExternalMap]),
%    ExternalEnvName = maps:get(<<"name">>,IpsetExternalMap),
%    {ok,LocalEnvName} = application:get_env(dog_trainer, env),
%    UserData = #{
%      ipsets => jsx:encode(IpsetExternalMap),
%      name => LocalEnvName
%                },
%    Count = 1,
%    Pid = erlang:self(),
%    Message = term_to_binary([
%                              {count, Count}, 
%                              {local_time, calendar:local_time()}, 
%                              {pid, Pid}, 
%                              {user_data, UserData}
%                             ]),
%    RoutingKey = ExternalEnvName,
%    lager:info("~p, ~p, ~p",[Message, <<"inbound">>, RoutingKey]),
%    Response = thumper:publish(Message, <<"inbound">>, RoutingKey),
%    Response.

%d2_ipsets() ->
%  #{
%  <<"name">> => <<"d2">>,
%  <<"v4">> => 
%  #{ 
%      <<"groups">> => 
%      #{
%          <<"test_group">> => [<<"1.1.1.1">>]
%         },
%      <<"zones">> =>
%      #{
%          <<"test_zone">> => [<<"9.9.9.9">>]
%         }
%     },
%  <<"v6">> =>
%  #{ 
%      <<"groups">> => 
%      #{
%          <<"test_group">> => [<<"fd42:aeb8:a6c5:b75d:216:3eff:fe5c:e3eb/64">>]
%         },
%      <<"zones">> =>
%      #{
%          <<"test_zone">> => [<<"ad42:aeb8:a6c5:b75d:216:3eff:fe5c:e3eb/64">>]
%         }
%     }
% }.
%
%d2_ipsets2() ->
%  #{
%  <<"name">> => <<"d2">>,
%  <<"v4">> => 
%  #{ 
%      <<"groups">> => 
%      #{
%          <<"test_group">> => [<<"10.1.1.1">>]
%         },
%      <<"zones">> =>
%      #{
%          <<"test_zone">> => [<<"10.9.9.9">>]
%         }
%     },
%  <<"v6">> =>
%  #{ 
%      <<"groups">> => 
%      #{
%          <<"test_group">> => [<<"bd42:aeb8:a6c5:b75d:216:3eff:fe5c:e3eb/64">>]
%         },
%      <<"zones">> =>
%      #{
%          <<"test_zone">> => [<<"cd42:aeb8:a6c5:b75d:216:3eff:fe5c:e3eb/64">>]
%         }
%     }
% }.
%
%d3_ipsets() ->
%  #{
%  <<"name">> => <<"d3">>,
%  <<"v4">> => 
%  #{ 
%      <<"groups">> => 
%      #{
%          <<"test_group">> => [<<"3.1.1.1">>]
%         },
%      <<"zones">> =>
%      #{
%          <<"test_zone">> => [<<"3.9.9.9">>]
%         }
%     },
%  <<"v6">> =>
%  #{ 
%      <<"groups">> => 
%      #{
%          <<"test_group">> => [<<"3d42:aeb8:a6c5:b75d:216:3eff:fe5c:e3eb/64">>]
%         },
%      <<"zones">> =>
%      #{
%          <<"test_zone">> => [<<"4d42:aeb8:a6c5:b75d:216:3eff:fe5c:e3eb/64">>]
%         }
%     }
% }.
%
%d3_ipsets2() ->
%  #{
%  <<"name">> => <<"d3">>,
%  <<"v4">> => 
%  #{ 
%      <<"groups">> => 
%      #{
%          <<"test_group">> => [<<"4.1.1.1">>]
%         },
%      <<"zones">> =>
%      #{
%          <<"test_zone">> => [<<"4.9.9.9">>]
%         }
%     },
%  <<"v6">> =>
%  #{ 
%      <<"groups">> => 
%      #{
%          <<"test_group">> => [<<"5d42:aeb8:a6c5:b75d:216:3eff:fe5c:e3eb/64">>]
%         },
%      <<"zones">> =>
%      #{
%          <<"test_zone">> => [<<"6d42:aeb8:a6c5:b75d:216:3eff:fe5c:e3eb/64">>]
%         }
%     }
% }.
%
%test_create_env() ->
%	publish_to_inbound_queue(d2_ipsets()). 
%
%test_create_env2() ->
%	publish_to_inbound_queue(d2_ipsets2()). 

-spec get_schema() -> binary().
get_schema() ->
  dog_json_schema:get_file(?VALIDATION_TYPE).

-spec get_all_ips() -> {ok, list()}. 
get_all_ips() ->
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, ?TYPE_TABLE),
                                      reql:filter(X,#{<<"state">> => <<"active">>}),
                                      reql:pluck(X, [<<"v4">>, <<"v6">>])
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Externals = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    E1 = lists:map(fun(External) ->
        V4_Groups = maps:values(nested:get([<<"v4">>,<<"groups">>],External)),
        V4_Zones = maps:values(nested:get([<<"v4">>,<<"zones">>],External)),
        V6_Groups = maps:values(nested:get([<<"v6">>,<<"groups">>],External)),
        V6_Zones = maps:values(nested:get([<<"v6">>,<<"zones">>],External)),
        lists:flatten([V4_Groups,V4_Zones,V6_Groups,V6_Zones])
                   end, Externals),
    {ok, lists:flatten(E1)}.

-spec get_all_active_union_ec2_sgs() -> list().
get_all_active_union_ec2_sgs() ->
    {ok, ExternalUnionEnvs, _ExternalPrefixEnvs} = dump_all_active(), 
    AllGroups = lists:map(fun(Env) ->
                      maps:get(<<"ec2">>,Env)
                      end, ExternalUnionEnvs),
    dog_common:merge_maps_of_lists(AllGroups).
