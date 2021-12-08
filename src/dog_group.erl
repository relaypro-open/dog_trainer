-module(dog_group).

-include("dog_trainer.hrl").

-define(VALIDATION_TYPE, <<"group">>).
-define(TYPE_TABLE, group).

%API
-export([
        create/1,
        delete/1,
        get_by_id/1,
        get_by_name/1,
        get_all/0,
        get_schema/0,
        replace/2,
        update/2
        ]).

-export([
        all_ec2_sg_mappings/0,
        all_ips/0,
        all_ipv4s/0,
        all_ipv4s_grouped/0,
        all_ipv4s_grouped_by_id/0,
        all_ipv6s/0,
        all_ipv6s_grouped/0,
        all_ipv6s_grouped_by_id/0,
        all_external_ips/0,
        all_external_ipv4s/0,
        all_external_ipv6s/0,
        test_all_internal_ips/0,
        all_internal_ips/0,
        all_internal_ipv4s/0,
        all_internal_ipv6s/0,
        get_all_group_interfaces/0,
        get_all_group_interfaces/1,
        get_all_grouped_by_id/0,
        get_all_inbound_ports_by_protocol/1,
        get_all_ips_by_id/1,
        get_all_ipv4s_by_id/1,
        get_all_ipv4s_by_name/1,
        get_all_ipv6s_by_id/1,
        get_all_ipv6s_by_name/1,
        get_all_external_ec2_security_group_ids/0,
        get_all_internal_ec2_security_group_ids/0,
        get_internal_ec2_security_group_ids_by_id/1,
        get_ec2_security_group_ids_by_name/1,
        get_ec2_security_group_ids_from_members/1,
        get_external_ips_by_id/1,
        get_external_ipv4s_by_id/1,
        get_external_ipv6s_by_id/1,
        get_group_interfaces_by_name/1,
        get_group_interfaces_by_name/2,
        get_internal_ips_by_id/1,
        get_internal_ips_by_name/1,
        get_internal_ipv4s_by_id/1,
        get_internal_ipv6s_by_id/1
        ]).

-export([
        all_active/0,
        all_groups_in_group_profiles/0,
        all_zones_in_group_profiles/0,
        all_roles_in_group_profiles/0,
        external_active/0,
        internal_active/0,
        get_active_groups/0,
        get_group_ids/0,
        get_group_names/0,
        get_hash4_ipsets/1,
        get_hash4_iptables/1,
        get_hash6_ipsets/1,
        get_hash6_iptables/1,
        get_hosts_by_id/1,
        get_id_by_name/1,
        get_ids_with_profile_id/1,
        get_name_by_id/1,
        get_profile_by_id/1,
        get_profile_by_name/1,
        get_ppps_inbound_ec2/2,
        group_name_exists/1,
        init/0,
        in_active_profile/1,
        merge/1,
        merge_join/2,
        replace_profile_by_profile_id/2,
        replace_profile_by_profile_id/3,
        role_group_effects_groups/1,
        role_groups_in_groups_profiles/0,
        set_hash4_ipsets/2,
        set_hash4_iptables/2,
        set_hash6_ipsets/2,
        set_hash6_iptables/2,
        update_group_ec2_security_groups/2,
        where_ec2_sg_id_used/1,
        where_zone_used/1,
        zone_group_effects_groups/1,
        zone_groups_in_groups_profiles/0
        ]).

-export([
        get_document_by_id/1,
        inverse_map_of_lists/1,
        maps_append/3,
        set_ec2_group_mappings_from_members/0,
        set_ec2_group_mappings_from_members/1,
        set_ec2_group_mappings_from_members/2,
        tuple_pairs_to_map_of_lists/1
        ]).

%test
-export([get_group_interfaces_by_id/1]).

-spec init() -> any().
init() ->
  pass.

-spec get_all_ips_by_id( Id :: binary() ) -> {ok, iolist()}.
get_all_ips_by_id(Id) ->
    case Id of
        <<"all-active">> ->
            {ok, dog_ips:uniq(lists:flatten( all_ipv4s() ++ all_ipv6s()) ) };
        <<"internal-active">> ->
            {ok, all_internal_ips() };
        <<"external-active">> ->
            {ok, all_external_ipv4s() };
        _ ->
            {ok,Ipv4s} = get_all_ipv4s_by_id(Id),
            {ok,Ipv6s} = get_all_ipv6s_by_id(Id),
            {ok, lists:sort(dog_ips:uniq(lists:flatten(Ipv4s ++ Ipv6s)))}
    end.

zone_groups_in_groups_profiles() ->
    {ok, Groups} = get_active_groups(),
    Profiles = lists:map(fun(Group) ->
                                 Name = maps:get(<<"name">>,Group),
                                 lager:debug("Name: ~p~n",[Name]),
                                 case get_profile_by_name(Name) of 
                                     {ok, Profile} ->
                                         {Name, Profile};
                                     {error,notfound} ->
                                        []
                                 end
                 end, Groups),
    GroupsInGroups = lists:map(fun({Name, Profile}) ->
                               GroupsInProfile = dog_profile:get_zone_groups_in_profile(Profile),
                               {Name, GroupsInProfile} end, lists:flatten(Profiles)),
    maps:from_list(GroupsInGroups).

all_zones_in_group_profiles() ->
  GroupMap = zone_groups_in_groups_profiles(),
  DuplicateList = lists:flatten([V || {_K,V} <- maps:to_list(GroupMap)]),
  sets:to_list(sets:from_list(DuplicateList)).

all_roles_in_group_profiles() ->
  GroupMap = role_groups_in_groups_profiles(),
  DuplicateList = lists:flatten([V || {_K,V} <- maps:to_list(GroupMap)]),
  sets:to_list(sets:from_list(DuplicateList)).

all_groups_in_group_profiles() ->
  all_zones_in_group_profiles() ++ all_roles_in_group_profiles().

role_groups_in_groups_profiles() ->
    {ok, Groups} = get_active_groups(),
    ProfilesRaw = lists:map(fun(Group) ->
                                 Name = maps:get(<<"name">>,Group),
                                 lager:debug("Name: ~p~n",[Name]),
                                 case get_profile_by_name(Name) of
                                    {ok, Profile} ->
                                         {Name, Profile};
                                    _ ->
                                        []
                                 end
                                 end, Groups),
    Profiles = lists:flatten(ProfilesRaw),
    lager:debug("Profiles: ~p",[Profiles]),
    GroupNamesInGroups = lists:map(fun({Name, Profile}) ->
                               GroupIdsInProfile = dog_profile:get_role_groups_in_profile(Profile),
                               %lager:info("GroupIdsInProfile: ~p",[GroupIdsInProfile]),
                               %GroupNamesInProfile = [element(2,get_name_by_id(Id)) || Id <- GroupIdsInProfile],
                               GroupNamesInProfile = lists:map(fun(GroupId) ->
                                                                       case get_name_by_id(GroupId) of
                                                                           {ok,GroupName} ->
                                                                               GroupName;
                                                                           _ ->
                                                                               []
                                                                       end
                                                                       end, GroupIdsInProfile),
                               {Name, GroupNamesInProfile} end, lists:flatten(Profiles)),
    lager:debug("GroupNamesInGroups: ~p",[GroupNamesInGroups]),
    maps:from_list(GroupNamesInGroups).

%role_groups_in_groups_profiles() ->
%    {ok, Groups} = get_active_groups(),
%    Profiles = lists:map(fun(Group) ->
%                                 Name = maps:get(<<"name">>,Group),
%                                 case get_profile_by_name(Name) of
%                                 {ok, Profile} ->
%                                     {Name, Profile};
%                                 _ ->
%                                         []
%                                 end
%                                 end, Groups),
%    lager:debug("Profiles: ~p",[Profiles]),
%    GroupNamesInGroups = lists:map(fun({Name, Profile}) ->
%                               GroupIdsInProfile = dog_profile:get_role_groups_in_profile(Profile),
%                               %lager:info("GroupIdsInProfile: ~p",[GroupIdsInProfile]),
%                               GroupNamesInProfile = [element(2,get_name_by_id(Id)) || Id <- GroupIdsInProfile],
%                               {Name, GroupNamesInProfile} end, lists:flatten(Profiles)),
%    lager:debug("GroupNamesInGroups: ~p",[GroupNamesInGroups]),
%    maps:from_list(GroupNamesInGroups).

inverse_map_of_lists(Map) ->
    MapList = maps:to_list(Map),
    Tuplelist = lists:map(fun({Key,Values}) ->
                 lists:map(fun(Value) -> {Value,Key} end, Values)
                  end, MapList),
    lists:flatten(Tuplelist).

tuple_pairs_to_map_of_lists(TupleList) ->
    tuple_pairs_to_map_of_lists(TupleList,#{}).

tuple_pairs_to_map_of_lists([],Accum) ->
    Accum;
tuple_pairs_to_map_of_lists(TupleList,Accum) ->
    [Head|Tail] = TupleList,
    {Key,Value} = Head,
    Accum@1 = maps_append(Key,Value,Accum),
    tuple_pairs_to_map_of_lists(Tail,Accum@1).

-spec role_group_effects_groups(GroupName :: binary()) -> ({ok, list()} | error).
role_group_effects_groups(GroupName) ->
    GroupsInGroups = role_groups_in_groups_profiles(),
    TupleList = inverse_map_of_lists(GroupsInGroups),
    GroupEffectingGroups = tuple_pairs_to_map_of_lists(TupleList),
    {ok,OtherGroupsEffected} = case maps:find(GroupName,GroupEffectingGroups) of
        error -> {ok,[]};
        Else -> Else
    end,
    {ok,sets:to_list(sets:from_list(lists:flatten([OtherGroupsEffected,GroupName])))}.

-spec zone_group_effects_groups(ZoneId :: binary()) -> ({ok, list()} | error).
zone_group_effects_groups(ZoneId) ->
    GroupsInGroups = zone_groups_in_groups_profiles(),
    TupleList = inverse_map_of_lists(GroupsInGroups),
    GroupEffectingGroups = tuple_pairs_to_map_of_lists(TupleList),
    case maps:find(ZoneId,GroupEffectingGroups) of
        error -> {ok,[]};
        Else -> Else
    end.

-spec maps_append(Key::_, Value::_, Map::map()) -> map().
maps_append(Key,Value,Map) ->
    Map@1 = case maps:find(Key,Map) of
        error ->
            maps:put(Key,[Value],Map);
        {ok, Values} ->
            maps:put(Key,lists:append(Values,[Value]),Map)
    end,
    Map@1.

-spec get_active_groups() -> {ok, list()}.
get_active_groups() ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
    fun(X) ->
        reql:db(X, dog),
        reql:table(X, ?TYPE_TABLE),
        reql:order_by(X,<<"profile_id">>,#{index => <<"profile_id">>}),
        reql:pluck(X, [<<"name">>,<<"id">>])
    end),
    {ok, Result} = rethink_cursor:all(R),
    Groups = case lists:flatten(Result) of
        [] -> [];
        Else -> Else
    end,
    {ok, Groups}.

-spec get_all() -> {ok, list()}.
get_all() ->
    {ok, R} = dog_rethink:run(
    fun(X) ->
        reql:db(X, dog),
        reql:table(X, ?TYPE_TABLE),
        reql:pluck(X, [<<"name">>,<<"id">>,<<"profile_id">>,<<"profile_name">>, <<"profile_version">>,<<"ec2_security_group_ids">>])
    end),
    {ok, Result} = rethink_cursor:all(R),
    Groups = case lists:flatten(Result) of
        [] -> [];
        Else -> Else
    end,
    Groups@1 = lists:append(Groups,[all_active()]),
    {ok, Groups@1}.

-spec get_all_grouped_by_id() -> map().
get_all_grouped_by_id() ->
    {ok, All} = get_all(),
    maps:from_list([{maps:get(<<"id">>,Group), Group} || Group <- All]).

-spec get_group_names() -> list().
get_group_names() ->
    {ok, Groups} = get_all(),
    GroupNames = [ maps:get(<<"name">>,Group) || Group <- Groups],
    GroupNames.

-spec get_group_ids() -> list().
get_group_ids() ->
    {ok, Groups} = get_all(),
    GroupIds = [ maps:get(<<"id">>,Group) || Group <- Groups],
    GroupIds.

-spec create(Group :: map()) -> {ok, Key :: iolist()}.
create(Group@0) when is_map(Group@0)->
    GroupName = maps:get(<<"name">>,Group@0),
    GroupResult = dog_group:get_by_name(GroupName),
    DefaultMap = #{
        <<"hash4_ipsets">> => <<"">>,
        <<"hash6_ipsets">> => <<"">>,
        <<"hash4_iptables">> => <<"">>,
        <<"hash6_iptables">> => <<"">>,
        <<"ipset_hash">> => <<"">>,
        <<"external_ipv4_addresses">> => [],
        <<"external_ipv6_addresses">> => []
    },
    case GroupResult of
        {error, notfound} ->
            Timestamp = dog_time:timestamp(),
            Group@1 = maps:put(<<"created">>,Timestamp,Group@0),
            Group@2 = case maps:find(<<"profile_name">>,Group@1) of
                error ->
                   NewMap = maps:merge(DefaultMap,Group@1),
                   lager:info("NewMap: ~p",[NewMap]),
                   NewMap;
                {ok, _ProfileName} ->
                    ProfileId = case maps:find(<<"profile_version">>,Group@1) of
                        error ->
                            "";
                        {ok, <<"latest">>} ->
                            ProfileName = maps:get(<<"profile_name">>,Group@1),
                            case dog_profile:get_latest_profile(ProfileName) of
                                {ok, DogProfile} ->
                                    maps:get(<<"id">>,DogProfile);
                                {error, notfound} ->
                                    #{error => not_found}
                            end;
                        {ok, Id} ->
                            Id
                    end,
                    
                    NewMap = maps:merge(DefaultMap,Group@1),
                    lager:info("NewMap: ~p",[NewMap]),
                    maps:merge(NewMap,#{<<"profile_id">> => ProfileId})
            end,
            case dog_json_schema:validate(?VALIDATION_TYPE,Group@2) of
                ok ->
                    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
                    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
                    {ok, R} = dog_rethink:run(
                                              fun(X) ->
                                                      reql:db(X, dog),
                                                      reql:table(X, ?TYPE_TABLE),
                                                      reql:insert(X, Group@2)
                                              end),
                    Key = hd(maps:get(<<"generated_keys">>,R)),
                    {ok, Key};
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {ok, _} ->
            {error, exists}
     end.

-spec group_name_exists(GroupName :: binary()) -> boolean().
group_name_exists(GroupName) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
    fun(X) ->
        reql:db(X, dog),
        reql:table(X, ?TYPE_TABLE),
        reql:get_all(X, GroupName, #{index => <<"name">>})
    end),
    {ok, Result} = rethink_cursor:all(R),
    case lists:flatten(Result) of
        [] -> false;
        _Else -> true
    end.

%-spec get_profile_by_id(GroupName :: binary() ) -> {'ok', binary()} | {'error', 'notfound'}.
-spec get_profile_by_id(binary()) -> {ok, binary()} | {error, notfound}.
get_profile_by_id(GroupId) ->
  try
    R = dog_rethink:run(
          fun(X) ->
              reql:db(X, dog),
              reql:table(X, ?TYPE_TABLE),
              reql:get(X, GroupId)%,
              %reql:get_field(X, <<"profile_id">>)
          end),
    ProfileId = nested:get([<<"profile_id">>],R),
    ProfileId
  of
    {ok,Id} -> {ok, Id};
    {badkey,_} -> {error,notfound}
    %{error,{runtime_error,_Error}} -> {error,notfound}
  catch
    Exception:Reason:Stacktrace -> 
                        lager:info("~p, ~p, ~p",[Exception, Reason, Stacktrace]),
                        {error,notfound}
  end.

-spec in_active_profile(Id :: binary()) -> {false, []} | {true, Profiles :: list() }.
in_active_profile(Id) ->
    {ok, Used} = dog_zone:where_used(Id),
    {ok, Active} = dog_profile:all_active(),
    Profiles = sets:to_list(sets:intersection(sets:from_list(Used), sets:from_list(Active))),
    case Profiles of
        [] -> 
            {false,[]};
        _ -> 
            {true, Profiles}
    end.

all_active() -> 
    ExternalIpv4s = all_external_ipv4s(),
    ExternalIpv6s = all_external_ipv6s(), 
    InternalIpv4s = all_internal_ipv4s(),
    InternalIpv6s = all_internal_ipv6s(),
    #{  <<"name">> => <<"all-active">>,
        <<"id">> => <<"all-active">>,
        <<"external_ipv4_addresses">> => ExternalIpv4s,
        <<"external_ipv6_addresses">> => ExternalIpv6s,
        <<"internal_ipv4_addresses">> => InternalIpv4s,
        <<"internal_ipv6_addresses">> => InternalIpv6s
         }.

internal_active() -> 
    InternalIpv4s = all_internal_ipv4s(),
    InternalIpv6s = all_internal_ipv6s(),
    #{  <<"name">> => <<"internal-active">>,
        <<"id">> => <<"internal-active">>,
        <<"internal_ipv4_addresses">> => InternalIpv4s,
        <<"internal_ipv6_addresses">> => InternalIpv6s
         }.

external_active() -> 
    ExternalIpv4s = all_external_ipv4s(),
    ExternalIpv6s = all_external_ipv6s(), 
    #{  <<"name">> => <<"external-active">>,
        <<"id">> => <<"external-active">>,
        <<"external_ipv4_addresses">> => ExternalIpv4s,
        <<"external_ipv6_addresses">> => ExternalIpv6s
         }.

-spec get_id_by_name(GroupName :: binary()) -> {ok, binary()}.
get_id_by_name(GroupName) ->
    case GroupName of
        <<"all-active">> ->
            {ok, <<"all-active">>};
        <<"internal-active">> ->
            {ok, <<"internal-active">>};
        <<"external-active">> ->
            {ok, <<"external-active">>};
        _ ->
            case get_by_name(GroupName) of
                {ok,Group} ->
                    Id = maps:get(<<"id">>,Group),
                    {ok, Id};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec get_name_by_id(GroupId :: binary()) -> {ok, binary()} | {error, atom()}.
get_name_by_id(GroupId) ->
    case GroupId of
        <<"all-active">> ->
            {ok, <<"all-active">>};
        <<"internal-active">> ->
            {ok, <<"internal-active">>};
        <<"external-active">> ->
            {ok, <<"external-active">>};
        _ ->
            R = dog_rethink:run(
            fun(X) ->
                reql:db(X, dog),
                reql:table(X, ?TYPE_TABLE),
                reql:get(X, GroupId),
                reql:get_field(X, <<"name">>)
            end),
            R
    end.

-spec get_profile_by_name(GroupName :: binary()) -> {ok, map()} | {error, atom()}.
get_profile_by_name(GroupName) ->
    {ok, R} = dog_rethink:run(
    fun(X) ->
        reql:db(X, dog),
        reql:table(X, ?TYPE_TABLE),
        reql:get_all(X, GroupName, #{index => <<"name">>}),
        reql:has_fields(X,[<<"profile_id">>]),
        reql:pluck(X, [<<"profile_version">>,<<"profile_name">>,<<"profile_id">>])
    end),
    {ok, Result} = rethink_cursor:all(R),
    case lists:flatten(Result) of
        [] ->
            {error,notfound};
        GroupRecord@0 ->
            GroupRecord@1 = hd(GroupRecord@0),
            ProfileId = maps:get(<<"profile_id">>,GroupRecord@1),
            dog_profile:get_by_id(ProfileId) 
    end.

get_hash4_ipsets(GroupName) ->
    get_hash(GroupName, <<"hash4_ipsets">>).
get_hash6_ipsets(GroupName) ->
    get_hash(GroupName, <<"hash6_ipsets">>).
get_hash4_iptables(GroupName) ->
    get_hash(GroupName, <<"hash4_iptables">>).
get_hash6_iptables(GroupName) ->
    get_hash(GroupName, <<"hash6_iptables">>).

-spec get_hash(GroupName :: binary(), Field :: binary()) -> {ok, binary()}.
get_hash(GroupName, Field) ->
    {ok, R} = dog_rethink:run(
    fun(X) ->
        reql:db(X, dog),
        reql:table(X, ?TYPE_TABLE),
        reql:get_all(X, GroupName, #{index => <<"name">>}),
        reql:get_field(X, Field)
    end),
    {ok, Result} = rethink_cursor:all(R),
    Hash = hd(lists:flatten(Result)),
    {ok, Hash}.

set_hash4_ipsets(GroupName, Hash) ->
    set_hash(GroupName, Hash, <<"hash4_ipsets">>).
set_hash6_ipsets(GroupName, Hash) ->
    set_hash(GroupName, Hash, <<"hash6_ipsets">>).
set_hash4_iptables(GroupName, Hash) ->
    set_hash(GroupName, Hash, <<"hash4_iptables">>).
set_hash6_iptables(GroupName, Hash) ->
    set_hash(GroupName, Hash, <<"hash6_iptables">>).

-spec set_hash(GroupName :: binary(), Hash :: binary(), Field :: binary()) -> {ok, any()}.
set_hash(GroupName, Hash, Field) ->
    lager:debug("GroupName: ~p, Hash: ~p",[GroupName,Hash]),
    {ok, GroupId} = dog_group:get_id_by_name(GroupName),
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
    fun(X) ->
        reql:db(X, dog),
        reql:table(X, ?TYPE_TABLE),
        reql:get(X,GroupId),
        reql:update(X, #{Field => Hash})
    end),
    {ok, R}.


-spec get_by_name(GroupName :: binary()) -> {ok, map()} | {error, atom()}.
get_by_name(GroupName) ->
    case GroupName of
        <<"all-active">> ->
            {ok, all_active() };
        <<"internal-active">> ->
            {ok, internal_active()};
        <<"external-active">> ->
            {ok, external_active() };
        _ ->
            %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
            %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
            {ok, R} = dog_rethink:run(
            fun(X) ->
                reql:db(X, dog),
                reql:table(X, ?TYPE_TABLE),
                reql:get_all(X, GroupName, #{index => <<"name">>})
            end),
            {ok, R3} = rethink_cursor:all(R),
            R4 = lists:flatten(R3),
            %lager:info("R4: ~p",[R4]),
            case R4 of
               [] ->
                   {error, notfound};
               _ ->
                   Result = hd(R4),
                   case Result of
                       {error, Error} ->
                           lager:error("group name not found: ~p, ~p",[GroupName, Error]),
                           {error, Error};
                       GroupJson -> 
                           {ok, GroupJson}
                   end
            end
    end.

-spec get_by_id(GroupId :: binary()) -> {ok, map()} | {error, atom()}.
get_by_id(GroupId) ->
    case GroupId of
        <<"all-active">> ->
            {ok, all_active() };
        <<"internal-active">> ->
            {ok, internal_active()};
        <<"external-active">> ->
            {ok, external_active() };
        <<"any">> ->
            {ok,#{
                 <<"external_ipv4_addresses">> => [ <<"0.0.0.0">> ],
                 <<"external_ipv6_addresses">> => [ <<"::/0">> ],
                 <<"name">> => <<"any">>
            }};
        _ ->
            get_document_by_id(GroupId)
    end.

-spec get_document_by_id(GroupId :: binary()) -> {atom(), any()}.
get_document_by_id(Id) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
    fun(X) ->
        reql:db(X, dog),
        reql:table(X, ?TYPE_TABLE),
        reql:get(X, Id)
    end),
    case R of
       null ->
           {error, notfound};
       _ -> {ok, R}
    end.

-spec replace(Id :: binary(), ReplaceMap :: map()) -> {'false','no_replaced' | 'notfound' | binary()} | {'true',binary()} | {'validation_error',binary()}.
replace(Id, ReplaceMap) ->
    case get_by_id(Id) of
        {ok, OldExternal} ->
            NewItem = maps:merge(OldExternal,ReplaceMap),
            NewItem2  = dog_time:merge_timestamp(NewItem),
            NewItem3 = maps:put(<<"id">>, Id, NewItem2),
            case dog_json_schema:validate(?VALIDATION_TYPE,NewItem3) of
                ok ->
                    {ok, R} = dog_rethink:run(
                          fun(X) -> 
                                  reql:db(X, dog),
                                  reql:table(X, ?TYPE_TABLE),
                                  reql:get(X, Id),
                                  reql:replace(X,NewItem3)
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

-spec update(GroupId :: binary(), UpdateMap :: map()) -> {atom(), any()} .
update(Id, UpdateMap) ->
    case get_by_id(Id) of
        {ok,OldService} ->
            NewService = maps:merge(OldService,UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE,NewService) of
                ok ->
                    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
                    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
                    {ok, R} = dog_rethink:run(
                          fun(X) ->
                                  reql:db(X, dog),
                                  reql:table(X, ?TYPE_TABLE),
                                  reql:get(X, Id),
                                  reql:update(X,UpdateMap)
                          end),
                    lager:debug("update R: ~p~n", [R]),
                    Replaced = maps:get(<<"replaced">>, R),
                    Unchanged = maps:get(<<"unchanged">>, R),
                    case {Replaced,Unchanged} of
                        {1,0} -> {true,Id};
                        {0,1} -> {false,Id};
                        _ -> {false, no_updated}
                    end;
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {error, Error} ->
            {false, Error}
    end.

-spec delete(GroupId :: binary()) -> (ok | {error, Error :: iolist()}).
delete(Id) ->
    case in_active_profile(Id) of
        {false,[]} -> 
            {ok, R} = dog_rethink:run(
                                      fun(X) -> 
                                              reql:db(X, dog),
                                              reql:table(X, ?TYPE_TABLE),
                                              reql:get(X, Id),
                                              reql:delete(X)
                                      end),
            lager:debug("delete R: ~p~n",[R]),
            Deleted = maps:get(<<"deleted">>, R),
            case Deleted of
                1 -> ok;
                _ -> {error,#{<<"error">> => <<"error">>}}
            end;
        {true,Profiles} ->
            lager:info("group ~p not deleted, in profiles: ~p~n",[Id,Profiles]),
            {error,#{<<"errors">> => #{<<"in active profile">> => Profiles}}}
     end.

- spec get_internal_ips_by_name( iolist() ) -> {'ok', list()} | {'error', atom()}.
get_internal_ips_by_name(GroupName) ->
   {ok, Interfaces} = get_group_interfaces_by_name(GroupName),
   {ok, IPs} = dog_ips:addresses_from_interfaces(Interfaces),
   {ok, IPs}.

- spec get_internal_ips_by_id( binary() ) -> {'ok', list()} | {'error', atom()}.
get_internal_ips_by_id(GroupId) ->
    case GroupId of
        <<"all-active">> ->
            {ok, all_ips() };
        <<"internal-active">> ->
            {ok, all_internal_ips() };
        <<"external-active">> ->
            {ok, all_external_ips() };
        _ ->
           {ok, Interfaces} = get_group_interfaces_by_id(GroupId),
           {ok, IPs} = dog_ips:addresses_from_interfaces(Interfaces),
           {ok, IPs}
    end.

- spec get_internal_ipv4s_by_id( binary() ) -> {'ok', list()} | {'error', atom()}.
get_internal_ipv4s_by_id(GroupId) ->
    case GroupId of
        <<"all-active">> ->
            {ok, all_ipv4s() };
        <<"internal-active">> ->
            {ok, all_internal_ipv4s() };
        <<"external-active">> ->
            {ok, [] };
        _ ->
            {ok, IPs} = get_internal_ips_by_id(GroupId),
            IPv4s = dog_ips:filter_ipv4(IPs),
            {ok, IPv4s}
    end.

- spec get_internal_ipv6s_by_id( binary() ) -> {'ok', list()} | {'error', atom()}.
get_internal_ipv6s_by_id(GroupId) ->
    case GroupId of
        <<"all-active">> ->
            {ok, all_ipv6s() };
        <<"internal-active">> ->
            {ok, all_internal_ipv6s() };
        <<"external-active">> ->
            {ok, [] };
        _ ->
            {ok, IPs} = get_internal_ips_by_id(GroupId),
            IPv6s = dog_ips:filter_ipv6(IPs),
            {ok, IPv6s}
    end.

- spec get_external_ips_by_id( binary() ) -> {'ok', list()} | {'error', atom()}.
get_external_ips_by_id(Id) ->
    case Id of
        <<"all-active">> ->
            {ok, dog_ips:uniq(lists:sort(lists:flatten(all_external_ipv4s() ++ all_external_ipv6s())))};
        <<"internal-active">> ->
            {ok, [] };
        <<"external-active">> ->
            {ok, all_external_ips() };
        _ ->
            {ok, Ipv4s} = get_external_ipv4s_by_id(Id),
            {ok, Ipv6s} = get_external_ipv6s_by_id(Id),
            {ok, dog_ips:uniq(lists:sort(lists:flatten(Ipv4s ++ Ipv6s)))}
    end.

- spec get_external_ipv6s_by_id( binary() ) -> {'ok', list()} | {'error', atom()}.
get_external_ipv6s_by_id(Id) ->
    case Id of
        <<"all-active">> ->
            {ok, all_external_ipv6s()};
        <<"internal-active">> ->
            {ok, [] };
        <<"external-active">> ->
            {ok, all_external_ipv6s() };
        _ ->
            %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
            %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
            {ok, R} = dog_rethink:run(
                                      fun(X) ->
                                              reql:db(X, dog),
                                              reql:table(X, ?TYPE_TABLE),
                                              reql:get(X, Id)
                                      end),
            case R of
               null ->
                    {notfound,[]};
               _ ->
                    ExternalAddresses = maps:get(<<"external_ipv6_addresses">>,R,[]),
                    {ok, dog_ips:filter_ipv6(ExternalAddresses)}
            end
    end.

- spec get_external_ipv4s_by_id( binary() ) -> {'ok', list()} | {'error', atom()}.
get_external_ipv4s_by_id(Id) ->
    case Id of
        <<"all-active">> ->
            {ok, all_external_ipv4s()};
        <<"internal-active">> ->
            {ok, [] };
        <<"external-active">> ->
            {ok, all_external_ipv6s() };
        _ ->
            %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
            %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
            {ok, R} = dog_rethink:run(
                                      fun(X) ->
                                              reql:db(X, dog),
                                              reql:table(X, ?TYPE_TABLE),
                                              reql:get(X, Id)
                                      end),
            case R of
               null ->
                    {notfound,[]};
               _ ->
                    ExternalAddresses = maps:get(<<"external_ipv4_addresses">>,R,[]),
                    {ok, dog_ips:filter_ipv4(ExternalAddresses)}
            end
    end.

-spec get_hosts_by_id(GroupId :: binary()) -> {ok, list()}.
get_hosts_by_id(GroupId) ->
    case GroupId of
        <<"all-active">> ->
            dog_host:get_all();
        <<"internal-active">> ->
            dog_host:get_all();
        <<"external-active">> ->
            {ok, []};
        _ ->
            {ok, Group} = get_by_id(GroupId),
            GroupName = maps:get(<<"name">>, Group),
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, host),
                    reql:filter(X,fun(Y) -> reql:bracket(Y, <<"group">>), reql:eq(Y, GroupName) end),
                    reql:pluck(X,[<<"name">>,<<"id">>,<<"hostkey">>])
                end),
            {ok, Result} = rethink_cursor:all(R),
            Hosts = lists:flatten(Result),
            Hosts@1 = lists:map(fun(X) -> X end, Hosts),
            case Hosts@1 of
                [] -> {ok, []};
                _ -> {ok, Hosts@1}
            end
    end.

-spec get_group_interfaces_by_id(GroupId :: binary()) -> {ok, iolist()}.
get_group_interfaces_by_id(GroupId) ->
    {ok, Group} = get_by_id(GroupId),
    GroupName = maps:get(<<"name">>, Group),
    get_group_interfaces_by_name(GroupName).

-spec get_all_group_interfaces() -> {'ok',[any()]}.
get_all_group_interfaces() ->
    get_all_group_interfaces(true).
-spec get_all_group_interfaces(OnlyActive :: boolean() ) -> {'ok',[any()]}.
get_all_group_interfaces(OnlyActive) ->
    case OnlyActive of
        true ->
            %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
            %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, host),
                    reql:filter(X,#{<<"active">> => <<"active">>}),
                    reql:get_field(X, <<"interfaces">>)
                end),
            {ok, Result} = rethink_cursor:all(R),
            Interfaces = lists:flatten(Result),
            lager:debug("Interfaces: ~p",[Interfaces]),
            Interfaces@1 = merge(Interfaces),
            lager:debug("Interfaces@1: ~p",[Interfaces@1]),
            case Interfaces@1 of
                [] -> {ok, []};
                _ -> {ok, Interfaces@1}
            end;
        false ->
            %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
            %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, host),
                    reql:get_field(X, <<"interfaces">>)
                end),
            {ok, Result} = rethink_cursor:all(R),
            Interfaces = lists:flatten(Result),
            Interfaces@1 = merge(Interfaces),
            case Interfaces@1 of
                [] -> {ok, []};
                _ -> {ok, Interfaces@1}
            end
    end.

-spec get_group_interfaces_by_name(GroupName :: iolist()) -> {'ok',[any()]}.
get_group_interfaces_by_name(GroupName) ->
    get_group_interfaces_by_name(GroupName, true).
-spec get_group_interfaces_by_name(GroupName :: iolist(), OnlyActive :: boolean() ) -> {'ok',[any()]}.
get_group_interfaces_by_name(GroupName, OnlyActive) ->
    %lager:info("GroupName: ~p~n",[GroupName]),
    case OnlyActive of
        true ->
            %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
            %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, host),
                    reql:get_all(X, GroupName, #{index => <<"group">>}),
                    reql:filter(X,#{<<"active">> => <<"active">>}),
                    reql:get_field(X, <<"interfaces">>)
                end),
            {ok, Result} = rethink_cursor:all(R),
            Interfaces = lists:flatten(Result),
            %lager:debug("Interfaces: ~p",[Interfaces]),
            Interfaces@1 = merge(Interfaces),
            %lager:debug("Interfaces@1: ~p",[Interfaces@1]),
            case Interfaces@1 of
                [] -> {ok, []};
                _ -> {ok, Interfaces@1}
            end;
        false ->
            %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
            %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, host),
                    reql:get_all(X, GroupName, #{index => <<"group">>}),
                    reql:get_field(X, <<"interfaces">>)
                end),
            {ok, Result} = rethink_cursor:all(R),
            Interfaces = lists:flatten(Result),
            Interfaces@1 = merge(Interfaces),
            case Interfaces@1 of
                [] -> {ok, []};
                _ -> {ok, Interfaces@1}
            end
    end.
%TODO
merge(Interfaces) ->
    %io:format("Interfaces: ~p~n",[Interfaces]),
    Interfaces@1 = [jsx:decode(I) || I <- Interfaces],
    %io:format("Interfaces@1: ~p~n",[Interfaces@1]),
    L3 = lists:foldl(fun(Interface,Acc) ->
                           merge_join(Interface,Acc) end,
                [], Interfaces@1),
    L4 = [{Key, lists:flatten(Values)} || {Key,Values} <- L3],
    L4.

-spec merge_join(L1 :: list(), L2 :: list()) -> list().
merge_join(L1, []) ->
   L1;
merge_join(L1, L2) ->
    %io:format("L1: ~p~n",[L1]),
    %io:format("L2: ~p~n",[L2]),
    R = sofs:relation(L1 ++ L2),
    F = sofs:relation_to_family(R),
    L3 = sofs:to_external(F),
    L3.

-spec all_ipv4s_grouped() -> map().
all_ipv4s_grouped() ->
    GroupNames = get_group_names(),
    Ipv4s = lists:map(fun(Name) -> {ok, Ips} = get_all_ipv4s_by_name(Name), {Name,Ips} end, GroupNames),
    maps:from_list(lists:flatten(Ipv4s)).

-spec all_ipv4s_grouped_by_id() -> map().
all_ipv4s_grouped_by_id() ->
    GroupIds = get_group_ids(),
    Ipv4s = lists:map(fun(Id) -> {ok, Ips} = get_all_ipv4s_by_id(Id), {Id,Ips} end, GroupIds),
    maps:from_list(lists:flatten(Ipv4s)).

-spec all_ipv6s_grouped() -> map().
all_ipv6s_grouped() ->
    GroupNames = get_group_names(),
    Ipv6s = lists:map(fun(Name) -> {ok, Ips} = get_all_ipv6s_by_name(Name), {Name,Ips} end, GroupNames),
    maps:from_list(lists:flatten(Ipv6s)).

-spec all_ipv6s_grouped_by_id() -> map().
all_ipv6s_grouped_by_id() ->
    GroupIds = get_group_ids(),
    Ipv6s = lists:map(fun(Id) -> {ok, Ips} = get_all_ipv6s_by_id(Id), {Id,Ips} end, GroupIds),
    maps:from_list(lists:flatten(Ipv6s)).


-spec all_ipv4s() -> list().
all_ipv4s() ->
    {ok, Interfaces} = get_all_group_interfaces(),
    {ok, Ips} = dog_ips:addresses_from_interfaces(Interfaces),
    AllExternalIPv4s = all_external_ipv4s(),
    Ipv4s = dog_ips:filter_ipv4(Ips),
    UniqueIpv4s = dog_ips:uniq(lists:flatten(Ipv4s ++ AllExternalIPv4s)),
    UniqueIpv4s.

-spec all_ipv6s() -> list().
all_ipv6s() ->
    {ok, Interfaces} = get_all_group_interfaces(),
    {ok, Ips} = dog_ips:addresses_from_interfaces(Interfaces),
    AllExternalIPv6s = all_external_ipv6s(),
    Ipv6s = dog_ips:filter_ipv6(Ips),
    UniqueIpv6s = dog_ips:uniq(lists:flatten(Ipv6s ++ AllExternalIPv6s)),
    UniqueIpv6s.

-spec all_ips() -> list().
all_ips() ->
    dog_ips:uniq(lists:flatten(all_ipv4s() ++ all_ipv6s() )).

%-spec all_internal_ips() -> list().
test_all_internal_ips() ->
    {ok, Groups} = get_all(),
    InternalIps = lists:map(fun(Group) ->
                        GroupId = maps:get(<<"id">>,Group),
                        {ok, Interfaces} = get_group_interfaces_by_id(GroupId),
                        {ok, Ips} = dog_ips:addresses_from_interfaces(Interfaces),
                        Ips end, Groups),
    dog_ips:uniq(lists:flatten(InternalIps)).

-spec all_internal_ips() -> list().
all_internal_ips() ->
    {ok, Interfaces} = dog_host:get_all_active_interfaces(),
    {ok, Ips} = dog_ips:addresses_from_interfaces(Interfaces),
    Ips.

-spec all_internal_ipv4s() -> list().
all_internal_ipv4s() ->
    Ips = all_internal_ips(),
    Ipv4s = dog_ips:filter_ipv4(Ips),
    Ipv4s.

-spec all_internal_ipv6s() -> list().
all_internal_ipv6s() ->
    Ips = all_internal_ips(),
    Ipv6s = dog_ips:filter_ipv6(Ips),
    Ipv6s.

-spec all_external_ips() -> list().
all_external_ips() ->
    {ok, ExternalIPs} = get_external_ips_by_id(<<"all-active">>),
    ExternalIPs.

-spec all_external_ipv4s() -> list().
all_external_ipv4s() ->
    %{ok, Ips} = dog_zone:get_all_ipv4s_by_name(<<"cpz_All_Active_serv">>),
    %Ips.
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
    fun(X) ->
        reql:db(X, dog),
        reql:table(X, ?TYPE_TABLE),
        reql:pluck(X, [<<"external_ipv4_addresses">>])
    end),
    {ok, Result} = rethink_cursor:all(R),
    ExternalIpv4s = case lists:flatten(Result) of
        [] -> [];
        Else -> Else
    end,
    Ipv4s = [ maps:get(<<"external_ipv4_addresses">>,Addresses) || Addresses <- ExternalIpv4s],
    UniqueIpv4s = dog_ips:uniq(Ipv4s),
    dog_ips:filter_ipv4(lists:flatten(UniqueIpv4s)).

-spec all_external_ipv6s() -> list().
all_external_ipv6s() ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
    fun(X) ->
        reql:db(X, dog),
        reql:table(X, ?TYPE_TABLE),
        reql:pluck(X, [<<"external_ipv6_addresses">>])
    end),
    {ok, Result} = rethink_cursor:all(R),
    ExternalIpv6s = case lists:flatten(Result) of
        [] -> [];
        Else -> Else
    end,
    Ipv6s = [ maps:get(<<"external_ipv6_addresses">>,Addresses, []) || Addresses <- ExternalIpv6s],
    UniqueIpv6s = dog_ips:uniq(Ipv6s),
    dog_ips:filter_ipv6(lists:flatten(UniqueIpv6s)).

-spec get_all_ipv4s_by_name( GroupName :: binary() ) -> {'ok',[any()]}.
get_all_ipv4s_by_name(GroupName) ->
    {ok, GroupId} = get_id_by_name(GroupName),
    get_all_ipv4s_by_id(GroupId).

-spec get_all_ipv6s_by_name( GroupName :: binary() ) -> {'ok',[any()]}.
get_all_ipv6s_by_name(GroupName) ->
    {ok, GroupId} = get_id_by_name(GroupName),
    get_all_ipv6s_by_id(GroupId).

-spec get_all_ipv4s_by_id( GroupId :: binary() ) -> {'ok',[any()]}.
get_all_ipv4s_by_id(GroupId) ->
    case GroupId of
        <<"all-active">> ->
            {ok, all_ipv4s() };
        <<"internal-active">> ->
            {ok, all_internal_ipv4s() };
        <<"external-active">> ->
            {ok, all_external_ipv4s() };
        _ ->
            {ok, IPs} = get_internal_ips_by_id(GroupId),
            {ok,External_IPv4s} = get_external_ipv4s_by_id(GroupId),
            IPv4s = dog_ips:filter_ipv4(lists:flatten(IPs ++ External_IPv4s)),
            {ok,IPv4s}
    end.

-spec get_all_ipv6s_by_id( GroupId :: binary() ) -> {'ok',[any()]}.
get_all_ipv6s_by_id(GroupId) ->
    case GroupId of
        <<"all-active">> ->
            {ok, all_ipv6s() };
        <<"internal-active">> ->
            {ok, all_internal_ipv6s() };
        <<"external-active">> ->
            {ok, all_external_ipv6s() };
        _ ->
            {ok, IPs} = get_internal_ips_by_id(GroupId),
            {ok,External_IPv6s} = get_external_ipv6s_by_id(GroupId),
            IPv6s = dog_ips:filter_ipv6(lists:flatten(IPs ++ External_IPv6s)),
            {ok,IPv6s}
    end.


-spec replace_profile_by_profile_id(OldId :: binary(), NewId :: binary()) ->  list().
replace_profile_by_profile_id(OldId, NewId) ->
    GroupIds = get_ids_with_profile_id(OldId),
    Results = lists:map(fun(GroupId) ->
        update(GroupId, #{<<"profile_id">> => NewId}) end, GroupIds),
    Results.

-spec replace_profile_by_profile_id(OldId :: binary(), NewId :: binary(), ProfileName :: iolist() ) ->  list().
replace_profile_by_profile_id(OldId, NewId, ProfileName) ->
    lager:debug("OldId: ~p, NewId: ~p, ProfileName: ~p", [OldId, NewId, ProfileName]),
    GroupIds = get_ids_with_profile_id(OldId),
    Results = lists:map(fun(GroupId) ->
        update(GroupId, #{<<"profile_id">> => NewId, <<"profile_name">> => ProfileName}) end, GroupIds),
    Results.

-spec get_ids_with_profile_id(Id :: binary()) -> iolist().
get_ids_with_profile_id(Id) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
    fun(X) ->
        reql:db(X, dog),
        reql:table(X, ?TYPE_TABLE),
        reql:order_by(X,<<"profile_id">>,#{index => <<"profile_id">>}),
        reql:pluck(X, [<<"id">>,<<"profile_id">>,<<"profile_version">>])
    end),
    {ok, Result} = rethink_cursor:all(R),
    Groups = case lists:flatten(Result) of
        [] -> [];
        Else -> Else
    end,
    Ids = [maps:get(<<"id">>,X) || X <- Groups, maps:get(<<"profile_id">>,X) == Id andalso maps:get(<<"profile_version">>,X) == <<"latest">>],
    Ids.

-spec where_zone_used(GroupName :: binary()) -> {ok, list()}.
where_zone_used(GroupName) ->
    {ok, Profiles} = dog_profile:get_all(),
    ProfileGroups = lists:map(fun(Profile) ->
        ProfileId = maps:get(<<"id">>,Profile),
        {ok, ProfileMap} = dog_profile:get_by_id(ProfileId),
        Groups = dog_profile:get_zone_groups_in_profile(ProfileMap),
        {ProfileId, Groups}
              end, Profiles),
    FilteredProfiles = lists:filtermap(fun({_Id, Groups}) -> lists:member(GroupName,Groups) end, ProfileGroups),
    {ok, [erlang:element(1,P) || P <- FilteredProfiles]}.

-spec get_schema() -> binary().
get_schema() ->
  dog_json_schema:get_file(?VALIDATION_TYPE).

-spec get_all_inbound_ports_by_protocol(GroupName :: string()) -> ProtocolPorts :: list().
get_all_inbound_ports_by_protocol(GroupName) ->
    case get_profile_by_name(GroupName) of
        {error,_Error} ->
            lager:info("No profile associated with group: ~p",[GroupName]),
            throw(profile_not_found);
        {ok, ProfileJson} ->
            dog_profile:get_all_inbound_ports_by_protocol(ProfileJson)
    end.

-spec get_ppps_inbound_ec2(Group :: map(), Region :: string()) -> list().
get_ppps_inbound_ec2(Group, Region) ->
            Ec2SecurityGroupList = maps:get(<<"ec2_security_group_ids">>,Group,[]),
            Ec2SecurityGroupMap = dog_common:lmm(Ec2SecurityGroupList,<<"region">>),
            Ec2Sg = maps:get(Region,Ec2SecurityGroupMap,[]),
            case Ec2Sg of
                [] ->
                    [];
                _ ->
                  %Region = maps:get(<<"region">>,Ec2Sg),
                  %SgId = maps:get(<<"sgid">>,Ec2Sg),
                  ProfileId = maps:get(<<"profile_id">>,Group),
                  {ok,ProfileJson} = dog_profile:get_by_id(ProfileId),
                  %{Region,SgId,dog_profile:get_spp_inbound_ec2(ProfileJson,Region)}
                  dog_profile:get_ppps_inbound_ec2(ProfileJson,Region)
            end.

-spec get_all_internal_ec2_security_group_ids() -> IdsByGroup :: map().
get_all_internal_ec2_security_group_ids() ->
    {ok, AllGroups} = get_all(),
    IdsByGroup = lists:map(fun(Group) ->
                      GroupName = maps:get(<<"name">>,Group),
                      case maps:get(<<"ec2_security_group_ids">>,Group,[]) of
                          [] ->
                              {GroupName,[]};
                          RegionGroups ->
                              {GroupName, RegionGroups}
                      end
              end, AllGroups),
    IdsByGroupMap = maps:from_list(IdsByGroup),
    IdsByGroupMap.

-spec get_all_external_ec2_security_group_ids() -> IdsByGroup :: map().
get_all_external_ec2_security_group_ids() ->
    AllActiveUnionEc2Sgs = dog_external:get_all_active_union_ec2_sgs(),
    AllActiveUnionEc2Sgs.

%GROUP BASED EC2 INFO
get_internal_ec2_security_group_ids_by_id(GroupId) ->
    {ok,Group} = get_by_id(GroupId),
    case maps:get(<<"ec2_security_group_ids">>,Group,[]) of
                       [] ->
                           [];
                       RegionGroups ->
                        RegionGroups
                   end.
%TODO
get_ec2_security_group_ids_by_name(GroupName) ->
    case get_id_by_name(GroupName) of
        {ok, GroupId} ->
            case get_internal_ec2_security_group_ids_by_id(GroupId) of
                [] ->
                    AllActiveUnionEc2Sgs = dog_external:get_all_active_union_ec2_sgs(),
                    case maps:get(GroupName, AllActiveUnionEc2Sgs,[]) of
                        [] ->
                            [];
                        SgIds ->
                            SgIds
                    end;
                SgIds ->
                    SgIds
            end;
        {error,notfound} ->
            AllActiveUnionEc2Sgs = dog_external:get_all_active_union_ec2_sgs(),
            case maps:get(GroupName, AllActiveUnionEc2Sgs,[]) of
                [] ->
                    [];
                SgIds ->
                    SgIds
            end
    end.

%HOST BASED EC2 INFO
get_ec2_security_group_ids_from_members(GroupName) ->
    {ok,GroupId} = get_id_by_name(GroupName),
    {ok, HostList} = get_hosts_by_id(GroupId),
    Ec2SecurityGroupList = lists:map(fun(Host) ->
                                             {ok,DogHost} = dog_host:get_by_id(maps:get(<<"id">>,Host)),
                                             Ec2SecurityGroupIds = maps:get(<<"ec2_security_group_ids">>,DogHost,[]),
                                             Ec2AvailablityZone = maps:get(<<"ec2_availability_zone">>,DogHost,[]),
                                             Ec2Region = case Ec2AvailablityZone of
                                                             [] -> 
                                                                 [];
                                                             _ -> 
                                                                 binary:list_to_bin(lists:droplast(binary:bin_to_list(Ec2AvailablityZone)))
                                                         end,
                                             lists:map(fun(Ec2SecurityGroupId) ->
                                                               {Ec2Region,Ec2SecurityGroupId}
                                                       end, Ec2SecurityGroupIds)
                                     end,HostList),
    sets:to_list(sets:from_list(lists:flatten(Ec2SecurityGroupList))).
    %lists:flatten(Ec2SecurityGroupList).


all_ec2_sg_mappings() ->
    lists:filter(fun(X) -> element(2,X) =/= [] end,([{G,dog_group:get_ec2_security_group_ids_from_members(G)} || G <- dog_group:get_group_names(), G =/= <<"all-active">>])).


set_ec2_group_mappings_from_members() ->
    Ec2SgMappings = all_ec2_sg_mappings(),
          lists:map(fun({GroupName,SgList}) ->
                set_ec2_group_mappings_from_members(GroupName,SgList)
              end, Ec2SgMappings).

set_ec2_group_mappings_from_members(GroupName) ->
    SgList = dog_group:get_ec2_security_group_ids_from_members(GroupName),
    set_ec2_group_mappings_from_members(GroupName,SgList).

set_ec2_group_mappings_from_members(GroupName,SgList) ->
        {ok, GroupId} = dog_group:get_id_by_name(GroupName),
        io:format("GroupId: ~p, SgList: ~p~n",[GroupId,maps:from_list(SgList)]),
        {ok,CurrentGroupMap} = dog_group:get_by_id(GroupId),
        SgListOfMaps = lists:map(fun({Region,SgId}) ->
                                  #{<<"region">> => Region,
                                    <<"sgid">> => SgId}
                          end,SgList),
        UpdateMap = maps:merge(CurrentGroupMap,
                         #{<<"ec2_security_group_ids">> =>
                           SgListOfMaps}),
        dog_group:replace(GroupId,UpdateMap).

-spec where_ec2_sg_id_used(SgId :: binary()) -> {ok, list()}.
where_ec2_sg_id_used(SgId) ->
    {ok, R} = dog_rethink:run(fun(X) -> 
                                      reql:db(X,dog),
                                      reql:table(X,group),
                                      reql:has_fields(X,[<<"ec2_security_group_ids">>]),
                                      reql:filter(X,fun(Y) -> 
                                                            reql:match(
                                                              reql:to_json_string(
                                                                reql:bracket(Y,<<"ec2_security_group_ids">>)),SgId)
                                                                                                                                                                     end),
                                     reql:get_field(X,<<"id">>) 
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Groups = case lists:flatten(Result) of
                 [] -> [];
                 Else -> Else
             end,
    {ok, Groups}.

-spec update_group_ec2_security_groups(GroupZoneIdentifier :: binary(), GroupType :: binary() ) -> 'ok'.
update_group_ec2_security_groups(GroupZoneIdentifier, GroupType) ->
    lager:info("GroupZoneIdentifier: ~p",[GroupZoneIdentifier]),
    Groups = case GroupType of
                <<"role">> -> 
                    {ok,G} = dog_group:role_group_effects_groups(GroupZoneIdentifier),
                    G;
                <<"zone">> -> 
                    %TODO: Zone support in Ec2
                    %{ok, G} = dog_group:zone_group_effects_groups(GroupZoneIdentifier),
                    %G
                    []
            end,
    AllActiveUnionEc2Sgs = dog_external:get_all_active_union_ec2_sgs(),
    GroupsWithEc2SgIds = lists:filter(fun(Group) ->
                                              case dog_group:get_ec2_security_group_ids_by_name(Group) of
                                                  [] ->
                                                      case maps:get(Group,AllActiveUnionEc2Sgs,[]) of
                                                          [] ->
                                                              false;
                                                          _ ->
                                                              true
                                                      end;
                                                  _ ->
                                                      true
                                              end
                                      end, lists:flatten(Groups)),
    lager:info("Effected Groups: ~p",[GroupsWithEc2SgIds]),
    lists:foreach(fun(Group) ->
       dog_ec2_sg:publish_ec2_sg_by_name(Group)
                  end, GroupsWithEc2SgIds),
    ok.
