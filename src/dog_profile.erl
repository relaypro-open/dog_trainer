-module(dog_profile).

-include("dog_trainer.hrl").

-define(VALIDATION_TYPE, <<"profile">>).
-define(TYPE_TABLE, profile).

%API
-export([
         create/1,
         delete/1,
         get_by_id/1,
         get_by_name/1, 
         get_all/0,
         get_schema/0,
         update/2,
         update/3
        ]).

-export([
         all_active/0,
         date_string/0,
         generate_ipv4_ruleset_by_name/1,
         generate_ipv6_ruleset_by_name/1,
         generate_ipv4_ruleset_by_id/1,
         generate_ipv6_ruleset_by_id/1,
         generate_ipv4_ruleset_by_group_id/1,
         generate_ipv4_ruleset_by_group_name/1,
         generate_ipv6_ruleset_by_group_id/1,
         generate_ipv6_ruleset_by_group_name/1,
         get_id_by_name/1,
         get_latest_profile/1,
         get_name_by_id/1,
         get_role_groups_in_profile/1,
         get_zone_groups_in_profile/1,
         get_all_inbound_ports_by_protocol/1,
         init/0,
         in_active_profile/1,
         is_active/1,
         create_ruleset/10,
         create_ruleset/11,
         create_ruleset/12,
         create_ruleset/13,
         normalize_ruleset/1,
         to_text/1,
         where_used/1,
         read_profile_from_file/1
        ]).

%test
-export([create_hash/1]).

-spec init() -> any().
init() ->
  pass.

-spec generate_ipv4_ruleset_by_name(Name :: binary()) -> {{ok,iolist()}, {ok,iolist()}} | {error, Error :: any()}.
generate_ipv4_ruleset_by_name(Name) ->
    Result = dog_profile:get_by_name(Name),
    case Result of
        {error, _Error} ->
            lager:info("No profile associated with group id: ~p",[Name]),
            throw(profile_not_found);
        {ok, ProfileJson} ->
            IpsetsRuleset = dog_ruleset:generate_ruleset(ProfileJson, ipsets, <<"v4">>),
            IptablesRuleset = dog_ruleset:generate_ruleset(ProfileJson, iptables, <<"v4">>),
            {IpsetsRuleset, IptablesRuleset}
    end.

-spec generate_ipv6_ruleset_by_id(Id :: binary()) -> {{ok,iolist()}, {ok,iolist()}}.
generate_ipv6_ruleset_by_id(Id) ->
    case get_by_id(Id) of
        {error, _Error} ->
            lager:info("No profile associated with group id: ~p",[Id]),
            throw(profile_not_found);
        {ok, ProfileJson} ->
             IpsetsRulesetResult = dog_ruleset:generate_ruleset(ProfileJson, ipsets, <<"v6">>),
             IptablesRulesetResult = dog_ruleset:generate_ruleset(ProfileJson, iptables, <<"v6">>),
             {IpsetsRulesetResult, IptablesRulesetResult}
    end.

-spec generate_ipv6_ruleset_by_name(Name :: binary()) -> {{ok,iolist()}, {ok,iolist()}}.
generate_ipv6_ruleset_by_name(Name) ->
    case get_by_name(Name) of
        {error, _Error} ->
            lager:info("No profile associated with group id: ~p",[Name]),
            throw(profile_not_found);
        {ok, ProfileJson} ->
            IpsetsRulesetResult = dog_ruleset:generate_ruleset(ProfileJson, ipsets, <<"v6">>),
            IptablesRulesetResult = dog_ruleset:generate_ruleset(ProfileJson, iptables, <<"v6">>),
            {IpsetsRulesetResult, IptablesRulesetResult}
    end.

-spec generate_ipv4_ruleset_by_id(GroupId :: binary()) -> {{ok,iolist()}, {ok,iolist()}}.
generate_ipv4_ruleset_by_id(Id) ->
    {ok, Json} = get_by_id(Id),
    Ipsets = dog_ruleset:generate_ruleset(Json, ipsets, <<"v4">>),
    Iptables = dog_ruleset:generate_ruleset(Json, iptables, <<"v4">>),
    {Ipsets, Iptables}.

-spec generate_ipv4_ruleset_by_group_name(GroupName :: binary()) -> {{ok,iolist()}, {ok,iolist()}}.
generate_ipv4_ruleset_by_group_name(GroupName) ->
  {Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap} = dog_ipset:id_maps(),
    generate_ipv4_ruleset_by_group_name(GroupName,Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap).

-spec generate_ipv4_ruleset_by_group_name(GroupName :: binary(), Ipv4RoleMap :: map(), Ipv6RoleMap :: map(), Ipv4ZoneMap :: map(), Ipv6ZoneMap :: map(), ZoneIdMap :: map(), GroupIdMap :: map(), ServiceIdMap :: map() ) -> {{ok,list()}, {ok,iolist()}}.
generate_ipv4_ruleset_by_group_name(GroupName,Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap) ->
    case dog_group:get_profile_by_name(GroupName) of
        {error,_Error} ->
            lager:info("No profile associated with group: ~p",[GroupName]),
            throw(profile_not_found);
        {ok, ProfileJson} ->
            write_profile_to_file(ProfileJson, GroupName),
            IpsetsRulesetResult = dog_ruleset:generate_ruleset(ProfileJson,ipsets,<<"v4">>,Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap),
            IptablesRulesetResult = case application:get_env(dog_trainer,generate_unset_tables,true) of 
                true -> dog_ruleset:generate_ruleset(ProfileJson,iptables,<<"v4">>,Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap);
                false -> {ok,[]}
            end,
            {IpsetsRulesetResult, IptablesRulesetResult}
    end.

-spec generate_ipv6_ruleset_by_group_id(GroupId :: binary()) -> {iolist(), iolist()}.
generate_ipv6_ruleset_by_group_id(GroupId) ->
    ProfileId = case dog_group:get_profile_by_id(GroupId) of
                  {'error','notfound'} -> profile_not_found(GroupId);
                  {ok, Id} -> Id
                end,
    {ok, Json} = get_by_id(ProfileId),
    {ok, Ipsets} = dog_ruleset:generate_ruleset(Json, ipsets, <<"v4">>),
    {ok, Iptables} = dog_ruleset:generate_ruleset(Json, iptables, <<"v4">>),
    {Ipsets, Iptables}.

-spec profile_not_found(GroupId :: binary()) -> no_return(). 
profile_not_found(GroupId) ->
          lager:info("No profile associated with group id: ~p",[GroupId]),
          throw(profile_not_found).

-spec generate_ipv6_ruleset_by_group_name(GroupName :: binary()) -> {{ok,iolist()}, {ok,iolist()}}.
generate_ipv6_ruleset_by_group_name(GroupName) ->
    {Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap} = dog_ipset:id_maps(),
    generate_ipv6_ruleset_by_group_name(GroupName,Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap).

-spec generate_ipv6_ruleset_by_group_name(GroupName :: binary(), Ipv4RoleMap :: map(), Ipv6RoleMap :: map(), Ipv4ZoneMap :: map(), Ipv6ZoneMap :: map(), ZoneIdMap :: map(), GroupIdMap :: map(), ServiceIdMap :: map() ) -> {{ok,iolist()}, {ok,iolist()}}.
generate_ipv6_ruleset_by_group_name(GroupName,Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap) ->
    Response = dog_group:get_profile_by_name(GroupName),
    case Response of
        {error, _Reason} ->
            lager:info("No profile associated with group name: ~p",[GroupName]),
            throw(profile_not_found);
        {ok, ProfileJson} ->
            write_profile_to_file(ProfileJson, GroupName),
            IpsetsRulesetResult = dog_ruleset:generate_ruleset(ProfileJson,ipsets,<<"v6">>,Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap),
            IptablesRulesetResult = case application:get_env(dog_trainer,generate_unset_tables,true) of 
                true -> dog_ruleset:generate_ruleset(ProfileJson,iptables,<<"v6">>,Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap);
                false -> {ok,[]}
            end,
            {IpsetsRulesetResult, IptablesRulesetResult}
    end.

%-spec generate_ipv4_ruleset_by_group_id(GroupId :: binary()) -> {iolist(), iolist()}.
%generate_ipv4_ruleset_by_group_id(GroupId) ->
%    {ok, ProfileId} = dog_group:get_profile_by_id(GroupId),
%    {ok, Json} = get_by_id(ProfileId),
%    {ok, Ipsets} = dog_ruleset:generate_ruleset(Json, ipsets, <<"v4">>),
%    {ok, Iptables} = dog_ruleset:generate_ruleset(Json, iptables, <<"v4">>),
%    {Ipsets, Iptables}.

-spec generate_ipv4_ruleset_by_group_id(GroupId :: binary()) -> {iolist(), iolist()}.
generate_ipv4_ruleset_by_group_id(GroupId) ->
    ProfileId = case dog_group:get_profile_by_id(GroupId) of
                  {'error','notfound'} -> profile_not_found(GroupId);
                  {ok, Id} -> Id
                end,
    {ok, Json} = get_by_id(ProfileId),
    {ok, Ipsets} = dog_ruleset:generate_ruleset(Json, ipsets, <<"v4">>),
    {ok, Iptables} = dog_ruleset:generate_ruleset(Json, iptables, <<"v4">>),
    {Ipsets, Iptables}.

-spec read_profile_from_file(GroupName :: binary()) -> Contents :: binary().
read_profile_from_file(GroupName) ->
    FileName = ?RUNDIR ++ "/profile." ++ binary_to_list(GroupName) ++ ".txt",
    {ok, Contents} = file:read_file(FileName),
    Contents.

-spec write_profile_to_file(Profile :: map(), GroupName :: binary()) -> ok.
write_profile_to_file(Profile, GroupName) ->
    FileName = ?RUNDIR ++ "/profile." ++ binary_to_list(GroupName) ++ ".txt",
    ok = file:write_file(FileName, jsx:encode(Profile)),
    ok.


remove_comments(Ruleset) ->
  NoCommentRulesList = lists:filter(fun(X) -> case re:run(X,"^#") of nomatch -> true; _ -> false end end, split(Ruleset,"\n", all) ),
  NoCommentRules = lists:flatten(lists:join("\n",
                         NoCommentRulesList)),
  NoCommentRules.
    
%remove_docker(Ruleset) ->
%  lists:filter(fun(Line0) ->
%	       {ok, Re} = re:compile("docker", [caseless, unicode]),
%	       case re:run(Line0, Re) of
%                  nomatch -> true;
%                  _  -> false 
%              end
%            end, Ruleset).

remove_docker(Ruleset) ->
    lists:map(fun(Line0) ->
        Line1 = re:replace(Line0, "^-A DOCKER(.*)","",[{return,list}]),
        Line2 = re:replace(Line1, "^:DOCKER(.*)","",[{return,list}]),
        Line3 = case Line2 of
            "-A FORWARD -j REJECT --reject-with icmp-port-unreachable" ->
                Line2;
            _ ->
                re:replace(Line2, "^-A FORWARD(.*)","",[{return,list}])
        end,
        Line3
              end, Ruleset).

remove_empty_lists(List) ->
  [L || L <- List, L =/= []].

remove_quotes(Line0) ->
    Line1 = re:replace(Line0, "\"", "", [{return, list},global]),
    Line2 = re:replace(Line1, "\'", "", [{return, list},global]),
    Line2.

-spec zero_counters(Ruleset :: iolist()) -> iolist().
zero_counters(Ruleset) ->
    re:replace(Ruleset, "(:.*) \\[.*\\]", "\\1 [0:0]",
           [{return, list}, global]).

normalize_ruleset(Ruleset) ->
    RulesetNoComments = remove_comments(Ruleset),
    RulesetZeroed = zero_counters(RulesetNoComments),
    RulesetSplit = string:split(RulesetZeroed,"\n",all),
    RulesetNoQuotes = [remove_quotes(Line) || Line <- RulesetSplit],
    RulesetTrimmed = [string:trim(Line,trailing," ") || Line <- RulesetNoQuotes],
    RulesetNoDocker = remove_docker(RulesetTrimmed),
    RulesetNoBlankLines = remove_empty_lists(RulesetNoDocker),
    RulesetNormalized = lists:flatten(lists:join("\n",RulesetNoBlankLines)),
    RulesetNormalized.

-spec create_hash(Ruleset :: iodata() ) -> binary().
create_hash(Ruleset) ->
    RulesetTrimmed = normalize_ruleset(Ruleset),
    lager:info("RulesetTrimmed: ~p",[RulesetTrimmed]),
    BitString = base16:encode(crypto:hash(sha256, RulesetTrimmed)),
    Binary = binary:list_to_bin(erlang:bitstring_to_list(BitString)),
    Binary.

-spec create_ruleset(RoutingKey :: binary(), Group :: binary(), Ipv4RoleMap :: map(), Ipv6RoleMap :: map(), Ipv4ZoneMap :: map(), Ipv6ZoneMap :: map(), ZoneIdMap :: map(), GroupIdMap :: map(), ServiceIdMap :: map(), Ipsets :: iolist() ) ->
  error | {R4IpsetsRuleset :: iolist(), R6IpsetsRuleset :: iolist(), R4IptablesRuleset :: iolist(), R6IptablesRuleset :: iolist()}.
    create_ruleset(RoutingKey, Group, Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap,Ipsets) -> 
        create_ruleset(RoutingKey, Group, <<"*">>, <<"*">>, <<"*">>, Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap,Ipsets).

-spec create_ruleset(RoutingKey :: binary(), Group :: binary(), Environment :: binary(), Ipv4RoleMap :: map(), Ipv6RoleMap :: map(), Ipv4ZoneMap :: map(), Ipv6ZoneMap :: map(), ZoneIdMap :: map(), GroupIdMap :: map(), ServiceIdMap :: map(), Ipsets :: iolist() ) ->
  error | {R4IpsetsRuleset :: iolist(), R6IpsetsRuleset :: iolist(), R4IptablesRuleset :: iolist(), R6IptablesRuleset :: iolist()}.
    create_ruleset(RoutingKey, Group, Environment, Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap,Ipsets) -> 
    create_ruleset(RoutingKey, Group, Environment, <<"*">>, <<"*">>, Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap,Ipsets).

-spec create_ruleset(RoutingKey :: binary(), Group :: binary(), Environment :: binary(), Location :: binary(), Ipv4RoleMap :: map(), Ipv6RoleMap :: map(), Ipv4ZoneMap :: map(), Ipv6ZoneMap :: map(), ZoneIdMap :: map(), GroupIdMap :: map(), ServiceIdMap :: map(), Ipsets :: iolist() ) ->
  error | {R4IpsetsRuleset :: iolist(), R6IpsetsRuleset :: iolist(), R4IptablesRuleset :: iolist(), R6IptablesRuleset :: iolist()}.
    create_ruleset(RoutingKey, Group, Environment, Location, Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap,Ipsets) -> 
    create_ruleset(RoutingKey, Group, Environment, Location, <<"*">>, Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap,Ipsets).

-spec create_ruleset(RoutingKey :: binary(), Group :: binary(), Environment :: binary(), Location :: binary(), HostKey :: binary(), Ipv4RoleMap :: map(), Ipv6RoleMap :: map() ,Ipv4ZoneMap :: map() ,Ipv6ZoneMap :: map() ,ZoneIdMap :: map() ,GroupIdMap :: map(), ServiceIdMap :: map(), Ipsets :: iolist()) -> 
  error | {R4IpsetsRuleset :: iolist(), R6IpsetsRuleset :: iolist(), R4IptablesRuleset :: iolist(), R6IptablesRuleset :: iolist()}.
create_ruleset(RoutingKey, Group, _Environment, _Location, _HostKey,Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap,_Ipsets) ->
    lager:info("creating Ipv4,Ipv6 rulesets, ipsets: ~p",[RoutingKey]),
    {R4IpsetsResult, R4IptablesResult} = generate_ipv4_ruleset_by_group_name(Group,Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap),
    {R6IpsetsResult, R6IptablesResult} = generate_ipv6_ruleset_by_group_name(Group,Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap),
    AnyError = lists:any(fun(X) -> X == error end,[R4IpsetsResult,R6IpsetsResult,R4IptablesResult,R6IptablesResult]),
    AnyNull = lists:any(fun(X) -> X == null end,[R4IpsetsResult,R6IpsetsResult,R4IptablesResult,R6IptablesResult]),
    case AnyError of
        true ->
            lager:info("Error generating at least one Ipv4,Ipv6 ruleset or ipsets, not publishing: ~p",[RoutingKey]),
            error;
        false ->
            case AnyNull of
                true ->
                    lager:info("Found null Ipset or IpRuleset, not publishing: ~p",[RoutingKey]),
                    {false, false, false, false};
                false ->
                    {ok, R4IpsetsRuleset} = R4IpsetsResult, 
                    {ok, R6IpsetsRuleset} = R6IpsetsResult,
                    {ok, R4IptablesRuleset} = R4IptablesResult,  
                    {ok, R6IptablesRuleset} = R6IptablesResult,  
                    Hash4Ipsets = create_hash(R4IpsetsRuleset),
                    Hash6Ipsets = create_hash(R6IpsetsRuleset),
                    Hash4Iptables = create_hash(R4IptablesRuleset),
                    Hash6Iptables = create_hash(R6IptablesRuleset),
                    {ok, _} = dog_group:set_hash4_ipsets(Group, Hash4Ipsets),
                    {ok, _} = dog_group:set_hash6_ipsets(Group, Hash6Ipsets),
                    {ok, _} = dog_group:set_hash4_iptables(Group, Hash4Iptables),
                    {ok, _} = dog_group:set_hash6_iptables(Group, Hash6Iptables),
                    dog_ruleset:write_ruleset_set_v4_to_file(R4IpsetsRuleset,Group),
                    dog_ruleset:write_ruleset_set_v6_to_file(R6IpsetsRuleset,Group),
                    dog_ruleset:write_ruleset_unset_v4_to_file(R4IptablesRuleset,Group),
                    dog_ruleset:write_ruleset_unset_v6_to_file(R6IptablesRuleset,Group),
                    {R4IpsetsRuleset, R6IpsetsRuleset, R4IptablesRuleset, R6IptablesRuleset}
        end
    end.

-spec date_string() -> iolist().
date_string() ->
    {Date, Time} = calendar:universal_time(),
    {Year, Month, Day} = Date,
    {Hour, Minute, Second} = Time,
    DayNumber = calendar:day_of_the_week(Date),
    DayName = case DayNumber of
                  1 -> "Mon";
                  2 -> "Tue";
                  3 -> "Wed";
                  4 -> "Thu";
                  5 -> "Fri";
                  6 -> "Sat";
                  7 -> "Sun"
              end,
    MonthName = case Month of
                    1 -> "Jan";
                    2 -> "Feb";
                    3 -> "Mar";
                    4 -> "Apr";
                    5 -> "May";
                    6 -> "Jun";
                    7 -> "Jul";
                    8 -> "Aug";
                    9 -> "Sep";
                    10 -> "Oct";
                    11 -> "Nov";
                    12 -> "Dec"
                end,
    DateString = io_lib:format("~s ~s ~B ~B:~B:~B ~B UTC",[DayName,MonthName,Day,Hour,Minute,Second,Year]),
    DateString.

-spec create(Profile :: map()) -> {'ok', iolist() } | {atom(), binary()}.
create(Profile@0) ->
    lager:debug("Profile@0: ~p~n",[Profile@0]),
    Timestamp = dog_time:timestamp(),
    case dog_json_schema:validate(?VALIDATION_TYPE,Profile@0) of
        ok ->
            Profile@1 = maps:put(<<"created">>,Timestamp,Profile@0),
            %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
            %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
            {ok, R} = dog_rethink:run(
                                      fun(X) ->
                                              reql:db(X, dog),
                                              reql:table(X, ?TYPE_TABLE),
                                              reql:insert(X, Profile@1)
                                      end),
            Key = hd(maps:get(<<"generated_keys">>,R)),
            {ok, Key};
        {error, Error} ->
            lager:error("~p",[Error]),
            Response = dog_parse:validation_error(Error),
            {validation_error, Response}
    end.

-spec get_all() -> {'ok',list()}.
get_all() ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
                              fun(X) ->
                                      reql:db(X, dog),
                                      reql:table(X, ?TYPE_TABLE),
                                      reql:pluck(X, [<<"name">>,<<"id">>,<<"created">>])
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Profiles = case lists:flatten(Result) of
                   [] -> [];
                   Else -> Else
               end,
    {ok, Profiles}.

-spec get_id_by_name(ProfileName :: binary()) -> {ok, iolist()} | {error, atom()}.
get_id_by_name(ProfileName) ->
    Result = get_by_name(ProfileName),
    case Result of
        {ok, Profile} ->
            {ok, maps:get(<<"id">>,Profile)};
        {error, Error} ->
            lager:error("profile name not found: ~p, ~p",[ProfileName,Error]),
            {error, Error}
    end.

-spec get_name_by_id(ProfileId :: binary()) -> {ok, iolist()} | {error, atom()}.
get_name_by_id(ProfileId) ->
    case get_by_id(ProfileId) of
        {ok, Profile} ->
            {ok, maps:get(<<"name">>,Profile)};
        {error, Error} ->
            {error, Error}
    end.

-spec get_by_name(ProfileName :: binary()) -> {'ok', map() } | {error, atom()} .
get_by_name(ProfileName) ->
    get_latest_profile(ProfileName).

-spec all_active() -> {ok, Profiles :: list()}.
all_active() ->
    {ok, R} = dog_rethink:run(
                             fun(X) ->
                                     reql:db(X, dog),
                                     reql:table(X, group),
                                     reql:get_field(X, <<"profile_id">>)
                             end),
    %{ok, R}.
    {ok, Result} = rethink_cursor:all(R),
    Profiles = case lists:flatten(Result) of
                 [] -> [];
                 Else -> Else
             end,
    {ok, Profiles}.

get_latest_profile(Name) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    R = dog_rethink:run(
                             fun(X) ->
                                     reql:db(X, dog),
                                     reql:table(X, ?TYPE_TABLE),
                                     reql:filter(X,#{name => Name}),
                                     reql:order_by(X, reql:desc(<<"created">>) ),
                                     reql:nth(X,0)
                             end),
    case R of
        {error,{runtime_error,<<"Index out of bounds: 0">>}} ->
            {error, notfound};
        {error, Error} ->
            {error, Error};
        _ ->
        R
    end.

-spec get_by_id(Id :: binary()) -> {ok, map()} | {error, notfound}.
get_by_id(Id) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    R = dog_rethink:run(
                                   fun(X) ->
                                           reql:db(X, dog),
                                           reql:table(X, ?TYPE_TABLE),
                                           reql:get(X, Id)
                                 end),
    case R of
        {ok, null} ->
            lager:error("profile id null return value: ~p",[Id]),
            {error, notfound};
        {ok, Result} ->
            {ok, Result}
    end.


-spec update(Id :: binary(), UpdateMap :: map()) -> {atom(), Id :: iolist()} | {false, atom()}.
update(Id, UpdateMap) ->
    update(Id,UpdateMap,false).

-spec update(Id :: binary(), UpdateMap :: map(), InPlace :: boolean() ) -> {atom(), Id :: iolist()} | {false, atom()}.
update(Id, UpdateMap, InPlace) ->
    lager:debug("Id: ~p~n",[Id]),
    lager:debug("UpdateMap: ~p~n",[UpdateMap]),
    case get_by_id(Id) of
        {ok, Profile@0} ->
            Profile@1 = maps:merge(Profile@0,UpdateMap),
            Profile@2 = maps:remove(<<"id">>,Profile@1),
            case InPlace of
                false -> 
                    case create(Profile@2) of
                        {validation_error, Error} ->
                            {validation_error, Error};
                        {ok, Id2} ->
                            case maps:find(<<"name">>,UpdateMap) of
                                error ->
                                    case lists:any(fun(X) -> element(1,X) /= ok end, dog_group:replace_profile_by_profile_id(Id,Id2)) of
                                        true ->
                                            {true, Id2};
                                        false ->
                                            {false, error_replacing_profile}
                                    end;
                                {ok,UpdateName} ->
                                    lager:debug("UpdateName: ~p",[UpdateName]),
                                    case dog_group:replace_profile_by_profile_id(Id,Id2,UpdateName) of
                                        [] -> % This profile not associated with any group
                                            {true, Id2};
                                        [{true, _}] ->
                                            {true, Id2}
                                    end
                            end
                    end;
                true ->
                   update_in_place(Id,Profile@2) 
            end;
        {error, Error} ->
            {false, Error}
    end.

-spec update_in_place(Id :: binary(), UpdateMap :: map()) -> {false, atom()} | {validation_error, iolist()} | {true, binary()}.
update_in_place(Id, UpdateMap) ->
    lager:info("update_in_place"),
    case get_by_id(Id) of
        {ok,OldProfile} ->
            NewProfile = maps:merge(OldProfile,UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE,NewProfile) of
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

-spec is_active(Id :: binary()) -> boolean().
is_active(Id) ->
    {ok, Active} = dog_profile:all_active(),
    lists:member(Id,Active).

-spec delete(GroupId :: binary()) -> (ok | {error, Error :: map()}).
delete(Id) ->
    case where_used(Id) of
        {ok,[]} ->
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
        {ok,Groups} ->
            lager:info("profile ~p not deleted, associated with group: ~p~n",[Id,Groups]),
            {error,#{<<"errors">> => #{<<"associated with group">> => Groups}}}
    end.

-spec rule_to_text(Rule :: map(), Keys :: list()) -> iolist().
rule_to_text(Rule, Keys) ->
    Values = lists:map(fun(L) -> 
                               Value = maps:get(L, Rule),
                               lager:debug("Key: ~p Value: ~p~n", [L, Value]),
                               case L of
                                   <<"group">> ->
                                       case Value of
                                           <<" ">> ->
                                               Value;
                                           <<"any">> ->
                                               Value;
                                           <<"ANY">> ->
                                               Value;
                                           _ ->
                                               case dog_group:get_name_by_id(Value) of
                                                   {ok, Group} ->
                                                       Group;
                                                   {error, _} ->
                                                       case dog_zone:get_name_by_id(Value) of
                                                           {ok, Zone} -> 
                                                                Zone;
                                                           {error, _} ->
                                                               throw(group_not_found)
                                                       end 
                                               end
                                       end;
                                   <<"service">> ->
                                       case Value of
                                           <<" ">> ->
                                               Value;
                                           _ ->
                                               dog_service:get_name_by_id(Value)
                                       end;
                                   _ ->
                                       Value
                               end
                       end, Keys),

    ValuesStrings = [ dog_common:to_list(X) || X <- Values],
    io_lib:format("~s",[string:join(ValuesStrings,"\t")]).

-spec to_text(Profile :: map()) -> {'ok', iolist() }.
to_text(Profile) ->
    Keys = [
            <<"order">>,
            <<"group">>,
            <<"service">>,
            <<"states">>,
            <<"action">>,
            <<"active">>
           ],
    Header = lists:map(fun(L) -> dog_common:to_list(L) end, Keys),
    Rules = maps:get(<<"rules">>, Profile),
    Inbound = maps:get(<<"inbound">>, Rules),
    Outbound = maps:get(<<"outbound">>, Rules),
    InboundList = lists:map(fun(Rule) -> rule_to_text(Rule,Keys) end, Inbound),
    OutboundList = lists:map(fun(Rule) -> rule_to_text(Rule,Keys) end, Outbound),
    Text = io_lib:format("Inbound:~n~s~n~s~n~nOutbound:~n~s~n~s~n",[
                                                                    string:join(Header,"\t"),
                                                                    string:join(InboundList,"\n"),
                                                                    string:join(Header,"\t"),
                                                                    string:join(OutboundList,"\n")
                                                                   ]),
    {ok, Text}.

only_zone_group(Rule) ->
    case maps:get(<<"group_type">>,Rule) of
        <<"ZONE">> -> {true,Rule};
        _ -> false
    end.

only_role_group(Rule) ->
    case maps:get(<<"group_type">>,Rule) of
        <<"ROLE">> -> {true,Rule};
        _ -> false
    end.

only_active_rule(Rule) ->
    case maps:get(<<"active">>,Rule) of
        true -> {true,Rule};
        _ -> false
    end.

-spec get_zone_groups_in_profile(Profile :: map()) -> iolist().
get_zone_groups_in_profile(Profile) ->
    Rules = maps:get(<<"rules">>,Profile),
    Inbound = maps:get(<<"inbound">>,Rules),
    Outbound = maps:get(<<"outbound">>,Rules),
    InboundActive = lists:filtermap(fun(Rule) -> only_active_rule(Rule) end, Inbound),
    OutboundActive = lists:filtermap(fun(Rule) -> only_active_rule(Rule) end, Outbound),
    InboundWithGroups = lists:filtermap(fun(Rule) ->
                                                only_zone_group(Rule) end, InboundActive),
    OutboundWithGroups = lists:filtermap(fun(Rule) ->
                                                 only_zone_group(Rule) end, OutboundActive),
    InboundGroups = lists:map(fun(Rule) ->
                                      maps:get(<<"group">>,Rule) end, InboundWithGroups),
    OutboundGroups = lists:map(fun(Rule) ->
                                       maps:get(<<"group">>,Rule) end, OutboundWithGroups),
    GroupsSet = sets:from_list(InboundGroups ++ OutboundGroups),
    Groups = sets:to_list(GroupsSet),
    Groups.

-spec get_role_groups_in_profile(Profile :: map()) -> iolist().
get_role_groups_in_profile(Profile) ->
    Rules = maps:get(<<"rules">>,Profile),
    Inbound = maps:get(<<"inbound">>,Rules),
    Outbound = maps:get(<<"outbound">>,Rules),
    InboundActive = lists:filtermap(fun(Rule) -> only_active_rule(Rule) end, Inbound),
    OutboundActive = lists:filtermap(fun(Rule) -> only_active_rule(Rule) end, Outbound),
    InboundWithGroups = lists:filtermap(fun(Rule) ->
                                                only_role_group(Rule) end, InboundActive),
    OutboundWithGroups = lists:filtermap(fun(Rule) ->
                                                 only_role_group(Rule) end, OutboundActive),
    InboundGroups = lists:map(fun(Rule) ->
                                      maps:get(<<"group">>,Rule) end, InboundWithGroups),
    OutboundGroups = lists:map(fun(Rule) ->
                                       maps:get(<<"group">>,Rule) end, OutboundWithGroups),
    GroupsSet = sets:from_list(InboundGroups ++ OutboundGroups),
    Groups = sets:to_list(GroupsSet),
    Groups.

-spec where_used(ProfileId :: binary()) -> {ok, list()}.
where_used(ProfileId) ->
    {ok, R} = dog_rethink:run(
                              fun(X) ->
                                      reql:db(X, dog),
                                      reql:table(X, group),
                                      reql:has_fields(X,[<<"profile_id">>]),
                                      reql:filter(X,fun(Y) -> reql:bracket(Y, <<"profile_id">>), reql:eq(Y, ProfileId) end),
                                      reql:get_field(X,<<"id">>)
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Groups = case lists:flatten(Result) of
                 [] -> [];
                 Else -> Else
             end,
    {ok, Groups}.

%r.db("dog").table("profile").filter({name: "test2"}).getField("rules").getField("inbound").nth(0).bracket("group")

%get all profiles ids currently in use:
%r.db("dog").table("group").map(
%  function(group) {
%       return r.branch(group('profile_version').eq("latest"),
%                   r.db("dog").table("profile").filter({name:group("name")}).orderBy(r.desc("created")).nth(0).bracket("id"),
%                         group("profile_version"))  ;
%                           }
%                           )

-spec get_schema() -> binary().
get_schema() ->
  dog_json_schema:get_file(?VALIDATION_TYPE).

-spec in_active_profile(Id :: binary()) -> {false, []} | {true, Profiles :: map() }.
in_active_profile(Id) ->
    {ok, Used} = where_used(Id),
    {ok, Active} = dog_profile:all_active(),
    Profiles = sets:to_list(sets:intersection(sets:from_list(Used), sets:from_list(Active))),
    case Profiles of
        [] -> 
            {false,[]};
        _ -> 
            {true, Profiles}
    end.

%TODO: Phase 1 of ec2 sg management: only control ports, allow any source address
-spec get_all_inbound_ports_by_protocol(ProfileJson :: map()) -> ProtocolPorts :: list().
get_all_inbound_ports_by_protocol(ProfileJson) ->
    Inbound = nested:get([<<"rules">>,<<"inbound">>], ProfileJson),
    ActiveInbound = [Rule || Rule <- Inbound, maps:get(<<"active">>,Rule) == true],
    RawPortsProtocols = lists:map(fun(Rule) ->
                              ServiceId = maps:get(<<"service">>,Rule),
                              {ok,Service} = dog_service:get_by_id(ServiceId),
                              Services = maps:get(<<"services">>,Service),
                              lists:nth(1,lists:map(fun(S) ->
                                                Ports = maps:get(<<"ports">>,S),
                                                Protocol = maps:get(<<"protocol">>,S),
                                                case Protocol of
                                                    <<"any">> ->
                                                            [
                                                             {<<"tcp">>, Ports},
                                                             {<<"udp">>, Ports}
                                                            ];
                                                        _ ->
                                                            {Protocol,Ports}
                                                end
                                        end,Services))
                      end,ActiveInbound),
    merge_lists_in_tuples(lists:flatten(RawPortsProtocols)).

%TODO: Add security group source to rules
%-spec get_all_inbound_rule_specs(ProfileJson :: list()) -> Rules :: list().
%get_all_inbound_rule_specs(ProfileJson) ->
%    Inbound = nested:get([<<"rules">>,<<"inbound">>], ProfileJson),
%    RawPortsProtocols = lists:map(fun(Rule) ->
%                              ServiceId = maps:get(<<"service">>,Rule),
%                              {ok,Service} = dog_service:get_by_id(ServiceId),
%                              Services = maps:get(<<"services">>,Service),
%                              lists:nth(1,lists:map(fun(S) ->
%                                                Ports = maps:get(<<"ports">>,S),
%                                                Protocol = maps:get(<<"protocol">>,S),
%                                                Users
%                                                IpRanges
%                                                Groups
%                                                case Protocol of
%                                                    <<"any">> ->
%                                                            [
%                                                             {<<"tcp">>, Ports},
%                                                             {<<"udp">>, Ports}
%                                                            ];
%                                                        _ ->
%                                                            {Protocol,Ports}
%                                                end
%                                        end,Services))
%                      end,Inbound),
%    merge_lists_in_tuples(lists:flatten(RawPortsProtocols)).
    
%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

hex(N) when N < 10 ->
    N + $0;
hex(N) when N < 16 ->
    N - 10 + $a.

%For pre-2X Erlang:
trim(String, trailing, " ") ->
    re:replace(re:replace(String, "\\s+$", "",
              [global, {return, list}]),
           "^\\s+", "", [global, {return, list}]).

split(String, Delimiter, all) ->
    split(String, Delimiter).
split(String, Delimiter) ->
    re:split(String, Delimiter, [{return, list}]).
replace(String,SearchPattern,Replacement,all) ->
    Replaced = re:replace(String,SearchPattern,Replacement,[global,{return,list}]),
    [Replaced].

merge_lists_in_tuples(List) ->
    Map = lists:foldl(fun fun_merge_lists_in_tuples/2, maps:new(), List),
    lists:map(fun({K,V}) ->
                      UniqueV = lists:usort(lists:flatten(V)),
                      {K,UniqueV}
              end,maps:to_list(Map)).

fun_merge_lists_in_tuples(H, A) ->
    K = element(1, H),
    case maps:is_key(K, A) of
        true ->
            V = maps:get(K, A),
            maps:put(K, [element(2,H) | V], A);
        false ->
            maps:put(K, element(2,H), A)
    end.
