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
    update/2
]).

-export([
    add_rules/1,
    all/0,
    all_active/0,
    create_hash/1,
    create_iptables_ruleset/10,
    create_iptables_ruleset/11,
    create_iptables_ruleset/12,
    create_iptables_ruleset/13,
    date_string/0,
    generate_ipv4_iptables_ruleset_by_group_id/1,
    generate_ipv4_iptables_ruleset_by_id/2,
    generate_ipv6_iptables_ruleset_by_group_id/1,
    generate_ipv6_iptables_ruleset_by_id/2,
    get_all_inbound_ports_by_protocol/1,
    get_ppps_inbound_ec2/2,
    get_ppps_outbound_ec2/2,
    get_role_groups_in_profile/1,
    get_zone_groups_in_profile/1,
    init/0,
    to_hcl/1,
    to_hcl_by_id/1,
    to_text/1,
    where_used/1
]).

-spec init() -> any().
init() ->
    pass.

-spec generate_ipv6_iptables_ruleset_by_id(Id :: binary(), SelfGroupName :: binary()) -> {{ok, iolist()}, {ok, iolist()}}.
generate_ipv6_iptables_ruleset_by_id(Id, SelfGroupName) ->
    case get_by_id(Id) of
        {error, _Error} ->
            ?LOGT_INFO("No profile associated with group id: ~p", [{id,Id}]),
            throw(profile_not_found);
        {ok, ProfileJson} ->
            IpsetsIptablesRulesetResult = dog_iptables_ruleset:generate_iptables_ruleset(
                ProfileJson, ipsets, <<"v6">>, SelfGroupName
            ),
            IptablesIptablesRulesetResult = dog_iptables_ruleset:generate_iptables_ruleset(
                ProfileJson, iptables, <<"v6">>, SelfGroupName
            ),
            {IpsetsIptablesRulesetResult, IptablesIptablesRulesetResult}
    end.

-spec generate_ipv4_iptables_ruleset_by_id(GroupId :: binary(), SelfGroupName :: binary() ) -> {{ok, iolist()}, {ok, iolist()}}.
generate_ipv4_iptables_ruleset_by_id(Id, SelfGroupName) ->
    {ok, Json} = get_by_id(Id),
    Ipsets = dog_iptables_ruleset:generate_iptables_ruleset(Json, ipsets, <<"v4">>, SelfGroupName),
    Iptables = dog_iptables_ruleset:generate_iptables_ruleset(Json, iptables, <<"v4">>, SelfGroupName),
    {Ipsets, Iptables}.

-spec generate_ipv4_iptables_ruleset_by_group_name(
    GroupName :: binary(),
    Ipv4RoleMap :: map(),
    Ipv6RoleMap :: map(),
    Ipv4ZoneMap :: map(),
    Ipv6ZoneMap :: map(),
    ZoneIdMap :: map(),
    GroupIdMap :: map(),
    ServiceIdMap :: map()
) -> {{ok, list()}, {ok, iolist()}}.
generate_ipv4_iptables_ruleset_by_group_name(
    GroupName,
    Ipv4RoleMap,
    Ipv6RoleMap,
    Ipv4ZoneMap,
    Ipv6ZoneMap,
    ZoneIdMap,
    GroupIdMap,
    ServiceIdMap
) ->
    case dog_group:get_profile_by_name(GroupName) of
        {error, _Error} ->
            ?LOGT_INFO("No profile associated with group: ~p", [{group_name,GroupName}]),
            throw(profile_not_found);
        {ok, ProfileJson} ->
            write_profile_to_file(ProfileJson, GroupName),
            IpsetsIptablesRulesetResult = dog_iptables_ruleset:generate_iptables_ruleset(
                ProfileJson,
                ipsets,
                <<"v4">>,
                Ipv4RoleMap,
                Ipv6RoleMap,
                Ipv4ZoneMap,
                Ipv6ZoneMap,
                ZoneIdMap,
                GroupIdMap,
                ServiceIdMap,
                GroupName
            ),
            IptablesIptablesRulesetResult =
                case application:get_env(dog_trainer, generate_unset_tables, true) of
                    true ->
                        dog_iptables_ruleset:generate_iptables_ruleset(
                            ProfileJson,
                            iptables,
                            <<"v4">>,
                            Ipv4RoleMap,
                            Ipv6RoleMap,
                            Ipv4ZoneMap,
                            Ipv6ZoneMap,
                            ZoneIdMap,
                            GroupIdMap,
                            ServiceIdMap,
                            GroupName
                        );
                    false ->
                        {ok, []}
                end,
            {IpsetsIptablesRulesetResult, IptablesIptablesRulesetResult}
    end.

-spec generate_ipv6_iptables_ruleset_by_group_id(GroupId :: binary()) -> {iolist(), iolist()}.
generate_ipv6_iptables_ruleset_by_group_id(GroupId) ->
    SelfGroupName = maps:get(<<"Name">>, dog_group:get_by_id(GroupId)),
    ProfileId =
        case dog_group:get_profile_by_id(GroupId) of
            {'error', 'notfound'} -> profile_not_found(GroupId);
            {ok, Id} -> Id
        end,
    {ok, Json} = get_by_id(ProfileId),
    {ok, Ipsets} = dog_iptables_ruleset:generate_iptables_ruleset(Json, ipsets, <<"v4">>, SelfGroupName),
    {ok, Iptables} = dog_iptables_ruleset:generate_iptables_ruleset(Json, iptables, <<"v4">>, SelfGroupName),
    {Ipsets, Iptables}.

-spec profile_not_found(GroupId :: binary()) -> no_return().
profile_not_found(GroupId) ->
    ?LOGT_INFO("No profile associated with group id: ~p", [{group_id,GroupId}]),
    throw(profile_not_found).

-spec generate_ipv6_iptables_ruleset_by_group_name(
    GroupName :: binary(),
    Ipv4RoleMap :: map(),
    Ipv6RoleMap :: map(),
    Ipv4ZoneMap :: map(),
    Ipv6ZoneMap :: map(),
    ZoneIdMap :: map(),
    GroupIdMap :: map(),
    ServiceIdMap :: map()
) -> {{ok, iolist()}, {ok, iolist()}}.
generate_ipv6_iptables_ruleset_by_group_name(
    GroupName,
    Ipv4RoleMap,
    Ipv6RoleMap,
    Ipv4ZoneMap,
    Ipv6ZoneMap,
    ZoneIdMap,
    GroupIdMap,
    ServiceIdMap
) ->
    Response = dog_group:get_profile_by_name(GroupName),
    case Response of
        {error, _Reason} ->
            ?LOGT_INFO("No profile associated with group name: ~p", [{group_name,GroupName}]),
            throw(profile_not_found);
        {ok, ProfileJson} ->
            write_profile_to_file(ProfileJson, GroupName),
            IpsetsIptablesRulesetResult = dog_iptables_ruleset:generate_iptables_ruleset(
                ProfileJson,
                ipsets,
                <<"v6">>,
                Ipv4RoleMap,
                Ipv6RoleMap,
                Ipv4ZoneMap,
                Ipv6ZoneMap,
                ZoneIdMap,
                GroupIdMap,
                ServiceIdMap,
                GroupName
            ),
            IptablesIptablesRulesetResult =
                case application:get_env(dog_trainer, generate_unset_tables, true) of
                    true ->
                        dog_iptables_ruleset:generate_iptables_ruleset(
                            ProfileJson,
                            iptables,
                            <<"v6">>,
                            Ipv4RoleMap,
                            Ipv6RoleMap,
                            Ipv4ZoneMap,
                            Ipv6ZoneMap,
                            ZoneIdMap,
                            GroupIdMap,
                            ServiceIdMap,
                            GroupName
                        );
                    false ->
                        {ok, []}
                end,
            {IpsetsIptablesRulesetResult, IptablesIptablesRulesetResult}
    end.

-spec generate_ipv4_iptables_ruleset_by_group_id(GroupId :: binary()) -> {iolist(), iolist()}.
generate_ipv4_iptables_ruleset_by_group_id(GroupId) ->
    SelfGroupName = maps:get(<<"Name">>, dog_group:get_by_id(GroupId)),
    ProfileId =
        case dog_group:get_profile_by_id(GroupId) of
            {'error', 'notfound'} -> profile_not_found(GroupId);
            {ok, Id} -> Id
        end,
    {ok, Json} = get_by_id(ProfileId),
    {ok, Ipsets} = dog_iptables_ruleset:generate_iptables_ruleset(Json, ipsets, <<"v4">>, SelfGroupName),
    {ok, Iptables} = dog_iptables_ruleset:generate_iptables_ruleset(Json, iptables, <<"v4">>, SelfGroupName),
    {Ipsets, Iptables}.

-spec write_profile_to_file(Profile :: map(), GroupName :: binary()) -> ok.
write_profile_to_file(Profile, GroupName) ->
    FileName = ?RUNDIR ++ "/profile." ++ binary_to_list(GroupName) ++ ".txt",
    ok = file:write_file(FileName, jsx:encode(Profile)),
    ok.

remove_comments(IptablesRuleset) ->
    NoCommentRulesList = lists:filter(
        fun(X) ->
            case re:run(X, "^#") of
                nomatch -> true;
                _ -> false
            end
        end,
        split(IptablesRuleset, "\n", all)
    ),
    NoCommentRules = lists:flatten(
        lists:join(
            "\n",
            NoCommentRulesList
        )
    ),
    NoCommentRules.

remove_docker(IptablesRuleset) ->
    lists:map(
        fun(Line0) ->
            Line1 = re:replace(Line0, "^-A DOCKER(.*)", "", [{return, list}]),
            Line2 = re:replace(Line1, "^:DOCKER(.*)", "", [{return, list}]),
            Line3 =
                case Line2 of
                    "-A FORWARD -j REJECT --reject-with icmp-port-unreachable" ->
                        Line2;
                    _ ->
                        re:replace(Line2, "^-A FORWARD(.*)", "", [{return, list}])
                end,
            Line3
        end,
        IptablesRuleset
    ).

remove_empty_lists(List) ->
    [L || L <- List, L =/= []].

remove_quotes(Line0) ->
    Line1 = re:replace(Line0, "\"", "", [{return, list}, global]),
    Line2 = re:replace(Line1, "\'", "", [{return, list}, global]),
    Line2.

-spec zero_counters(IptablesRuleset :: iolist()) -> iolist().
zero_counters(IptablesRuleset) ->
    re:replace(
        IptablesRuleset,
        "(:.*) \\[.*\\]",
        "\\1 [0:0]",
        [{return, list}, global]
    ).

normalize_iptables_ruleset(IptablesRuleset) ->
    IptablesRulesetNoComments = remove_comments(IptablesRuleset),
    IptablesRulesetZeroed = zero_counters(IptablesRulesetNoComments),
    IptablesRulesetSplit = string:split(IptablesRulesetZeroed, "\n", all),
    IptablesRulesetNoQuotes = [remove_quotes(Line) || Line <- IptablesRulesetSplit],
    IptablesRulesetTrimmed = [string:trim(Line, trailing, " ") || Line <- IptablesRulesetNoQuotes],
    IptablesRulesetNoDocker = remove_docker(IptablesRulesetTrimmed),
    IptablesRulesetNoBlankLines = remove_empty_lists(IptablesRulesetNoDocker),
    IptablesRulesetNormalized = lists:flatten(lists:join("\n", IptablesRulesetNoBlankLines)),
    IptablesRulesetNormalized.

-spec create_hash(IptablesRuleset :: iodata()) -> binary().
create_hash(IptablesRuleset) ->
    IptablesRulesetTrimmed = normalize_iptables_ruleset(IptablesRuleset),
    ?LOGT_INFO("IptablesRulesetTrimmed: ~p", [{iptables_ruleset_trimmed,IptablesRulesetTrimmed}]),
    BitString = base16:encode(crypto:hash(sha256, IptablesRulesetTrimmed)),
    Binary = binary:list_to_bin(erlang:bitstring_to_list(BitString)),
    Binary.

-spec create_iptables_ruleset(
    RoutingKey :: binary(),
    Group :: binary(),
    Ipv4RoleMap :: map(),
    Ipv6RoleMap :: map(),
    Ipv4ZoneMap :: map(),
    Ipv6ZoneMap :: map(),
    ZoneIdMap :: map(),
    GroupIdMap :: map(),
    ServiceIdMap :: map(),
    Ipsets :: iolist()
) ->
    error
    | {
        R4IpsetsIptablesRuleset :: iolist(),
        R6IpsetsIptablesRuleset :: iolist(),
        R4IptablesIptablesRuleset :: iolist(),
        R6IptablesIptablesRuleset :: iolist()
    }.
create_iptables_ruleset(
    RoutingKey,
    Group,
    Ipv4RoleMap,
    Ipv6RoleMap,
    Ipv4ZoneMap,
    Ipv6ZoneMap,
    ZoneIdMap,
    GroupIdMap,
    ServiceIdMap,
    Ipsets
) ->
    create_iptables_ruleset(
        RoutingKey,
        Group,
        <<"*">>,
        <<"*">>,
        <<"*">>,
        Ipv4RoleMap,
        Ipv6RoleMap,
        Ipv4ZoneMap,
        Ipv6ZoneMap,
        ZoneIdMap,
        GroupIdMap,
        ServiceIdMap,
        Ipsets
    ).

-spec create_iptables_ruleset(
    RoutingKey :: binary(),
    Group :: binary(),
    Environment :: binary(),
    Ipv4RoleMap :: map(),
    Ipv6RoleMap :: map(),
    Ipv4ZoneMap :: map(),
    Ipv6ZoneMap :: map(),
    ZoneIdMap :: map(),
    GroupIdMap :: map(),
    ServiceIdMap :: map(),
    Ipsets :: iolist()
) ->
    error
    | {
        R4IpsetsIptablesRuleset :: iolist(),
        R6IpsetsIptablesRuleset :: iolist(),
        R4IptablesIptablesRuleset :: iolist(),
        R6IptablesIptablesRuleset :: iolist()
    }.
create_iptables_ruleset(
    RoutingKey,
    Group,
    Environment,
    Ipv4RoleMap,
    Ipv6RoleMap,
    Ipv4ZoneMap,
    Ipv6ZoneMap,
    ZoneIdMap,
    GroupIdMap,
    ServiceIdMap,
    Ipsets
) ->
    create_iptables_ruleset(
        RoutingKey,
        Group,
        Environment,
        <<"*">>,
        <<"*">>,
        Ipv4RoleMap,
        Ipv6RoleMap,
        Ipv4ZoneMap,
        Ipv6ZoneMap,
        ZoneIdMap,
        GroupIdMap,
        ServiceIdMap,
        Ipsets
    ).

-spec create_iptables_ruleset(
    RoutingKey :: binary(),
    Group :: binary(),
    Environment :: binary(),
    Location :: binary(),
    Ipv4RoleMap :: map(),
    Ipv6RoleMap :: map(),
    Ipv4ZoneMap :: map(),
    Ipv6ZoneMap :: map(),
    ZoneIdMap :: map(),
    GroupIdMap :: map(),
    ServiceIdMap :: map(),
    Ipsets :: iolist()
) ->
    error
    | {
        R4IpsetsIptablesRuleset :: iolist(),
        R6IpsetsIptablesRuleset :: iolist(),
        R4IptablesIptablesRuleset :: iolist(),
        R6IptablesIptablesRuleset :: iolist()
    }.
create_iptables_ruleset(
    RoutingKey,
    Group,
    Environment,
    Location,
    Ipv4RoleMap,
    Ipv6RoleMap,
    Ipv4ZoneMap,
    Ipv6ZoneMap,
    ZoneIdMap,
    GroupIdMap,
    ServiceIdMap,
    Ipsets
) ->
    create_iptables_ruleset(
        RoutingKey,
        Group,
        Environment,
        Location,
        <<"*">>,
        Ipv4RoleMap,
        Ipv6RoleMap,
        Ipv4ZoneMap,
        Ipv6ZoneMap,
        ZoneIdMap,
        GroupIdMap,
        ServiceIdMap,
        Ipsets
    ).

-spec create_iptables_ruleset(
    RoutingKey :: binary(),
    Group :: binary(),
    Environment :: binary(),
    Location :: binary(),
    HostKey :: binary(),
    Ipv4RoleMap :: map(),
    Ipv6RoleMap :: map(),
    Ipv4ZoneMap :: map(),
    Ipv6ZoneMap :: map(),
    ZoneIdMap :: map(),
    GroupIdMap :: map(),
    ServiceIdMap :: map(),
    Ipsets :: iolist()
) ->
    error
    | {
        R4IpsetsIptablesRuleset :: iolist(),
        R6IpsetsIptablesRuleset :: iolist(),
        R4IptablesIptablesRuleset :: iolist(),
        R6IptablesIptablesRuleset :: iolist()
    }.
create_iptables_ruleset(
    RoutingKey,
    Group,
    _Environment,
    _Location,
    _HostKey,
    Ipv4RoleMap,
    Ipv6RoleMap,
    Ipv4ZoneMap,
    Ipv6ZoneMap,
    ZoneIdMap,
    GroupIdMap,
    ServiceIdMap,
    _Ipsets
) ->
    ?LOGT_INFO("creating Ipv4,Ipv6 iptables_rulesets, ipsets: ~p", [{routing_key,RoutingKey}]),
    {R4IpsetsResult, R4IptablesResult} = generate_ipv4_iptables_ruleset_by_group_name(
        Group,
        Ipv4RoleMap,
        Ipv6RoleMap,
        Ipv4ZoneMap,
        Ipv6ZoneMap,
        ZoneIdMap,
        GroupIdMap,
        ServiceIdMap
    ),
    {R6IpsetsResult, R6IptablesResult} = generate_ipv6_iptables_ruleset_by_group_name(
        Group,
        Ipv4RoleMap,
        Ipv6RoleMap,
        Ipv4ZoneMap,
        Ipv6ZoneMap,
        ZoneIdMap,
        GroupIdMap,
        ServiceIdMap
    ),
    AnyError = lists:any(fun(X) -> X == error end, [
        R4IpsetsResult, R6IpsetsResult, R4IptablesResult, R6IptablesResult
    ]),
    AnyNull = lists:any(fun(X) -> X == null end, [
        R4IpsetsResult, R6IpsetsResult, R4IptablesResult, R6IptablesResult
    ]),
    case AnyError of
        true ->
            ?LOGT_INFO("Error generating at least one Ipv4,Ipv6 iptables_ruleset or ipsets, not publishing: ~p", [
                {routing_key,RoutingKey}
            ]),
            error;
        false ->
            case AnyNull of
                true ->
                    ?LOGT_INFO("Found null Ipset or IpIptablesRuleset, not publishing: ~p", [
                        {routing_key,RoutingKey}
                    ]),
                    {false, false, false, false};
                false ->
                    {ok, R4IpsetsIptablesRuleset} = R4IpsetsResult,
                    {ok, R6IpsetsIptablesRuleset} = R6IpsetsResult,
                    {ok, R4IptablesIptablesRuleset} = R4IptablesResult,
                    {ok, R6IptablesIptablesRuleset} = R6IptablesResult,
                    Hash4Ipsets = create_hash(R4IpsetsIptablesRuleset),
                    Hash6Ipsets = create_hash(R6IpsetsIptablesRuleset),
                    Hash4Iptables = create_hash(R4IptablesIptablesRuleset),
                    Hash6Iptables = create_hash(R6IptablesIptablesRuleset),
                    {ok, _} = dog_group:set_hash4_ipsets(Group, Hash4Ipsets),
                    {ok, _} = dog_group:set_hash6_ipsets(Group, Hash6Ipsets),
                    {ok, _} = dog_group:set_hash4_iptables(Group, Hash4Iptables),
                    {ok, _} = dog_group:set_hash6_iptables(Group, Hash6Iptables),
                    dog_iptables_ruleset:write_iptables_ruleset_set_v4_to_file(
                        R4IpsetsIptablesRuleset, Group
                    ),
                    dog_iptables_ruleset:write_iptables_ruleset_set_v6_to_file(
                        R6IpsetsIptablesRuleset, Group
                    ),
                    dog_iptables_ruleset:write_iptables_ruleset_unset_v4_to_file(
                        R4IptablesIptablesRuleset, Group
                    ),
                    dog_iptables_ruleset:write_iptables_ruleset_unset_v6_to_file(
                        R6IptablesIptablesRuleset, Group
                    ),
                    {R4IpsetsIptablesRuleset, R6IpsetsIptablesRuleset, R4IptablesIptablesRuleset,
                        R6IptablesIptablesRuleset}
            end
    end.

-spec date_string() -> iolist().
date_string() ->
    {Date, Time} = calendar:universal_time(),
    {Year, Month, Day} = Date,
    {Hour, Minute, Second} = Time,
    DayNumber = calendar:day_of_the_week(Date),
    DayName =
        case DayNumber of
            1 -> "Mon";
            2 -> "Tue";
            3 -> "Wed";
            4 -> "Thu";
            5 -> "Fri";
            6 -> "Sat";
            7 -> "Sun"
        end,
    MonthName =
        case Month of
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
    DateString = io_lib:format("~s ~s ~B ~B:~B:~B ~B UTC", [
        DayName, MonthName, Day, Hour, Minute, Second, Year
    ]),
    DateString.

-spec create(Group :: map()) -> {ok | error, Key :: iolist() | name_exists}.
create(ProfileMap@0) ->
    ?LOGT_DEBUG("ProfileMap@0: ~p",[{profilemap@0,ProfileMap@0}]),
    Name = maps:get(<<"name">>, ProfileMap@0),
    {RulesMap@0, ProfileMap@1} = maps:take(<<"rules">>, ProfileMap@0),
    {ok, ExistingProfiles} = get_all(),
    ExistingNames = [maps:get(<<"name">>, Profile) || Profile <- ExistingProfiles],
    case lists:member(Name, ExistingNames) of
        false ->
            case dog_json_schema:validate(?VALIDATION_TYPE, ProfileMap@1) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:insert(X, ProfileMap@1)
                        end
                    ),
                    ProfileId = hd(maps:get(<<"generated_keys">>, R)),
                    ?LOGT_DEBUG("create R: ~p~n", [{r,R}]),
                    {ok, _RulesetId} = dog_ruleset:create(
                        #{
                            <<"name">> => Name,
                            <<"rules">> => RulesMap@0,
                            <<"profile_id">> => ProfileId
                        }
                    ),
                    {ok, ProfileId};
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        true ->
            {error, name_exists}
    end.

-spec get_all() -> {'ok', list()}.
get_all() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:pluck(X, [<<"name">>, <<"id">>, <<"created">>])
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Profiles =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Profiles}.

-spec get_by_name(Name :: binary()) -> {ok, map()} | {error, atom()}.
get_by_name(Name) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get_all(X, Name, #{index => <<"name">>})
        end
    ),
    {ok, R3} = rethink_cursor:all(R),
    Result = lists:flatten(R3),
    case Result of
        [] ->
            ?LOGT_ERROR("error, profile name not found: ~p", [{name,Name}]),
            {error, notfound};
        _ ->
            Profile = hd(Result),
            add_rules(Profile)
    end.

-spec all_active() -> {ok, Profiles :: list()}.
all_active() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, group),
            reql:get_field(X, <<"profile_id">>)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Profiles =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Profiles}.

-spec all() -> {ok, Profiles :: list()}.
all() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get_field(X, <<"id">>)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Profiles =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Profiles}.

-spec get_by_id(Id :: binary()) -> {ok, map()} | {error, notfound}.
get_by_id(Id) ->
    R = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, Id)
        end
    ),
    case R of
        {ok, null} ->
            ?LOGT_DEBUG("profile id null return value: ~p", [{id,Id}]),
            {error, notfound};
        {ok, Profile} ->
            add_rules(Profile)
    end.

add_rules(Profile) ->
    ProfileId = maps:get(<<"id">>, Profile),
    case dog_ruleset:get_by_profile_id(ProfileId) of
        {error, notfound} ->
            {ok,
                maps:put(
                    <<"rules">>,
                    #{
                        <<"inbound">> => [],
                        <<"outbound">> => []
                    },
                    Profile
                )};
        {ok, Ruleset} ->
            {ok, maps:put(<<"rules">>, maps:get(<<"rules">>, Ruleset), Profile)}
    end.

-spec update(Id :: binary(), UpdateMap :: map()) ->
    {false, atom()} | {validation_error, iolist()} | {true, binary()}.
update(Id, UpdateMap) ->
    ?LOGT_INFO("update_in_place",[]),
    case get_by_id(Id) of
        {ok, OldProfile} ->
            ProfileId = maps:get(<<"id">>, OldProfile),
            case dog_ruleset:get_id_by_profile_id(ProfileId) of
                {error, notfound} ->
                    ProfileName = maps:get(<<"name">>, OldProfile),
                    {Rules, UpdateMap@0} = maps:take(<<"rules">>, UpdateMap),
                    RulesMap@0 = #{
                        <<"name">> => ProfileName,
                        <<"rules">> => Rules,
                        <<"profile_id">> => ProfileId
                    },
                    {_ok_or_error, _NewRulesetId} = dog_ruleset:create(RulesMap@0),
                    NewProfile = maps:merge(OldProfile, UpdateMap@0),
                    case dog_json_schema:validate(?VALIDATION_TYPE, NewProfile) of
                        ok ->
                            {ok, R} = dog_rethink:run(
                                fun(X) ->
                                    reql:db(X, dog),
                                    reql:table(X, ?TYPE_TABLE),
                                    reql:get(X, Id),
                                    reql:update(X, UpdateMap@0)
                                end
                            ),
                            ?LOGT_DEBUG("update R: ~p~n", [{r,R}]),
                            Replaced = maps:get(<<"replaced">>, R),
                            Unchanged = maps:get(<<"unchanged">>, R),
                            case {Replaced, Unchanged} of
                                {1, 0} -> {true, Id};
                                {0, 1} -> {false, Id};
                                _ -> {false, no_updated}
                            end;
                        {error, Error} ->
                            Response = dog_parse:validation_error(Error),
                            {validation_error, Response}
                    end;
                RulesetId ->
                    ProfileName = maps:get(<<"name">>, OldProfile),
                    {Rules, UpdateMap@0} = maps:take(<<"rules">>, UpdateMap),
                    RulesMap@0 = #{
                        <<"name">> => ProfileName,
                        <<"rules">> => Rules,
                        <<"profile_id">> => ProfileId
                    },
                    NewProfile = maps:merge(OldProfile, UpdateMap@0),
                    {_, _NewRulesetId} = dog_ruleset:update(RulesetId, RulesMap@0),
                    case dog_json_schema:validate(?VALIDATION_TYPE, NewProfile) of
                        ok ->
                            {ok, R} = dog_rethink:run(
                                fun(X) ->
                                    reql:db(X, dog),
                                    reql:table(X, ?TYPE_TABLE),
                                    reql:get(X, Id),
                                    reql:update(X, UpdateMap@0)
                                end
                            ),
                            ?LOGT_DEBUG("update R: ~p~n", [{r,R}]),
                            Replaced = maps:get(<<"replaced">>, R),
                            Unchanged = maps:get(<<"unchanged">>, R),
                            case {Replaced, Unchanged} of
                                {1, 0} -> {true, Id};
                                {0, 1} -> {false, Id};
                                _ -> {false, no_updated}
                            end;
                        {error, Error} ->
                            Response = dog_parse:validation_error(Error),
                            {validation_error, Response}
                    end
            end;
        {error, Error} ->
            {false, Error}
    end.

-spec delete(GroupId :: binary()) -> (ok | {error, Error :: map()}).
delete(Id) ->
    case where_used(Id) of
        {ok, []} ->
            {ok, Profile} = get_by_id(Id),
            ProfileName = maps:get(<<"name">>, Profile),
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, ?TYPE_TABLE),
                    reql:get(X, Id),
                    reql:delete(X)
                end
            ),
            ?LOGT_DEBUG("delete R: ~p~n", [{r,R}]),
            {ok, Ruleset} = dog_ruleset:get_by_name(ProfileName),
            RulesetId = maps:get(<<"id">>, Ruleset),
            ?LOGT_DEBUG("RulesetId: ~p",[{rulesetid,RulesetId}]),
            dog_ruleset:delete(RulesetId),
            ok;
        {ok, Groups} ->
            ?LOGT_INFO("profile ~p not deleted, associated with group: ~p~n", [{id,Id}, {groups,Groups}]),
            {error, #{<<"errors">> => #{<<"associated with group">> => Groups}}}
    end.

-spec rule_to_text(Rule :: map(), Keys :: list()) -> iolist().
rule_to_text(Rule, Keys) ->
    Values = lists:map(
        fun(L) ->
            Value = maps:get(L, Rule),
            ?LOGT_DEBUG("Key: ~p Value: ~p~n", [{l,L}, {value,Value}]),
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
        end,
        Keys
    ),

    ValuesStrings = [dog_common:to_list(X) || X <- Values],
    io_lib:format("~s", [string:join(ValuesStrings, "\t")]).

-spec to_text(Profile :: map()) -> {'ok', iolist()}.
to_text(Profile) ->
    Keys = [
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
    InboundList = lists:map(fun(Rule) -> rule_to_text(Rule, Keys) end, Inbound),
    OutboundList = lists:map(fun(Rule) -> rule_to_text(Rule, Keys) end, Outbound),
    Text = io_lib:format("Inbound:~n~s~n~s~n~nOutbound:~n~s~n~s~n", [
        string:join(Header, "\t"),
        string:join(InboundList, "\n"),
        string:join(Header, "\t"),
        string:join(OutboundList, "\n")
    ]),
    {ok, Text}.

only_zone_group(Rule) ->
    case maps:get(<<"group_type">>, Rule) of
        <<"ZONE">> -> {true, Rule};
        _ -> false
    end.

only_role_group(Rule) ->
    case maps:get(<<"group_type">>, Rule) of
        G when G =:= <<"ROLE">>; G =:= <<"GROUP">> -> {true, Rule};
        _ -> false
    end.

only_active_rule(Rule) ->
    case maps:get(<<"active">>, Rule) of
        true -> {true, Rule};
        _ -> false
    end.

-spec get_zone_groups_in_profile(Profile :: map()) -> iolist().
get_zone_groups_in_profile(Profile) ->
    Rules = maps:get(<<"rules">>, Profile),
    Inbound = maps:get(<<"inbound">>, Rules),
    Outbound = maps:get(<<"outbound">>, Rules),
    InboundActive = lists:filtermap(fun(Rule) -> only_active_rule(Rule) end, Inbound),
    OutboundActive = lists:filtermap(fun(Rule) -> only_active_rule(Rule) end, Outbound),
    InboundWithGroups = lists:filtermap(
        fun(Rule) ->
            only_zone_group(Rule)
        end,
        InboundActive
    ),
    OutboundWithGroups = lists:filtermap(
        fun(Rule) ->
            only_zone_group(Rule)
        end,
        OutboundActive
    ),
    InboundGroups = lists:map(
        fun(Rule) ->
            maps:get(<<"group">>, Rule)
        end,
        InboundWithGroups
    ),
    OutboundGroups = lists:map(
        fun(Rule) ->
            maps:get(<<"group">>, Rule)
        end,
        OutboundWithGroups
    ),
    GroupsSet = sets:from_list(InboundGroups ++ OutboundGroups),
    Groups = sets:to_list(GroupsSet),
    Groups.

-spec get_role_groups_in_profile(Profile :: map()) -> iolist().
get_role_groups_in_profile(Profile) ->
    Rules = maps:get(<<"rules">>, Profile),
    Inbound = maps:get(<<"inbound">>, Rules),
    Outbound = maps:get(<<"outbound">>, Rules),
    InboundActive = lists:filtermap(fun(Rule) -> only_active_rule(Rule) end, Inbound),
    OutboundActive = lists:filtermap(fun(Rule) -> only_active_rule(Rule) end, Outbound),
    InboundWithGroups = lists:filtermap(
        fun(Rule) ->
            only_role_group(Rule)
        end,
        InboundActive
    ),
    OutboundWithGroups = lists:filtermap(
        fun(Rule) ->
            only_role_group(Rule)
        end,
        OutboundActive
    ),
    InboundGroups = lists:map(
        fun(Rule) ->
            maps:get(<<"group">>, Rule)
        end,
        InboundWithGroups
    ),
    OutboundGroups = lists:map(
        fun(Rule) ->
            maps:get(<<"group">>, Rule)
        end,
        OutboundWithGroups
    ),
    GroupsSet = sets:from_list(InboundGroups ++ OutboundGroups),
    Groups = sets:to_list(GroupsSet),
    Groups.

-spec where_used(ProfileId :: binary()) -> {ok, list()}.
where_used(ProfileId) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, group),
            reql:has_fields(X, [<<"profile_id">>]),
            reql:filter(X, fun(Y) ->
                reql:bracket(Y, <<"profile_id">>),
                reql:eq(Y, ProfileId)
            end),
            reql:get_field(X, <<"id">>)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Groups =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Groups}.

-spec get_schema() -> binary().
get_schema() ->
    dog_json_schema:get_file(?VALIDATION_TYPE).

%TODO: Phase 1 of ec2 sg management: only control ports, allow any source address
-spec get_all_inbound_ports_by_protocol(ProfileJson :: map()) -> ProtocolPorts :: list().
get_all_inbound_ports_by_protocol(ProfileJson) ->
    Inbound = nested:get([<<"rules">>, <<"inbound">>], ProfileJson),
    ActiveInbound = [Rule || Rule <- Inbound, maps:get(<<"active">>, Rule) == true],
    RawPortsProtocols = lists:map(
        fun(Rule) ->
            ServiceId = maps:get(<<"service">>, Rule),
            {ok, Service} = dog_service:get_by_id(ServiceId),
            Services = maps:get(<<"services">>, Service),
            lists:nth(
                1,
                lists:map(
                    fun(S) ->
                        Ports = maps:get(<<"ports">>, S),
                        Protocol = maps:get(<<"protocol">>, S),
                        case Protocol of
                            <<"any">> ->
                                [
                                    {<<"tcp">>, Ports},
                                    {<<"udp">>, Ports}
                                ];
                            _ ->
                                {Protocol, Ports}
                        end
                    end,
                    Services
                )
            )
        end,
        ActiveInbound
    ),
    dog_common:merge_lists_in_tuples(lists:flatten(RawPortsProtocols)).
%NOTE: erlcloud encodes 'all services' as an atom '-1'
%encodes 'all ports as integers from_port = 0, to_port = 0
%encodes icmp as type in from_port as integer, and to_port as integer -1
-spec expand_services(Source :: binary(), Services :: binary()) ->
    {Protocol :: binary(), FromPort :: binary(), ToPort :: binary(), Source :: binary()}.
expand_services(Source, Services) ->
    lists:nth(
        1,
        lists:map(
            fun(S) ->
                PortsList = maps:get(<<"ports">>, S),
                lists:map(
                    fun(Ports) ->
                        Protocol = maps:get(<<"protocol">>, S),
                        {FromPort, ToPort} =
                            case split(Ports, ":") of
                                [F, T] ->
                                    {list_to_integer(F), list_to_integer(T)};
                                [F] ->
                                    case Protocol of
                                        <<"icmp">> ->
                                            {list_to_integer(F), -1};
                                        _ ->
                                            {list_to_integer(F), list_to_integer(F)}
                                    end
                            end,
                        case Protocol of
                            <<"any">> ->
                                [
                                    %{tcp, FromPort, ToPort, Source},
                                    %{udp, FromPort, ToPort, Source}
                                    case {FromPort, ToPort} of
                                        {0, 65535} ->
                                            {'-1', 0, 0, Source};
                                        {_, _} ->
                                            {'-1', FromPort, ToPort, Source}
                                    end
                                ];
                            <<"udp">> ->
                                {udp, FromPort, ToPort, Source};
                            <<"tcp">> ->
                                {tcp, FromPort, ToPort, Source};
                            <<"icmp">> ->
                                {icmp, FromPort, ToPort, Source}
                        end
                    end,
                    PortsList
                )
            end,
            Services
        )
    ).

%TODO: Add security group source to rules
%protocol port port source
-spec get_ppps_inbound_ec2(ProfileJson :: map(), DestinationRegion :: string()) ->
    SourcePortProtocol :: list().
get_ppps_inbound_ec2(ProfileJson, DestinationRegion) ->
    Inbound = nested:get([<<"rules">>, <<"inbound">>], ProfileJson),
    ActiveInbound = [
        Rule
     || Rule <- Inbound,
        (maps:get(<<"active">>, Rule) == true) and (maps:get(<<"action">>, Rule) == <<"ACCEPT">>)
    ],
    SourceProtocolPorts = lists:map(
        fun(Rule) ->
            ServiceId = maps:get(<<"service">>, Rule),
            {ok, Service} = dog_service:get_by_id(ServiceId),
            Services = maps:get(<<"services">>, Service),
            GroupType = maps:get(<<"group_type">>, Rule),
            %TODO: Description = maps:get(<<"comment">>,Rule), %Not supported by erlcloud
            case GroupType of
                <<"ANY">> ->
                    Sources = [{cidr_ip, "0.0.0.0/0"}],
                    lists:map(
                        fun(Source) ->
                            expand_services(Source, Services)
                        end,
                        Sources
                    );
                G when G =:= <<"ROLE">>; G =:= <<"GROUP">> ->
                    GroupId = maps:get(<<"group">>, Rule),
                    {ok, Group} = dog_group:get_by_id(GroupId),
                    GroupName = maps:get(<<"name">>, Group),
                    Sources =
                        case dog_group:get_ec2_security_group_ids_by_name(GroupName) of
                            [] ->
                                [{cidr_ip, "0.0.0.0/0"}];
                            Ec2GroupIds ->
                                lists:map(
                                    fun(Ec2Group) ->
                                        SgRegion = maps:get(<<"region">>, Ec2Group),
                                        SgId = maps:get(<<"sgid">>, Ec2Group),
                                        Ec2ClassicSgIds = dog_ec2_update_agent:ec2_classic_security_group_ids(
                                            SgRegion
                                        ),

                                        case
                                            lists:member(binary:bin_to_list(SgId), Ec2ClassicSgIds)
                                        of
                                            true ->
                                                {cidr_ip, "0.0.0.0/0"};
                                            false ->
                                                case SgRegion == DestinationRegion of
                                                    false ->
                                                        {cidr_ip, "0.0.0.0/0"};
                                                    true ->
                                                        {group_id, binary:bin_to_list(SgId)}
                                                end
                                        end
                                    end,
                                    Ec2GroupIds
                                )
                        end,
                    lists:map(
                        fun(Source) ->
                            expand_services(Source, Services)
                        end,
                        Sources
                    );
                <<"ZONE">> ->
                    Sources = [{cidr_ip, "0.0.0.0/0"}],
                    lists:map(
                        fun(Source) ->
                            expand_services(Source, Services)
                        end,
                        Sources
                    )
                %TODO If not too long, list public+private IPs of Zone
                %{ok, Zone} = dog_zone:get_by_id(maps:get(<<"group">>,Rule)),
                %?LOG_DEBUG("Zone: ~p~n",[Zone]),
                %Ip4Addresses = maps:get(<<"ipv4_addresses">>,Zone),
                %MaxEc2ZoneSize = application:get_env(dog_trainer,max_ec2_zone_size,5),
                %case length(Ip4Addresses) of
                %    Length when Length > MaxEc2ZoneSize ->
                %        Sources = [{cidr_ip,"0.0.0.0/0"}], %TODO If not too long, list public+private IPs of Zone
                %        lists:map(fun(Source) ->
                %                          expand_services(Source,Services)
                %                  end, Sources);
                %    _Length ->
                %        ZoneRules =  lists:map(fun(Ipv4) ->
                %            Sources = [{cidr_ip,binary:bin_to_list(dog_ips:add_net_to_ipv4(Ipv4))}],
                %            lists:map(fun(Source) ->
                %                              expand_services(Source,Services)
                %                      end, Sources)
                %        end,Ip4Addresses),
                %        lists:flatten(ZoneRules)
                %end
            end
        end,
        ActiveInbound
    ),
    %[tuple_to_list(X) || X <- lists:flatten(SourceProtocolPorts)].
    lists:flatten(SourceProtocolPorts).
%merge_lists_in_tuples(lists:flatten(RawPortsProtocols)).
%dog_common:merge_lists_in_tuples(lists:flatten(SourceProtocolPorts)).

-spec get_ppps_outbound_ec2(ProfileJson :: map(), DestinationRegion :: string()) ->
    SourcePortProtocol :: list().
get_ppps_outbound_ec2(ProfileJson, DestinationRegion) ->
    Outbound = nested:get([<<"rules">>, <<"outbound">>], ProfileJson),
    ActiveOutbound = [
        Rule
     || Rule <- Outbound,
        (maps:get(<<"active">>, Rule) == true) and (maps:get(<<"action">>, Rule) == <<"ACCEPT">>)
    ],
    SourceProtocolPorts = lists:map(
        fun(Rule) ->
            ServiceId = maps:get(<<"service">>, Rule),
            {ok, Service} = dog_service:get_by_id(ServiceId),
            Services = maps:get(<<"services">>, Service),
            GroupType = maps:get(<<"group_type">>, Rule),
            case GroupType of
                <<"ANY">> ->
                    Sources = [{cidr_ip, "0.0.0.0/0"}],
                    lists:map(
                        fun(Source) ->
                            expand_services(Source, Services)
                        end,
                        Sources
                    );
                G when G =:= <<"ROLE">>; G =:= <<"GROUP">> ->
                    GroupId = maps:get(<<"group">>, Rule),
                    {ok, Group} = dog_group:get_by_id(GroupId),
                    GroupName = maps:get(<<"name">>, Group),
                    Sources =
                        case dog_group:get_ec2_security_group_ids_by_name(GroupName) of
                            [] ->
                                [{cidr_ip, "0.0.0.0/0"}];
                            Ec2GroupIds ->
                                lists:map(
                                    fun(Ec2Group) ->
                                        SgRegion = maps:get(<<"region">>, Ec2Group),
                                        SgId = maps:get(<<"sgid">>, Ec2Group),
                                        Ec2ClassicSgIds = dog_ec2_update_agent:ec2_classic_security_group_ids(
                                            SgRegion
                                        ),

                                        case
                                            lists:member(binary:bin_to_list(SgId), Ec2ClassicSgIds)
                                        of
                                            true ->
                                                {cidr_ip, "0.0.0.0/0"};
                                            false ->
                                                case SgRegion == DestinationRegion of
                                                    false ->
                                                        {cidr_ip, "0.0.0.0/0"};
                                                    true ->
                                                        {group_id, binary:bin_to_list(SgId)}
                                                end
                                        end
                                    end,
                                    Ec2GroupIds
                                )
                        end,
                    lists:map(
                        fun(Source) ->
                            expand_services(Source, Services)
                        end,
                        Sources
                    );
                <<"ZONE">> ->
                    Sources = [{cidr_ip, "0.0.0.0/0"}],
                    lists:map(
                        fun(Source) ->
                            expand_services(Source, Services)
                        end,
                        Sources
                    )
            end
        end,
        ActiveOutbound
    ),
    lists:flatten(SourceProtocolPorts).
%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

split(String, Delimiter, all) ->
    split(String, Delimiter).
split(String, Delimiter) ->
    re:split(String, Delimiter, [{return, list}]).

-spec to_hcl_by_id(ProfileId :: iolist()) -> iolist().
to_hcl_by_id(ProfileId) ->
    RulesetId = dog_ruleset:get_id_by_profile_id(ProfileId),
    dog_ruleset_api_v2:to_hcl_by_id(RulesetId).

-spec to_hcl(Profile :: map()) -> binary().
to_hcl(Profile) ->
    dog_profile_api_v2:to_hcl(Profile).
