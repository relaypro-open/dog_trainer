-module(dog_profile_tests).
-include_lib("eunit/include/eunit.hrl").

%% ============================================================
%% Helper: build profile maps for testing
%% ============================================================

make_rule(Group, GroupType, Active) ->
    make_rule(Group, GroupType, Active, <<"ACCEPT">>).

make_rule(Group, GroupType, Active, Action) ->
    #{
        <<"group">> => Group,
        <<"group_type">> => GroupType,
        <<"active">> => Active,
        <<"action">> => Action,
        <<"service">> => <<"svc1">>,
        <<"states">> => [<<"NEW">>],
        <<"log">> => false,
        <<"log_prefix">> => <<>>,
        <<"comment">> => <<"test">>,
        <<"interface">> => <<>>,
        <<"type">> => <<"BASIC">>,
        <<"environment">> => <<"local">>
    }.

make_profile(Inbound, Outbound) ->
    #{
        <<"id">> => <<"prof1">>,
        <<"name">> => <<"test_profile">>,
        <<"rules">> => #{
            <<"inbound">> => Inbound,
            <<"outbound">> => Outbound
        }
    }.

%% ============================================================
%% create_hash/1 tests
%%
%% create_hash normalizes the ruleset (removes comments, docker
%% lines, quotes, zeros counters, trims whitespace, removes
%% blank lines) then SHA-256 hashes the result.
%% ============================================================

create_hash_test_() ->
    {setup, fun() -> application:ensure_all_started(crypto) end, fun(_) -> ok end, [
        {"deterministic for same input", fun create_hash_deterministic/0},
        {"different inputs produce different hashes", fun create_hash_different_inputs/0},
        {"comments are stripped before hashing", fun create_hash_strips_comments/0},
        {"counters are zeroed before hashing", fun create_hash_zeros_counters/0},
        {"quotes are stripped before hashing", fun create_hash_strips_quotes/0},
        {"docker lines are stripped before hashing", fun create_hash_strips_docker/0},
        {"trailing whitespace is stripped", fun create_hash_strips_trailing_whitespace/0},
        {"empty lines are stripped", fun create_hash_strips_empty_lines/0},
        {"returns a binary", fun create_hash_returns_binary/0}
    ]}.

create_hash_deterministic() ->
    Input = "*filter\n:INPUT DROP [0:0]\nCOMMIT\n",
    Hash1 = dog_profile:create_hash(Input),
    Hash2 = dog_profile:create_hash(Input),
    ?assertEqual(Hash1, Hash2).

create_hash_different_inputs() ->
    Hash1 = dog_profile:create_hash("*filter\n:INPUT DROP [0:0]\nCOMMIT\n"),
    Hash2 = dog_profile:create_hash("*filter\n:INPUT ACCEPT [0:0]\nCOMMIT\n"),
    ?assertNotEqual(Hash1, Hash2).

create_hash_strips_comments() ->
    %% A ruleset with and without comments should hash the same
    Base = "*filter\n:INPUT DROP [0:0]\nCOMMIT\n",
    WithComments = "# This is a comment\n*filter\n:INPUT DROP [0:0]\nCOMMIT\n",
    ?assertEqual(dog_profile:create_hash(Base), dog_profile:create_hash(WithComments)).

create_hash_zeros_counters() ->
    %% Different counter values should normalize to [0:0]
    Low = "*filter\n:INPUT DROP [0:0]\nCOMMIT\n",
    High = "*filter\n:INPUT DROP [12345:67890]\nCOMMIT\n",
    ?assertEqual(dog_profile:create_hash(Low), dog_profile:create_hash(High)).

create_hash_strips_quotes() ->
    %% Single and double quotes should be removed
    WithQuotes = "*filter\n-A INPUT -m comment --comment \"ssh rule\"\nCOMMIT\n",
    WithoutQuotes = "*filter\n-A INPUT -m comment --comment ssh rule\nCOMMIT\n",
    ?assertEqual(dog_profile:create_hash(WithQuotes), dog_profile:create_hash(WithoutQuotes)).

create_hash_strips_docker() ->
    %% Docker-related lines should be removed
    Base =
        "*filter\n:INPUT DROP [0:0]\n:FORWARD DROP [0:0]\n:OUTPUT ACCEPT [0:0]\n"
        "-A FORWARD -j REJECT --reject-with icmp-port-unreachable\nCOMMIT\n",
    WithDocker =
        "*filter\n:INPUT DROP [0:0]\n:FORWARD DROP [0:0]\n:OUTPUT ACCEPT [0:0]\n"
        ":DOCKER - [0:0]\n:DOCKER-ISOLATION-STAGE-1 - [0:0]\n"
        "-A DOCKER-USER -j RETURN\n"
        "-A FORWARD -j DOCKER-USER\n"
        "-A FORWARD -j REJECT --reject-with icmp-port-unreachable\nCOMMIT\n",
    ?assertEqual(dog_profile:create_hash(Base), dog_profile:create_hash(WithDocker)).

create_hash_strips_trailing_whitespace() ->
    Base = "*filter\n:INPUT DROP [0:0]\nCOMMIT\n",
    WithTrailing = "*filter  \n:INPUT DROP [0:0]  \nCOMMIT  \n",
    ?assertEqual(dog_profile:create_hash(Base), dog_profile:create_hash(WithTrailing)).

create_hash_strips_empty_lines() ->
    Base = "*filter\n:INPUT DROP [0:0]\nCOMMIT\n",
    WithEmpty = "*filter\n\n:INPUT DROP [0:0]\n\n\nCOMMIT\n",
    ?assertEqual(dog_profile:create_hash(Base), dog_profile:create_hash(WithEmpty)).

create_hash_returns_binary() ->
    Result = dog_profile:create_hash("*filter\nCOMMIT\n"),
    ?assert(is_binary(Result)),
    %% SHA-256 as hex = 64 characters
    ?assertEqual(64, byte_size(Result)).

%% ============================================================
%% get_role_groups_in_profile/1 tests
%% ============================================================

get_role_groups_empty_rules_test() ->
    Profile = make_profile([], []),
    ?assertEqual([], dog_profile:get_role_groups_in_profile(Profile)).

get_role_groups_inbound_only_test() ->
    Rule1 = make_rule(<<"grp1">>, <<"ROLE">>, true),
    Rule2 = make_rule(<<"grp2">>, <<"GROUP">>, true),
    Profile = make_profile([Rule1, Rule2], []),
    Result = lists:sort(dog_profile:get_role_groups_in_profile(Profile)),
    ?assertEqual([<<"grp1">>, <<"grp2">>], Result).

get_role_groups_outbound_only_test() ->
    Rule1 = make_rule(<<"grp3">>, <<"ROLE">>, true),
    Profile = make_profile([], [Rule1]),
    ?assertEqual([<<"grp3">>], dog_profile:get_role_groups_in_profile(Profile)).

get_role_groups_both_directions_test() ->
    RuleIn = make_rule(<<"grp1">>, <<"ROLE">>, true),
    RuleOut = make_rule(<<"grp2">>, <<"GROUP">>, true),
    Profile = make_profile([RuleIn], [RuleOut]),
    Result = lists:sort(dog_profile:get_role_groups_in_profile(Profile)),
    ?assertEqual([<<"grp1">>, <<"grp2">>], Result).

get_role_groups_deduplicates_test() ->
    Rule1 = make_rule(<<"grp1">>, <<"ROLE">>, true),
    Rule2 = make_rule(<<"grp1">>, <<"ROLE">>, true),
    Profile = make_profile([Rule1, Rule2], []),
    ?assertEqual([<<"grp1">>], dog_profile:get_role_groups_in_profile(Profile)).

get_role_groups_skips_zone_test() ->
    RuleRole = make_rule(<<"grp1">>, <<"ROLE">>, true),
    RuleZone = make_rule(<<"zone1">>, <<"ZONE">>, true),
    Profile = make_profile([RuleRole, RuleZone], []),
    ?assertEqual([<<"grp1">>], dog_profile:get_role_groups_in_profile(Profile)).

get_role_groups_skips_any_test() ->
    RuleRole = make_rule(<<"grp1">>, <<"ROLE">>, true),
    RuleAny = make_rule(<<"any">>, <<"ANY">>, true),
    Profile = make_profile([RuleRole, RuleAny], []),
    ?assertEqual([<<"grp1">>], dog_profile:get_role_groups_in_profile(Profile)).

get_role_groups_skips_inactive_test() ->
    ActiveRule = make_rule(<<"grp1">>, <<"ROLE">>, true),
    InactiveRule = make_rule(<<"grp2">>, <<"ROLE">>, false),
    Profile = make_profile([ActiveRule, InactiveRule], []),
    ?assertEqual([<<"grp1">>], dog_profile:get_role_groups_in_profile(Profile)).

get_role_groups_includes_group_type_test() ->
    %% <<"GROUP">> should be treated same as <<"ROLE">>
    Rule = make_rule(<<"grp1">>, <<"GROUP">>, true),
    Profile = make_profile([Rule], []),
    ?assertEqual([<<"grp1">>], dog_profile:get_role_groups_in_profile(Profile)).

%% ============================================================
%% get_zone_groups_in_profile/1 tests
%% ============================================================

get_zone_groups_empty_rules_test() ->
    Profile = make_profile([], []),
    ?assertEqual([], dog_profile:get_zone_groups_in_profile(Profile)).

get_zone_groups_inbound_only_test() ->
    Rule = make_rule(<<"zone1">>, <<"ZONE">>, true),
    Profile = make_profile([Rule], []),
    ?assertEqual([<<"zone1">>], dog_profile:get_zone_groups_in_profile(Profile)).

get_zone_groups_outbound_only_test() ->
    Rule = make_rule(<<"zone2">>, <<"ZONE">>, true),
    Profile = make_profile([], [Rule]),
    ?assertEqual([<<"zone2">>], dog_profile:get_zone_groups_in_profile(Profile)).

get_zone_groups_both_directions_test() ->
    RuleIn = make_rule(<<"zone1">>, <<"ZONE">>, true),
    RuleOut = make_rule(<<"zone2">>, <<"ZONE">>, true),
    Profile = make_profile([RuleIn], [RuleOut]),
    Result = lists:sort(dog_profile:get_zone_groups_in_profile(Profile)),
    ?assertEqual([<<"zone1">>, <<"zone2">>], Result).

get_zone_groups_deduplicates_test() ->
    Rule1 = make_rule(<<"zone1">>, <<"ZONE">>, true),
    Rule2 = make_rule(<<"zone1">>, <<"ZONE">>, true),
    Profile = make_profile([Rule1, Rule2], []),
    ?assertEqual([<<"zone1">>], dog_profile:get_zone_groups_in_profile(Profile)).

get_zone_groups_skips_role_test() ->
    RuleZone = make_rule(<<"zone1">>, <<"ZONE">>, true),
    RuleRole = make_rule(<<"grp1">>, <<"ROLE">>, true),
    Profile = make_profile([RuleZone, RuleRole], []),
    ?assertEqual([<<"zone1">>], dog_profile:get_zone_groups_in_profile(Profile)).

get_zone_groups_skips_inactive_test() ->
    ActiveRule = make_rule(<<"zone1">>, <<"ZONE">>, true),
    InactiveRule = make_rule(<<"zone2">>, <<"ZONE">>, false),
    Profile = make_profile([ActiveRule, InactiveRule], []),
    ?assertEqual([<<"zone1">>], dog_profile:get_zone_groups_in_profile(Profile)).

%% ============================================================
%% date_string/0 tests
%% ============================================================

date_string_returns_string_test() ->
    Result = lists:flatten(dog_profile:date_string()),
    ?assert(is_list(Result)),
    ?assert(length(Result) > 0).

date_string_contains_day_name_test() ->
    Result = lists:flatten(dog_profile:date_string()),
    Days = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"],
    DayFound = lists:any(fun(Day) -> string:find(Result, Day) =/= nomatch end, Days),
    ?assert(DayFound).

date_string_contains_month_name_test() ->
    Result = lists:flatten(dog_profile:date_string()),
    Months = [
        "Jan",
        "Feb",
        "Mar",
        "Apr",
        "May",
        "Jun",
        "Jul",
        "Aug",
        "Sep",
        "Oct",
        "Nov",
        "Dec"
    ],
    MonthFound = lists:any(fun(M) -> string:find(Result, M) =/= nomatch end, Months),
    ?assert(MonthFound).

date_string_contains_utc_test() ->
    Result = lists:flatten(dog_profile:date_string()),
    ?assert(string:find(Result, "UTC") =/= nomatch).

%% ============================================================
%% Mixed profile scenarios
%% ============================================================

mixed_profile_role_and_zone_test() ->
    RuleRole = make_rule(<<"grp1">>, <<"ROLE">>, true),
    RuleZone = make_rule(<<"zone1">>, <<"ZONE">>, true),
    RuleAny = make_rule(<<"any">>, <<"ANY">>, true),
    RuleInactive = make_rule(<<"grp2">>, <<"ROLE">>, false),
    Profile = make_profile([RuleRole, RuleZone, RuleAny, RuleInactive], []),
    ?assertEqual([<<"grp1">>], dog_profile:get_role_groups_in_profile(Profile)),
    ?assertEqual([<<"zone1">>], dog_profile:get_zone_groups_in_profile(Profile)).
