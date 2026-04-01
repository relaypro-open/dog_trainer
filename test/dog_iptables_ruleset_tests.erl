-module(dog_iptables_ruleset_tests).
-include_lib("eunit/include/eunit.hrl").

%% ============================================================
%% Test fixtures - common test data
%% ============================================================

%% Setup: ensure imetrics app is started (needed by generate_iptables_ruleset)
setup() ->
    application:ensure_all_started(imetrics).

cleanup(_) ->
    ok.

%% A minimal ServiceIdMap entry: SSH on TCP/22
service_id_map() ->
    #{
        <<"svc_ssh">> => #{
            <<"name">> => <<"ssh">>,
            <<"services">> => [
                #{<<"protocol">> => <<"tcp">>, <<"ports">> => [<<"22">>]}
            ]
        },
        <<"svc_http">> => #{
            <<"name">> => <<"http">>,
            <<"services">> => [
                #{<<"protocol">> => <<"tcp">>, <<"ports">> => [<<"80">>, <<"443">>]}
            ]
        },
        <<"svc_any">> => #{
            <<"name">> => <<"any">>,
            <<"services">> => [
                #{<<"protocol">> => <<"any">>, <<"ports">> => []}
            ]
        },
        <<"svc_icmp">> => #{
            <<"name">> => <<"ping">>,
            <<"services">> => [
                #{<<"protocol">> => <<"icmp">>, <<"ports">> => [<<"8">>]}
            ]
        },
        <<"svc_multi">> => #{
            <<"name">> => <<"web_ports">>,
            <<"services">> => [
                #{<<"protocol">> => <<"tcp">>, <<"ports">> => [<<"80">>, <<"443">>, <<"8080">>]}
            ]
        }
    }.

group_id_map() ->
    #{
        <<"grp_web">> => #{<<"name">> => <<"web_servers">>},
        <<"grp_self">> => #{<<"name">> => <<"self">>}
    }.

zone_id_map() ->
    #{
        <<"zone_dmz">> => #{<<"name">> => <<"dmz">>}
    }.

%% Role/Zone maps are keyed by group/zone ID (same as used in rule JSON <<"group">> field)
ipv4_role_map() ->
    #{
        <<"grp_web">> => [<<"10.0.0.1/32">>, <<"10.0.0.2/32">>],
        <<"grp_self">> => [<<"10.0.1.1/32">>]
    }.

ipv6_role_map() ->
    #{
        <<"grp_web">> => [<<"2001:db8::1/128">>, <<"2001:db8::2/128">>],
        <<"grp_self">> => [<<"2001:db8::10/128">>]
    }.

ipv4_zone_map() ->
    #{<<"zone_dmz">> => [<<"192.168.1.0/24">>]}.

ipv6_zone_map() ->
    #{<<"zone_dmz">> => [<<"fd00::0/64">>]}.

%% Build a profile JSON for testing
make_profile(Inbound, Outbound) ->
    make_profile(Inbound, Outbound, <<"undefined">>).

make_profile(Inbound, Outbound, Docker) ->
    #{
        <<"id">> => <<"prof_test">>,
        <<"name">> => <<"test_profile">>,
        <<"docker">> => Docker,
        <<"rules">> => #{
            <<"inbound">> => Inbound,
            <<"outbound">> => Outbound
        }
    }.

%% Build a single rule JSON
make_rule(ServiceId, GroupId, GroupType, Action) ->
    make_rule(ServiceId, GroupId, GroupType, Action, #{}).

make_rule(ServiceId, GroupId, GroupType, Action, Overrides) ->
    Base = #{
        <<"service">> => ServiceId,
        <<"group">> => GroupId,
        <<"group_type">> => GroupType,
        <<"action">> => Action,
        <<"active">> => true,
        <<"states">> => [<<"NEW">>],
        <<"log">> => false,
        <<"log_prefix">> => <<>>,
        <<"comment">> => <<"test_rule">>,
        <<"interface">> => <<>>,
        <<"type">> => <<"BASIC">>,
        <<"environment">> => <<"local">>
    },
    maps:merge(Base, Overrides).

%% Call generate_iptables_ruleset/11 with our test maps
generate(ProfileJson, Type, Version, SelfGroupName) ->
    dog_iptables_ruleset:generate_iptables_ruleset(
        ProfileJson,
        Type,
        Version,
        ipv4_role_map(),
        ipv6_role_map(),
        ipv4_zone_map(),
        ipv6_zone_map(),
        zone_id_map(),
        group_id_map(),
        service_id_map(),
        SelfGroupName
    ).

flatten_result({ok, Result}) ->
    lists:flatten(Result).

%% ============================================================
%% Test suite using fixtures
%% ============================================================

generate_iptables_ruleset_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"v4 ipsets basic inbound ROLE rule", fun v4_ipsets_basic_inbound_role/0},
        {"v4 ipsets empty rules produces header+footer only", fun v4_ipsets_empty_rules/0},
        {"v4 iptables basic inbound ROLE rule (unset)", fun v4_iptables_basic_inbound_role/0},
        {"v6 ipsets basic inbound rule", fun v6_ipsets_basic_inbound/0},
        {"v4 ipsets ZONE group type", fun v4_ipsets_zone_group/0},
        {"v4 ipsets ANY group type", fun v4_ipsets_any_group/0},
        {"v4 ipsets inactive rule skipped", fun v4_ipsets_inactive_rule/0},
        {"v4 ipsets DROP inbound skips symmetric outbound", fun v4_ipsets_drop_skips_symmetric/0},
        {"v4 docker profile", fun v4_docker_profile/0},
        {"v4 ipsets outbound rule", fun v4_ipsets_outbound/0},
        {"v4 ipsets multiple ports uses multiport", fun v4_ipsets_multiport/0},
        {"v4 ipsets ICMP rule", fun v4_ipsets_icmp/0},
        {"v4 ipsets REJECT action", fun v4_ipsets_reject_action/0},
        {"v6 ipsets REJECT action", fun v6_ipsets_reject_action/0},
        {"v4 ipsets rule with interface", fun v4_ipsets_with_interface/0},
        {"v4 ipsets self group resolves to SelfGroupName", fun v4_ipsets_self_group/0},
        {"v4 ipsets rule with comment containing spaces", fun v4_ipsets_comment_with_spaces/0},
        {"v4 ipsets connlimit rule type", fun v4_ipsets_connlimit/0},
        {"v4 ipsets recent rule type", fun v4_ipsets_recent/0},
        {"v4 ipsets rule with states", fun v4_ipsets_with_states/0},
        {"v4 ipsets rule with no states", fun v4_ipsets_no_states/0},
        {"v4 ipsets ANY service", fun v4_ipsets_any_service/0},
        {"v4 iptables ZONE rule expands addresses", fun v4_iptables_zone_expands/0},
        {"v4 ipsets environment non-local", fun v4_ipsets_environment_nonlocal/0}
    ]}.

%% ============================================================
%% Individual test functions
%% ============================================================

v4_ipsets_basic_inbound_role() ->
    Rule = make_rule(<<"svc_ssh">>, <<"grp_web">>, <<"ROLE">>, <<"ACCEPT">>),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    %% Should contain iptables header
    ?assert(string:find(Result, "*filter") =/= nomatch),
    ?assert(string:find(Result, ":INPUT DROP") =/= nomatch),
    %% Should contain the SSH rule with ipset match
    ?assert(string:find(Result, "web_servers_gv4") =/= nomatch),
    ?assert(string:find(Result, "--dport 22") =/= nomatch),
    ?assert(string:find(Result, "-j ACCEPT") =/= nomatch),
    ?assert(string:find(Result, "-p tcp") =/= nomatch),
    %% Should contain COMMIT
    ?assert(string:find(Result, "COMMIT") =/= nomatch).

v4_ipsets_empty_rules() ->
    Profile = make_profile([], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    ?assert(string:find(Result, "*filter") =/= nomatch),
    ?assert(string:find(Result, "COMMIT") =/= nomatch),
    %% No custom rules, just headers and footers
    ?assert(string:find(Result, ":INPUT DROP") =/= nomatch),
    ?assert(string:find(Result, ":FORWARD DROP") =/= nomatch),
    ?assert(string:find(Result, ":OUTPUT ACCEPT") =/= nomatch).

v4_iptables_basic_inbound_role() ->
    Rule = make_rule(<<"svc_ssh">>, <<"grp_web">>, <<"ROLE">>, <<"ACCEPT">>),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, iptables, <<"v4">>, <<"my_group">>)),
    %% iptables (unset) mode should expand IPs directly instead of ipset names
    ?assert(string:find(Result, "10.0.0.1/32") =/= nomatch),
    ?assert(string:find(Result, "10.0.0.2/32") =/= nomatch),
    ?assert(string:find(Result, "--dport 22") =/= nomatch).

v6_ipsets_basic_inbound() ->
    Rule = make_rule(<<"svc_ssh">>, <<"grp_web">>, <<"ROLE">>, <<"ACCEPT">>),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v6">>, <<"my_group">>)),
    %% v6 header has different chains
    ?assert(string:find(Result, ":OUTPUT DROP") =/= nomatch),
    ?assert(string:find(Result, ":ICMPFLOOD") =/= nomatch),
    %% v6 ipset name should use v6 suffix
    ?assert(string:find(Result, "web_servers_gv6") =/= nomatch),
    %% v6 inbound header has ICMPv6 rules
    ?assert(string:find(Result, "ipv6-icmp") =/= nomatch).

v4_ipsets_zone_group() ->
    Rule = make_rule(<<"svc_ssh">>, <<"zone_dmz">>, <<"ZONE">>, <<"ACCEPT">>),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    %% Zone uses _z suffix
    ?assert(string:find(Result, "dmz_zv4") =/= nomatch).

v4_ipsets_any_group() ->
    Rule = make_rule(<<"svc_ssh">>, <<"any">>, <<"ANY">>, <<"ACCEPT">>),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    %% ANY group should not have an ipset match-set, just protocol/port
    ?assert(string:find(Result, "match-set") =:= nomatch),
    ?assert(string:find(Result, "--dport 22") =/= nomatch).

v4_ipsets_inactive_rule() ->
    Rule = make_rule(
        <<"svc_ssh">>,
        <<"grp_web">>,
        <<"ROLE">>,
        <<"ACCEPT">>,
        #{<<"active">> => false}
    ),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    %% Inactive rule should not appear - no --dport 22
    ?assert(string:find(Result, "--dport 22") =:= nomatch).

v4_ipsets_drop_skips_symmetric() ->
    Rule = make_rule(<<"svc_ssh">>, <<"grp_web">>, <<"ROLE">>, <<"DROP">>),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    %% INPUT should have the DROP rule
    ?assert(string:find(Result, "-A INPUT") =/= nomatch),
    %% OUTPUT should NOT have a symmetric rule for DROP
    Lines = string:split(Result, "\n", all),
    OutputRuleLines = [
        L
     || L <- Lines,
        string:find(L, "-A OUTPUT") =/= nomatch,
        string:find(L, "web_servers") =/= nomatch
    ],
    ?assertEqual([], OutputRuleLines).

v4_docker_profile() ->
    Rule = make_rule(<<"svc_ssh">>, <<"grp_web">>, <<"ROLE">>, <<"ACCEPT">>),
    Profile = make_profile([Rule], [], <<"true">>),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    %% Docker profile should have docker-specific chains
    ?assert(string:find(Result, ":DOCKER -") =/= nomatch),
    ?assert(string:find(Result, "DOCKER-ISOLATION-STAGE-1") =/= nomatch),
    ?assert(string:find(Result, "DOCKER-USER") =/= nomatch).

v4_ipsets_outbound() ->
    Rule = make_rule(<<"svc_ssh">>, <<"grp_web">>, <<"ROLE">>, <<"ACCEPT">>),
    Profile = make_profile([], [Rule]),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    %% Outbound rule should be in OUTPUT chain with dst
    ?assert(string:find(Result, "-A OUTPUT") =/= nomatch),
    Lines = string:split(Result, "\n", all),
    OutputSshLines = [
        L
     || L <- Lines,
        string:find(L, "-A OUTPUT") =/= nomatch,
        string:find(L, "--dport 22") =/= nomatch
    ],
    ?assert(length(OutputSshLines) > 0).

v4_ipsets_multiport() ->
    Rule = make_rule(<<"svc_multi">>, <<"grp_web">>, <<"ROLE">>, <<"ACCEPT">>),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    %% Multiple ports should use multiport module
    ?assert(string:find(Result, "multiport") =/= nomatch),
    ?assert(string:find(Result, "--dports") =/= nomatch),
    ?assert(string:find(Result, "80,443,8080") =/= nomatch).

v4_ipsets_icmp() ->
    Rule = make_rule(<<"svc_icmp">>, <<"grp_web">>, <<"ROLE">>, <<"ACCEPT">>),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    %% ICMP should use icmp module
    ?assert(string:find(Result, "-p icmp") =/= nomatch),
    ?assert(string:find(Result, "--icmp-type") =/= nomatch),
    %% Symmetric outbound should convert ping request (8) to reply (0)
    Lines = string:split(Result, "\n", all),
    OutputIcmpLines = [
        L
     || L <- Lines,
        string:find(L, "-A OUTPUT") =/= nomatch,
        string:find(L, "icmp") =/= nomatch
    ],
    OutputIcmpStr = lists:flatten(OutputIcmpLines),
    ?assert(string:find(OutputIcmpStr, " 0") =/= nomatch).

v4_ipsets_reject_action() ->
    Rule = make_rule(<<"svc_ssh">>, <<"grp_web">>, <<"ROLE">>, <<"REJECT">>),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    ?assert(string:find(Result, "REJECT --reject-with icmp-port-unreachable") =/= nomatch).

v6_ipsets_reject_action() ->
    Rule = make_rule(<<"svc_ssh">>, <<"grp_web">>, <<"ROLE">>, <<"REJECT">>),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v6">>, <<"my_group">>)),
    ?assert(string:find(Result, "REJECT --reject-with icmp6-port-unreachable") =/= nomatch).

v4_ipsets_with_interface() ->
    Rule = make_rule(
        <<"svc_ssh">>,
        <<"grp_web">>,
        <<"ROLE">>,
        <<"ACCEPT">>,
        #{<<"interface">> => <<"eth0">>}
    ),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    ?assert(string:find(Result, "-i eth0") =/= nomatch).

v4_ipsets_self_group() ->
    Rule = make_rule(<<"svc_ssh">>, <<"grp_self">>, <<"ROLE">>, <<"ACCEPT">>),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    %% "self" group should resolve to SelfGroupName ("my_group")
    ?assert(string:find(Result, "my_group_gv4") =/= nomatch).

v4_ipsets_comment_with_spaces() ->
    Rule = make_rule(
        <<"svc_ssh">>,
        <<"grp_web">>,
        <<"ROLE">>,
        <<"ACCEPT">>,
        #{<<"comment">> => <<"ssh access rule">>}
    ),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    %% Comment with spaces should be quoted
    ?assert(string:find(Result, "\"ssh access rule\"") =/= nomatch).

v4_ipsets_connlimit() ->
    Rule = make_rule(
        <<"svc_ssh">>,
        <<"grp_web">>,
        <<"ROLE">>,
        <<"ACCEPT">>,
        #{
            <<"type">> => <<"CONNLIMIT">>,
            <<"conn_limit_above">> => 20,
            <<"conn_limit_mask">> => 24
        }
    ),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    ?assert(string:find(Result, "connlimit") =/= nomatch),
    ?assert(string:find(Result, "--connlimit-above 20") =/= nomatch),
    ?assert(string:find(Result, "--connlimit-mask 24") =/= nomatch),
    %% Connlimit always uses tcp-reset reject
    ?assert(string:find(Result, "REJECT --reject-with tcp-reset") =/= nomatch).

v4_ipsets_recent() ->
    Rule = make_rule(
        <<"svc_ssh">>,
        <<"grp_web">>,
        <<"ROLE">>,
        <<"ACCEPT">>,
        #{
            <<"type">> => <<"RECENT">>,
            <<"recent_name">> => <<"ssh_brute">>,
            <<"recent_mask">> => [],
            <<"seconds">> => 300,
            <<"hit_count">> => 10
        }
    ),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    ?assert(string:find(Result, "recent") =/= nomatch),
    ?assert(string:find(Result, "--seconds 300") =/= nomatch),
    ?assert(string:find(Result, "--hitcount 10") =/= nomatch),
    ?assert(string:find(Result, "--name ssh_brute") =/= nomatch),
    %% Default v4 mask
    ?assert(string:find(Result, "--mask 255.255.255.255") =/= nomatch),
    %% Should also have a --set rule
    ?assert(string:find(Result, "--set --name ssh_brute") =/= nomatch).

v4_ipsets_with_states() ->
    Rule = make_rule(
        <<"svc_ssh">>,
        <<"grp_web">>,
        <<"ROLE">>,
        <<"ACCEPT">>,
        #{<<"states">> => [<<"NEW">>, <<"ESTABLISHED">>]}
    ),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    ?assert(string:find(Result, "--state NEW,ESTABLISHED") =/= nomatch).

v4_ipsets_no_states() ->
    Rule = make_rule(
        <<"svc_ssh">>,
        <<"grp_web">>,
        <<"ROLE">>,
        <<"ACCEPT">>,
        #{<<"states">> => []}
    ),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    %% Inbound rule with empty states should have no --state on the INPUT line
    Lines = string:split(Result, "\n", all),
    InputSshLines = [
        L
     || L <- Lines,
        string:find(L, "-A INPUT") =/= nomatch,
        string:find(L, "--dport 22") =/= nomatch
    ],
    InputSshStr = lists:flatten(InputSshLines),
    ?assert(string:find(InputSshStr, "--state") =:= nomatch),
    %% But symmetric outbound should still have RELATED,ESTABLISHED
    OutputSshLines = [
        L
     || L <- Lines,
        string:find(L, "-A OUTPUT") =/= nomatch,
        string:find(L, "web_servers") =/= nomatch
    ],
    OutputSshStr = lists:flatten(OutputSshLines),
    ?assert(string:find(OutputSshStr, "RELATED,ESTABLISHED") =/= nomatch).

v4_ipsets_any_service() ->
    Rule = make_rule(<<"svc_any">>, <<"grp_web">>, <<"ROLE">>, <<"ACCEPT">>),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    %% ANY service should use ipset match but no protocol/port
    ?assert(string:find(Result, "match-set") =/= nomatch),
    ?assert(string:find(Result, "--dport") =:= nomatch).

v4_iptables_zone_expands() ->
    Rule = make_rule(<<"svc_ssh">>, <<"zone_dmz">>, <<"ZONE">>, <<"ACCEPT">>),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, iptables, <<"v4">>, <<"my_group">>)),
    %% iptables (unset) mode with zone should expand zone IPs
    ?assert(string:find(Result, "192.168.1.0/24") =/= nomatch).

v4_ipsets_environment_nonlocal() ->
    Rule = make_rule(
        <<"svc_ssh">>,
        <<"grp_web">>,
        <<"ROLE">>,
        <<"ACCEPT">>,
        #{<<"environment">> => <<"staging">>}
    ),
    Profile = make_profile([Rule], []),
    Result = flatten_result(generate(Profile, ipsets, <<"v4">>, <<"my_group">>)),
    %% Non-local environment should prefix the group with environment#
    ?assert(string:find(Result, "staging#") =/= nomatch).
