-module(dog_host_tests).
-include_lib("eunit/include/eunit.hrl").

%% ============================================================
%% interfaces_to_ips/1 tests
%%
%% Takes a JSON-encoded binary of interface->IP mappings,
%% decodes it, flattens all IP values, and removes 127.0.0.1.
%% ============================================================

interfaces_to_ips_single_interface_test() ->
    Json = jsx:encode(#{<<"eth0">> => [<<"10.0.0.1">>]}),
    Result = dog_host:interfaces_to_ips(Json),
    ?assertEqual([<<"10.0.0.1">>], Result).

interfaces_to_ips_multiple_ips_test() ->
    Json = jsx:encode(#{<<"eth0">> => [<<"10.0.0.1">>, <<"10.0.0.2">>]}),
    Result = dog_host:interfaces_to_ips(Json),
    ?assertEqual(lists:sort([<<"10.0.0.1">>, <<"10.0.0.2">>]), lists:sort(Result)).

interfaces_to_ips_multiple_interfaces_test() ->
    Json = jsx:encode(#{
        <<"eth0">> => [<<"10.0.0.1">>],
        <<"eth1">> => [<<"192.168.1.1">>]
    }),
    Result = dog_host:interfaces_to_ips(Json),
    ?assertEqual(2, length(Result)),
    ?assert(lists:member(<<"10.0.0.1">>, Result)),
    ?assert(lists:member(<<"192.168.1.1">>, Result)).

interfaces_to_ips_removes_localhost_test() ->
    Json = jsx:encode(#{
        <<"lo">> => [<<"127.0.0.1">>],
        <<"eth0">> => [<<"10.0.0.1">>]
    }),
    Result = dog_host:interfaces_to_ips(Json),
    ?assertNot(lists:member(<<"127.0.0.1">>, Result)),
    ?assert(lists:member(<<"10.0.0.1">>, Result)).

interfaces_to_ips_only_localhost_test() ->
    Json = jsx:encode(#{<<"lo">> => [<<"127.0.0.1">>]}),
    Result = dog_host:interfaces_to_ips(Json),
    ?assertEqual([], Result).

interfaces_to_ips_empty_interfaces_test() ->
    Json = jsx:encode(#{}),
    Result = dog_host:interfaces_to_ips(Json),
    ?assertEqual([], Result).

interfaces_to_ips_ipv6_preserved_test() ->
    Json = jsx:encode(#{<<"eth0">> => [<<"2001:db8::1">>, <<"10.0.0.1">>]}),
    Result = dog_host:interfaces_to_ips(Json),
    ?assert(lists:member(<<"2001:db8::1">>, Result)),
    ?assert(lists:member(<<"10.0.0.1">>, Result)).

interfaces_to_ips_empty_ip_list_test() ->
    Json = jsx:encode(#{<<"eth0">> => []}),
    Result = dog_host:interfaces_to_ips(Json),
    ?assertEqual([], Result).

interfaces_to_ips_mixed_with_localhost_test() ->
    Json = jsx:encode(#{
        <<"lo">> => [<<"127.0.0.1">>],
        <<"eth0">> => [<<"10.0.0.1">>, <<"10.0.0.2">>],
        <<"eth1">> => [<<"192.168.1.1">>]
    }),
    Result = dog_host:interfaces_to_ips(Json),
    ?assertEqual(3, length(Result)),
    ?assertNot(lists:member(<<"127.0.0.1">>, Result)).

%% ============================================================
%% init/0 test
%% ============================================================

init_returns_pass_test() ->
    ?assertEqual(pass, dog_host:init()).

%% ============================================================
%% Test-exported pure functions
%%
%% The following tests require TEST-only exports added to
%% dog_host.erl. They test the state machine and hash logic
%% which are the most critical pure logic in this module.
%% ============================================================

-ifdef(TEST).
%% These functions are exported from dog_host under -ifdef(TEST).

get_state_from_host_active_hashpass_test() ->
    Host = #{<<"active">> => <<"active">>, <<"hashpass">> => true},
    ?assertEqual({ok, <<"active">>}, dog_host:get_state_from_host(Host)).

get_state_from_host_active_hashfail_test() ->
    Host = #{<<"active">> => <<"active">>, <<"hashpass">> => false},
    ?assertEqual({ok, <<"active_hashfail">>}, dog_host:get_state_from_host(Host)).

get_state_from_host_new_test() ->
    Host = #{<<"active">> => <<"new">>, <<"hashpass">> => true},
    ?assertEqual({ok, <<"new">>}, dog_host:get_state_from_host(Host)).

get_state_from_host_inactive_test() ->
    Host = #{<<"active">> => <<"inactive">>, <<"hashpass">> => true},
    ?assertEqual({ok, <<"inactive">>}, dog_host:get_state_from_host(Host)).

get_state_from_host_inactive_hashfail_test() ->
    Host = #{<<"active">> => <<"inactive">>, <<"hashpass">> => false},
    ?assertEqual({ok, <<"inactive_hashfail">>}, dog_host:get_state_from_host(Host)).

get_state_from_host_retired_test() ->
    Host = #{<<"active">> => <<"retired">>, <<"hashpass">> => true},
    ?assertEqual({ok, <<"retired">>}, dog_host:get_state_from_host(Host)).

get_state_from_host_defaults_test() ->
    Host = #{},
    ?assertEqual({ok, <<"new">>}, dog_host:get_state_from_host(Host)).

filter_out_retired_hosts_test() ->
    Hosts = [
        #{<<"name">> => <<"h1">>, <<"active">> => <<"active">>},
        #{<<"name">> => <<"h2">>, <<"active">> => <<"retired">>},
        #{<<"name">> => <<"h3">>, <<"active">> => <<"inactive">>}
    ],
    Result = dog_host:filter_out_retired_hosts(Hosts),
    Names = [maps:get(<<"name">>, H) || H <- Result],
    ?assertEqual([<<"h1">>, <<"h3">>], Names).

filter_out_retired_hosts_empty_test() ->
    ?assertEqual([], dog_host:filter_out_retired_hosts([])).

filter_out_retired_hosts_all_retired_test() ->
    Hosts = [#{<<"active">> => <<"retired">>}],
    ?assertEqual([], dog_host:filter_out_retired_hosts(Hosts)).

host_alert_active_default_true_test() ->
    ?assertEqual(true, dog_host:host_alert_active(#{})).

host_alert_active_explicit_true_test() ->
    ?assertEqual(true, dog_host:host_alert_active(#{<<"alert_enable">> => true})).

host_alert_active_false_test() ->
    ?assertEqual(false, dog_host:host_alert_active(#{<<"alert_enable">> => false})).

iptables_hash_logic_all_true_v6_enabled_test() ->
    application:set_env(dog_trainer, check_v6_hashes, true),
    ?assertEqual(true, dog_host:iptables_hash_logic(true, true, true, true)).

iptables_hash_logic_v4_fail_test() ->
    application:set_env(dog_trainer, check_v6_hashes, true),
    ?assertEqual(false, dog_host:iptables_hash_logic(false, true, true, true)).

iptables_hash_logic_v6_disabled_ignores_v6_test() ->
    application:set_env(dog_trainer, check_v6_hashes, false),
    ?assertEqual(true, dog_host:iptables_hash_logic(true, false, true, false)).

iptables_hash_logic_v6_disabled_v4_fail_test() ->
    application:set_env(dog_trainer, check_v6_hashes, false),
    ?assertEqual(false, dog_host:iptables_hash_logic(false, true, true, true)).

-endif.
