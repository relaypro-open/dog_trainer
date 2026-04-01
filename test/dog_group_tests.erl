-module(dog_group_tests).
-include_lib("eunit/include/eunit.hrl").

%% ============================================================
%% merge/1 tests
%%
%% merge/1 takes a list of JSON-encoded interface binaries,
%% decodes them, and merges by key using sofs (set of sets).
%% Duplicate keys have their value lists combined and flattened.
%% ============================================================

merge_empty_test() ->
    ?assertEqual([], dog_group:merge([])).

merge_single_interface_test() ->
    Interface = jsx:encode(#{<<"eth0">> => [<<"10.0.0.1">>]}),
    Result = dog_group:merge([Interface]),
    ?assertEqual({<<"eth0">>, [<<"10.0.0.1">>]}, lists:keyfind(<<"eth0">>, 1, Result)).

merge_two_interfaces_same_key_test() ->
    I1 = jsx:encode(#{<<"eth0">> => [<<"10.0.0.1">>]}),
    I2 = jsx:encode(#{<<"eth0">> => [<<"10.0.0.2">>]}),
    Result = dog_group:merge([I1, I2]),
    {<<"eth0">>, IPs} = lists:keyfind(<<"eth0">>, 1, Result),
    ?assertEqual(lists:sort([<<"10.0.0.1">>, <<"10.0.0.2">>]), lists:sort(IPs)).

merge_two_interfaces_different_keys_test() ->
    I1 = jsx:encode(#{<<"eth0">> => [<<"10.0.0.1">>]}),
    I2 = jsx:encode(#{<<"eth1">> => [<<"10.0.1.1">>]}),
    Result = dog_group:merge([I1, I2]),
    ?assert(lists:keyfind(<<"eth0">>, 1, Result) =/= false),
    ?assert(lists:keyfind(<<"eth1">>, 1, Result) =/= false).

merge_multiple_ips_per_interface_test() ->
    I1 = jsx:encode(#{<<"eth0">> => [<<"10.0.0.1">>, <<"10.0.0.2">>]}),
    I2 = jsx:encode(#{<<"eth0">> => [<<"10.0.0.3">>]}),
    Result = dog_group:merge([I1, I2]),
    {<<"eth0">>, IPs} = lists:keyfind(<<"eth0">>, 1, Result),
    ?assertEqual(3, length(IPs)).

merge_three_interfaces_test() ->
    I1 = jsx:encode(#{<<"eth0">> => [<<"10.0.0.1">>]}),
    I2 = jsx:encode(#{<<"eth0">> => [<<"10.0.0.2">>]}),
    I3 = jsx:encode(#{<<"eth0">> => [<<"10.0.0.3">>]}),
    Result = dog_group:merge([I1, I2, I3]),
    {<<"eth0">>, IPs} = lists:keyfind(<<"eth0">>, 1, Result),
    ?assertEqual(3, length(IPs)).

merge_mixed_keys_test() ->
    I1 = jsx:encode(#{<<"eth0">> => [<<"10.0.0.1">>], <<"lo">> => [<<"127.0.0.1">>]}),
    I2 = jsx:encode(#{<<"eth0">> => [<<"10.0.0.2">>]}),
    Result = dog_group:merge([I1, I2]),
    {<<"eth0">>, Eth0IPs} = lists:keyfind(<<"eth0">>, 1, Result),
    {<<"lo">>, LoIPs} = lists:keyfind(<<"lo">>, 1, Result),
    ?assertEqual(2, length(Eth0IPs)),
    ?assertEqual([<<"127.0.0.1">>], LoIPs).

%% ============================================================
%% get_id_by_name/1 - special case tests (pure, no DB)
%%
%% Four hardcoded group names return their own name as the ID.
%% Other names hit the DB, so we only test the pure cases.
%% ============================================================

get_id_by_name_self_test() ->
    ?assertEqual({ok, <<"self">>}, dog_group:get_id_by_name(<<"self">>)).

get_id_by_name_all_active_test() ->
    ?assertEqual({ok, <<"all-active">>}, dog_group:get_id_by_name(<<"all-active">>)).

get_id_by_name_internal_active_test() ->
    ?assertEqual({ok, <<"internal-active">>}, dog_group:get_id_by_name(<<"internal-active">>)).

get_id_by_name_external_active_test() ->
    ?assertEqual({ok, <<"external-active">>}, dog_group:get_id_by_name(<<"external-active">>)).

%% ============================================================
%% get_name_by_id/1 - special case tests (pure, no DB)
%% ============================================================

get_name_by_id_self_test() ->
    ?assertEqual({ok, <<"self">>}, dog_group:get_name_by_id(<<"self">>)).

get_name_by_id_all_active_test() ->
    ?assertEqual({ok, <<"all-active">>}, dog_group:get_name_by_id(<<"all-active">>)).

get_name_by_id_internal_active_test() ->
    ?assertEqual({ok, <<"internal-active">>}, dog_group:get_name_by_id(<<"internal-active">>)).

get_name_by_id_external_active_test() ->
    ?assertEqual({ok, <<"external-active">>}, dog_group:get_name_by_id(<<"external-active">>)).

%% ============================================================
%% get_by_id/1 - special case tests (pure, no DB)
%%
%% "self" returns a fixed map with empty address lists.
%% "any" returns a map with 0.0.0.0 and ::/0.
%% ============================================================

get_by_id_self_test() ->
    {ok, Result} = dog_group:get_by_id(<<"self">>),
    ?assertEqual(<<"self">>, maps:get(<<"name">>, Result)),
    ?assertEqual(<<"self">>, maps:get(<<"id">>, Result)),
    ?assertEqual([], maps:get(<<"external_ipv4_addresses">>, Result)),
    ?assertEqual([], maps:get(<<"external_ipv6_addresses">>, Result)),
    ?assertEqual([], maps:get(<<"internal_ipv4_addresses">>, Result)),
    ?assertEqual([], maps:get(<<"internal_ipv6_addresses">>, Result)).

get_by_id_any_test() ->
    {ok, Result} = dog_group:get_by_id(<<"any">>),
    ?assertEqual(<<"any">>, maps:get(<<"name">>, Result)),
    ?assertEqual([<<"0.0.0.0">>], maps:get(<<"external_ipv4_addresses">>, Result)),
    ?assertEqual([<<"::/0">>], maps:get(<<"external_ipv6_addresses">>, Result)).

%% ============================================================
%% get_by_name/1 - "self" special case (pure, no DB)
%% ============================================================

get_by_name_self_test() ->
    {ok, Result} = dog_group:get_by_name(<<"self">>),
    ?assertEqual(<<"self">>, maps:get(<<"name">>, Result)),
    ?assertEqual([], maps:get(<<"external_ipv4_addresses">>, Result)).

%% ============================================================
%% Special-case "self" returns for IP accessor functions
%%
%% The "self" group always returns empty IP lists since the
%% actual IPs are resolved at the agent level, not server-side.
%% ============================================================

get_all_ips_by_id_self_test() ->
    ?assertEqual({ok, []}, dog_group:get_all_ips_by_id(<<"self">>)).

get_all_ipv4s_by_id_self_test() ->
    ?assertEqual({ok, []}, dog_group:get_all_ipv4s_by_id(<<"self">>)).

get_all_ipv6s_by_id_self_test() ->
    ?assertEqual({ok, []}, dog_group:get_all_ipv6s_by_id(<<"self">>)).

get_internal_ips_by_id_self_test() ->
    ?assertEqual({ok, []}, dog_group:get_internal_ips_by_id(<<"self">>)).

get_internal_ipv4s_by_id_self_test() ->
    ?assertEqual({ok, []}, dog_group:get_internal_ipv4s_by_id(<<"self">>)).

get_internal_ipv6s_by_id_self_test() ->
    ?assertEqual({ok, []}, dog_group:get_internal_ipv6s_by_id(<<"self">>)).

get_external_ips_by_id_self_test() ->
    ?assertEqual({ok, []}, dog_group:get_external_ips_by_id(<<"self">>)).

get_external_ipv4s_by_id_self_test() ->
    ?assertEqual({ok, []}, dog_group:get_external_ipv4s_by_id(<<"self">>)).

get_external_ipv6s_by_id_self_test() ->
    ?assertEqual({ok, []}, dog_group:get_external_ipv6s_by_id(<<"self">>)).

%% ============================================================
%% "internal-active" returns empty for external IP functions
%% "external-active" returns empty for internal IP functions
%% ============================================================

get_internal_ipv4s_by_id_external_active_test() ->
    ?assertEqual({ok, []}, dog_group:get_internal_ipv4s_by_id(<<"external-active">>)).

get_internal_ipv6s_by_id_external_active_test() ->
    ?assertEqual({ok, []}, dog_group:get_internal_ipv6s_by_id(<<"external-active">>)).

get_external_ips_by_id_internal_active_test() ->
    ?assertEqual({ok, []}, dog_group:get_external_ips_by_id(<<"internal-active">>)).

get_external_ipv4s_by_id_internal_active_test() ->
    ?assertEqual({ok, []}, dog_group:get_external_ipv4s_by_id(<<"internal-active">>)).

get_external_ipv6s_by_id_internal_active_test() ->
    ?assertEqual({ok, []}, dog_group:get_external_ipv6s_by_id(<<"internal-active">>)).

%% ============================================================
%% init/0 test
%% ============================================================

init_returns_pass_test() ->
    ?assertEqual(pass, dog_group:init()).
