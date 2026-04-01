-module(dog_ips_tests).
-include_lib("eunit/include/eunit.hrl").

%% is_ipv4/1 tests
is_ipv4_valid_test() ->
    ?assert(dog_ips:is_ipv4("192.168.1.1")).

is_ipv4_cidr_test() ->
    ?assert(dog_ips:is_ipv4("10.0.0.0/8")).

is_ipv4_cidr_32_test() ->
    ?assert(dog_ips:is_ipv4("10.0.0.1/32")).

is_ipv4_invalid_test() ->
    ?assertNot(dog_ips:is_ipv4("not_an_ip")).

is_ipv4_empty_test() ->
    ?assertNot(dog_ips:is_ipv4("")).

is_ipv4_ipv6_test() ->
    ?assertNot(dog_ips:is_ipv4("2001:db8::1")).

%% is_ipv6/1 tests
is_ipv6_full_test() ->
    ?assert(dog_ips:is_ipv6(<<"2001:0db8:85a3:0000:0000:8a2e:0370:7334">>)).

is_ipv6_abbreviated_test() ->
    ?assert(dog_ips:is_ipv6(<<"::1">>)).

is_ipv6_link_local_test() ->
    ?assert(dog_ips:is_ipv6(<<"fe80::1%eth0">>)).

is_ipv6_invalid_test() ->
    ?assertNot(dog_ips:is_ipv6(<<"not_an_ip">>)).

is_ipv6_ipv4_test() ->
    ?assertNot(dog_ips:is_ipv6(<<"192.168.1.1">>)).

%% add_net_to_ipv4/1 tests
add_net_to_ipv4_bare_test() ->
    ?assertEqual(<<"192.168.1.1/32">>, dog_ips:add_net_to_ipv4(<<"192.168.1.1">>)).

add_net_to_ipv4_cidr_test() ->
    ?assertEqual(<<"10.0.0.0/8">>, dog_ips:add_net_to_ipv4(<<"10.0.0.0/8">>)).

%% add_net_to_ipv6 is not exported; tested indirectly via filter_ipv6
filter_ipv6_adds_mask_test() ->
    ?assertEqual([<<"::1/128">>], dog_ips:filter_ipv6([<<"::1">>])).

filter_ipv6_preserves_cidr_test() ->
    ?assertEqual([<<"2001:db8::/32">>], dog_ips:filter_ipv6([<<"2001:db8::/32">>])).

%% uniq/1 tests
uniq_duplicates_test() ->
    ?assertEqual([1, 2, 3], dog_ips:uniq([3, 1, 2, 1, 3])).

uniq_empty_test() ->
    ?assertEqual([], dog_ips:uniq([])).

uniq_already_unique_test() ->
    ?assertEqual([1, 2, 3], dog_ips:uniq([1, 2, 3])).

%% filter_ipv4/1 tests
filter_ipv4_mixed_test() ->
    IPs = [<<"192.168.1.1">>, <<"2001:db8::1">>, <<"10.0.0.1">>],
    Result = dog_ips:filter_ipv4(IPs),
    ?assertEqual([<<"10.0.0.1/32">>, <<"192.168.1.1/32">>], lists:sort(Result)).

filter_ipv4_empty_test() ->
    ?assertEqual([], dog_ips:filter_ipv4([])).

filter_ipv4_dedup_test() ->
    IPs = [<<"192.168.1.1">>, <<"192.168.1.1">>],
    ?assertEqual([<<"192.168.1.1/32">>], dog_ips:filter_ipv4(IPs)).

%% filter_ipv6/1 tests
filter_ipv6_mixed_test() ->
    IPs = [<<"192.168.1.1">>, <<"2001:db8::1">>],
    Result = dog_ips:filter_ipv6(IPs),
    ?assertEqual([<<"2001:db8::1/128">>], Result).

filter_ipv6_empty_test() ->
    ?assertEqual([], dog_ips:filter_ipv6([])).

%% remove_local_ips/1 tests
remove_local_ipv4_ips_test() ->
    IPs = ["192.168.1.1", "127.0.0.1", "10.0.0.1"],
    Result = dog_ips:remove_local_ipv4_ips(IPs),
    ?assertEqual(["192.168.1.1", "10.0.0.1"], Result).

remove_local_ipv6_ips_test() ->
    IPs = ["2001:db8::1", "fe80::1"],
    Result = dog_ips:remove_local_ipv6_ips(IPs),
    ?assertEqual(["2001:db8::1"], Result).

remove_local_ips_combined_test() ->
    IPs = ["192.168.1.1", "127.0.0.1", "fe80::1", "10.0.0.1"],
    Result = dog_ips:remove_local_ips(IPs),
    ?assertEqual(["192.168.1.1", "10.0.0.1"], Result).

%% addresses_from_interfaces/1 tests
addresses_from_interfaces_empty_test() ->
    ?assertEqual({ok, []}, dog_ips:addresses_from_interfaces([])).

addresses_from_interfaces_test() ->
    Interfaces = [{<<"eth0">>, [<<"192.168.1.1">>, <<"10.0.0.1">>]}, {<<"lo">>, [<<"127.0.0.1">>]}],
    {ok, Result} = dog_ips:addresses_from_interfaces(Interfaces),
    ?assertEqual([<<"192.168.1.1">>, <<"10.0.0.1">>], Result).

%% check_config/1 tests (not exported, tested indirectly)
%% sanitize_val tests - not exported, skip
