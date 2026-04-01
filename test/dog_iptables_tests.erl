-module(dog_iptables_tests).
-include_lib("eunit/include/eunit.hrl").

%% chunk_list/1 tests (default chunk size 2)
chunk_list_default_test() ->
    ?assertEqual([[1, 2], [3, 4]], dog_iptables:chunk_list([1, 2, 3, 4])).

chunk_list_default_odd_test() ->
    ?assertEqual([[1, 2], [3]], dog_iptables:chunk_list([1, 2, 3])).

%% chunk_list/2 tests
chunk_list_empty_test() ->
    ?assertEqual([], dog_iptables:chunk_list([], 3)).

chunk_list_exact_division_test() ->
    ?assertEqual([[1, 2, 3], [4, 5, 6]], dog_iptables:chunk_list([1, 2, 3, 4, 5, 6], 3)).

chunk_list_remainder_test() ->
    ?assertEqual([[1, 2], [3]], dog_iptables:chunk_list([1, 2, 3], 2)).

chunk_list_larger_than_list_test() ->
    ?assertEqual([[1, 2]], dog_iptables:chunk_list([1, 2], 5)).

chunk_list_size_one_test() ->
    ?assertEqual([[1], [2], [3]], dog_iptables:chunk_list([1, 2, 3], 1)).

chunk_list_single_element_test() ->
    ?assertEqual([[1]], dog_iptables:chunk_list([1], 1)).
