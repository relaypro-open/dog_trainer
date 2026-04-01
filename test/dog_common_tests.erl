-module(dog_common_tests).
-include_lib("eunit/include/eunit.hrl").

%% to_list/1 tests
to_list_atom_test() ->
    ?assertEqual("hello", dog_common:to_list(hello)).

to_list_list_test() ->
    ?assertEqual([1, 2, 3], dog_common:to_list([1, 2, 3])).

to_list_tuple_test() ->
    ?assertEqual([a, b], dog_common:to_list({a, b})).

to_list_map_test() ->
    Result = dog_common:to_list(#{a => 1}),
    ?assertEqual([{a, 1}], Result).

to_list_binary_test() ->
    ?assertEqual("hello", dog_common:to_list(<<"hello">>)).

to_list_integer_test() ->
    ?assertEqual("42", dog_common:to_list(42)).

to_list_float_test() ->
    Result = dog_common:to_list(3.14),
    ?assert(is_list(Result)).

%% to_binary/1 tests
to_binary_atom_test() ->
    ?assertEqual(<<"hello">>, dog_common:to_binary(hello)).

to_binary_list_test() ->
    ?assertEqual(<<"hello">>, dog_common:to_binary("hello")).

to_binary_binary_test() ->
    ?assertEqual(<<"hello">>, dog_common:to_binary(<<"hello">>)).

to_binary_integer_test() ->
    ?assertEqual(<<"42">>, dog_common:to_binary(42)).

to_binary_float_test() ->
    Result = dog_common:to_binary(3.14),
    ?assert(is_binary(Result)).

%% re_filter/2 tests
re_filter_match_test() ->
    ?assertEqual(["abc", "abd"], dog_common:re_filter(["abc", "abd", "xyz"], "ab")).

re_filter_no_match_test() ->
    ?assertEqual([], dog_common:re_filter(["abc", "def"], "zzz")).

re_filter_empty_list_test() ->
    ?assertEqual([], dog_common:re_filter([], "ab")).

%% merge_maps_of_lists/1 tests
merge_maps_of_lists_single_test() ->
    Input = [#{a => [1, 2]}],
    Result = dog_common:merge_maps_of_lists(Input),
    ?assertEqual([1, 2], lists:sort(maps:get(a, Result))).

merge_maps_of_lists_overlapping_test() ->
    Input = [#{a => [1, 2]}, #{a => [2, 3]}],
    Result = dog_common:merge_maps_of_lists(Input),
    ?assertEqual([1, 2, 3], lists:sort(maps:get(a, Result))).

merge_maps_of_lists_empty_test() ->
    ?assertEqual(#{}, dog_common:merge_maps_of_lists([])).

%% list_of_maps_to_map/2 tests
list_of_maps_to_map_test() ->
    Input = [#{<<"id">> => <<"a">>, <<"val">> => 1}, #{<<"id">> => <<"b">>, <<"val">> => 2}],
    Result = dog_common:list_of_maps_to_map(Input, <<"id">>),
    ?assertEqual(#{<<"val">> => 1}, maps:get(<<"a">>, Result)),
    ?assertEqual(#{<<"val">> => 2}, maps:get(<<"b">>, Result)).

list_of_maps_to_map_empty_test() ->
    ?assertEqual(#{}, dog_common:list_of_maps_to_map([], <<"id">>)).

lmm_alias_test() ->
    Input = [#{<<"id">> => <<"x">>, <<"v">> => 1}],
    ?assertEqual(dog_common:list_of_maps_to_map(Input, <<"id">>), dog_common:lmm(Input, <<"id">>)).

%% rekey_map_of_maps/3 and rkmm/3 tests
rkmm_test() ->
    MM = #{drew => #{test => rest, a => b}, bob => #{test => zest, a => c}},
    Result = dog_common:rkmm(MM, a, name),
    ?assertEqual(#{name => drew, test => rest}, maps:get(b, Result)),
    ?assertEqual(#{name => bob, test => zest}, maps:get(c, Result)).

rekey_map_of_maps_alias_test() ->
    MM = #{x => #{k => v1, rk => nk}},
    ?assertEqual(dog_common:rkmm(MM, rk, old), dog_common:rekey_map_of_maps(MM, rk, old)).

%% merge_lists_in_tuples/1 tests
merge_lists_in_tuples_test() ->
    Input = [{a, [1, 2]}, {a, [3]}, {b, [4]}],
    Result = lists:sort(dog_common:merge_lists_in_tuples(Input)),
    ?assertEqual([{a, [1, 2, 3]}, {b, [4]}], Result).

merge_lists_in_tuples_unique_test() ->
    Input = [{a, [1]}, {b, [2]}],
    Result = lists:sort(dog_common:merge_lists_in_tuples(Input)),
    ?assertEqual([{a, [1]}, {b, [2]}], Result).

%% inverse_map_of_lists/1 tests
inverse_map_of_lists_test() ->
    Input = #{a => [1, 2], b => [3]},
    Result = lists:sort(dog_common:inverse_map_of_lists(Input)),
    ?assertEqual([{1, a}, {2, a}, {3, b}], Result).

%% tuple_pairs_to_map_of_lists/1 tests
tuple_pairs_to_map_of_lists_test() ->
    Input = [{a, 1}, {a, 2}, {b, 3}],
    Result = dog_common:tuple_pairs_to_map_of_lists(Input),
    ?assertEqual([1, 2], maps:get(a, Result)),
    ?assertEqual([3], maps:get(b, Result)).

tuple_pairs_to_map_of_lists_empty_test() ->
    ?assertEqual(#{}, dog_common:tuple_pairs_to_map_of_lists([])).

%% create_hash/1 tests
create_hash_deterministic_test() ->
    Hash1 = dog_common:create_hash(<<"test">>),
    Hash2 = dog_common:create_hash(<<"test">>),
    ?assertEqual(Hash1, Hash2).

create_hash_different_inputs_test() ->
    Hash1 = dog_common:create_hash(<<"foo">>),
    Hash2 = dog_common:create_hash(<<"bar">>),
    ?assertNotEqual(Hash1, Hash2).

%% to_terraform_name/1 tests
to_terraform_name_dots_test() ->
    ?assertEqual("a_b_c", lists:flatten(dog_common:to_terraform_name("a.b.c"))).

to_terraform_name_special_chars_test() ->
    Result = lists:flatten(dog_common:to_terraform_name("a(b)/c d:e")),
    ?assertEqual("a_b__c_d_e", Result).

to_terraform_name_clean_test() ->
    ?assertEqual("abc", lists:flatten(dog_common:to_terraform_name("abc"))).

%% format_value/1 tests
format_value_binary_test() ->
    Result = lists:flatten(dog_common:format_value(<<"hello">>)),
    ?assertEqual("\"hello\"", Result).

format_value_list_test() ->
    Result = dog_common:format_value([<<"a">>, <<"b">>]),
    ?assert(is_list(Result)).

%% format_var/1 tests
format_var_test() ->
    {K, _V} = dog_common:format_var({<<"key">>, <<"val">>}),
    ?assertEqual(<<"key">>, K).

%% format_vars/1 tests
format_vars_empty_test() ->
    ?assertEqual(#{}, dog_common:format_vars([])).

format_vars_map_test() ->
    Result = dog_common:format_vars(#{<<"k">> => <<"v">>}),
    ?assert(is_map(Result)),
    ?assert(maps:is_key(<<"k">>, Result)).

%% quoted_comma_delimited/1 tests
quoted_comma_delimited_test() ->
    Result = lists:flatten(dog_common:quoted_comma_delimited(["a", "b", "c"])),
    ?assertEqual("\"a\",\"b\",\"c\"", Result).

quoted_comma_delimited_single_test() ->
    Result = lists:flatten(dog_common:quoted_comma_delimited(["a"])),
    ?assertEqual("\"a\"", Result).

%% concat/2 tests
concat_string_test() ->
    ?assertEqual("helloworld", dog_common:concat(["hello", "world"], string)).

concat_binary_test() ->
    ?assertEqual(<<"helloworld">>, dog_common:concat(["hello", "world"], binary)).

concat_mixed_test() ->
    ?assertEqual(<<"helloworld">>, dog_common:concat([<<"hello">>, "world"], binary)).
