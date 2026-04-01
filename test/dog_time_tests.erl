-module(dog_time_tests).
-include_lib("eunit/include/eunit.hrl").

timestamp_returns_integer_test() ->
    Result = dog_time:timestamp(),
    ?assert(is_integer(Result)).

timestamp_reasonable_value_test() ->
    Result = dog_time:timestamp(),
    %% Should be after 2024-01-01 (1704067200) and before 2030-01-01 (1893456000)
    ?assert(Result > 1704067200),
    ?assert(Result < 1893456000).

merge_timestamp_adds_key_test() ->
    Input = #{<<"name">> => <<"test">>},
    Result = dog_time:merge_timestamp(Input),
    ?assert(maps:is_key(<<"timestamp">>, Result)),
    ?assertEqual(<<"test">>, maps:get(<<"name">>, Result)).

merge_timestamp_value_is_integer_test() ->
    Result = dog_time:merge_timestamp(#{}),
    Ts = maps:get(<<"timestamp">>, Result),
    ?assert(is_integer(Ts)).

merge_timestamp_empty_map_test() ->
    Result = dog_time:merge_timestamp(#{}),
    ?assertEqual(1, maps:size(Result)).
