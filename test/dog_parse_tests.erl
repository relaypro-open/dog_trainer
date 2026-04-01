-module(dog_parse_tests).
-include_lib("eunit/include/eunit.hrl").

data_invalid_test() ->
    Error = [{data_invalid, #{}, wrong_type, <<"val">>, []}],
    Result = dog_parse:validation_error(Error),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"data_invalid">>, maps:get(<<"error">>, Decoded)),
    ?assertEqual(<<"wrong_type">>, maps:get(<<"error_type">>, Decoded)).

schema_invalid_test() ->
    Error = [{schema_invalid, #{<<"type">> => <<"object">>}, missing_required}],
    Result = dog_parse:validation_error(Error),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"schema_invalid">>, maps:get(<<"error">>, Decoded)).

database_error_test() ->
    Error = [{database_error, <<"table">>, not_found}],
    Result = dog_parse:validation_error(Error),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"database_error">>, maps:get(<<"error">>, Decoded)).
