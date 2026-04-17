-module(dog_external_tests).
-include_lib("eunit/include/eunit.hrl").

partition_by_address_handling_test() ->
    Externals = [
        #{<<"name">> => <<"e1">>, <<"address_handling">> => <<"union">>},
        #{<<"name">> => <<"e2">>, <<"address_handling">> => <<"prefix">>},
        #{<<"name">> => <<"e3">>, <<"address_handling">> => <<"union">>}
    ],
    {Union, Prefix} = dog_external:partition_by_address_handling(Externals),
    
    ?assertEqual(2, length(Union)),
    ?assertEqual(1, length(Prefix)),
    
    [U1, U2] = Union,
    ?assertEqual(<<"e1">>, maps:get(<<"name">>, U1)),
    ?assertEqual(<<"e3">>, maps:get(<<"name">>, U2)),
    
    [P1] = Prefix,
    ?assertEqual(<<"e2">>, maps:get(<<"name">>, P1)).

grouped_by_ipset_name_merge_test() ->
    Env1 = #{
        <<"name">> => <<"e1">>,
        <<"state">> => <<"active">>,
        <<"v4">> => #{
            <<"zones">> => #{
                <<"link_test">> => [<<"1.1.1.1">>]
            }
        }
    },
    Env2 = #{
        <<"name">> => <<"e2">>,
        <<"state">> => <<"active">>,
        <<"v4">> => #{
            <<"zones">> => #{
                <<"link_test">> => [<<"2.2.2.2">>]
            }
        }
    },
    Envs = [Env1, Env2],
    %% Using do_nothing simulate "union" address handling where names are not prefixed
    Result = dog_external:grouped_by_ipset_name(Envs, <<"zones">>, <<"v4">>, fun dog_external:do_nothing/2),
    
    ?assert(maps:is_key(<<"link_test">>, Result)),
    Addresses = maps:get(<<"link_test">>, Result),
    ?assert(lists:member(<<"1.1.1.1">>, Addresses)),
    ?assert(lists:member(<<"2.2.2.2">>, Addresses)).
