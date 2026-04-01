-module(dog_log_filter_tests).
-include_lib("eunit/include/eunit.hrl").

%% dog_trainer_only/2 tests
dog_trainer_only_match_test() ->
    Event = #{level => info, msg => {string, "test"}, meta => #{domain => [dog_trainer]}},
    ?assertEqual(Event, dog_log_filter:dog_trainer_only(Event, [])).

dog_trainer_only_match_nested_test() ->
    Event = #{level => info, msg => {string, "test"}, meta => #{domain => [dog_trainer, sub]}},
    ?assertEqual(Event, dog_log_filter:dog_trainer_only(Event, [])).

dog_trainer_only_other_domain_test() ->
    Event = #{level => info, msg => {string, "test"}, meta => #{domain => [other_app]}},
    ?assertEqual(stop, dog_log_filter:dog_trainer_only(Event, [])).

dog_trainer_only_no_domain_test() ->
    Event = #{level => info, msg => {string, "test"}, meta => #{}},
    ?assertEqual(stop, dog_log_filter:dog_trainer_only(Event, [])).

%% non_dog_trainer/2 tests
non_dog_trainer_match_test() ->
    Event = #{level => info, msg => {string, "test"}, meta => #{domain => [dog_trainer]}},
    ?assertEqual(stop, dog_log_filter:non_dog_trainer(Event, [])).

non_dog_trainer_other_domain_test() ->
    Event = #{level => info, msg => {string, "test"}, meta => #{domain => [other_app]}},
    ?assertEqual(Event, dog_log_filter:non_dog_trainer(Event, [])).

non_dog_trainer_no_domain_test() ->
    Event = #{level => info, msg => {string, "test"}, meta => #{}},
    ?assertEqual(Event, dog_log_filter:non_dog_trainer(Event, [])).
