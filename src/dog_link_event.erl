-module(dog_link_event).

-include("dog_trainer.hrl").

-export([
    on_create/1,
    on_update/2,
    on_delete/1
]).

-spec on_create(NewVal :: map()) -> ok.
on_create(NewVal) ->
    StateMap = #{
        env_name => maps:get(<<"name">>, NewVal),
        new_enabled_state => maps:get(<<"enabled">>, NewVal),
        new_direction_state => maps:get(<<"direction">>, NewVal),
        old_enabled_state => false,
        old_direction_state => <<"inbound">>
    },
    apply_side_effects(StateMap).

-spec on_update(OldVal :: map(), NewVal :: map()) -> ok.
on_update(OldVal, NewVal) ->
    StateMap = #{
        env_name => maps:get(<<"name">>, NewVal),
        new_enabled_state => maps:get(<<"enabled">>, NewVal),
        new_direction_state => maps:get(<<"direction">>, NewVal),
        old_enabled_state => maps:get(<<"enabled">>, OldVal),
        old_direction_state => maps:get(<<"direction">>, OldVal)
    },
    apply_side_effects(StateMap).

-spec on_delete(OldVal :: map()) -> ok.
on_delete(OldVal) ->
    StateMap = #{
        env_name => maps:get(<<"name">>, OldVal),
        new_enabled_state => false,
        new_direction_state => <<"bidirectional">>,
        old_enabled_state => maps:get(<<"enabled">>, OldVal),
        old_direction_state => maps:get(<<"direction">>, OldVal)
    },
    apply_side_effects(StateMap).

apply_side_effects(StateMap) ->
    EnvName = maps:get(env_name, StateMap),
    ?LOG_DEBUG(#{stateMap => StateMap}, #{domain => [dog_trainer]}),
    dog_external_agent:set_link_state(StateMap),
    ?LOG_INFO(#{message => "dog_ipset_update_agent:queue_add()"}, #{domain => [dog_trainer]}),
    dog_ipset_update_agent:queue_add(
        dog_common:concat([<<"link->">>, EnvName], binary)
    ),
    imetrics:add_m(event, link_update),
    ok.
