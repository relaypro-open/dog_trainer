-module(dog_zone_event).

-include("dog_trainer.hrl").

-export([
    on_create/1,
    on_update/2,
    on_delete/1
]).

-spec on_create(NewVal :: map()) -> ok.
on_create(NewVal) ->
    handle_change(NewVal).

-spec on_update(OldVal :: map(), NewVal :: map()) -> ok.
on_update(_OldVal, NewVal) ->
    handle_change(NewVal).

-spec on_delete(OldVal :: map()) -> ok.
on_delete(OldVal) ->
    handle_change(OldVal).

handle_change(Val) ->
    ZoneName = maps:get(<<"name">>, Val),
    imetrics:add_m(event, zone_update),
    ?LOG_INFO(#{message => "dog_ipset_update_agent:queue_add()"}, #{domain => [dog_trainer]}),
    dog_ipset_update_agent:queue_add(
        dog_common:concat([<<"zone->">>, ZoneName], binary)
    ),
    GroupType = <<"zone">>,
    dog_iptables:update_group_iptables(ZoneName, GroupType),
    ok.
