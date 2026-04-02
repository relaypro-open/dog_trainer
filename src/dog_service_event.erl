-module(dog_service_event).

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
    ServiceId = maps:get(<<"id">>, Val),
    imetrics:add_m(event, service_update),
    ?LOG_INFO(#{serviceid => ServiceId}, #{domain => [dog_trainer]}),
    {ok, ProfilesWithService} = dog_service:where_used(ServiceId),
    ?LOG_INFO(#{profileswithservice => ProfilesWithService}, #{domain => [dog_trainer]}),
    GroupIdsWithProfile = lists:flatten(
        lists:map(
            fun(ProfileId) ->
                dog_profile:where_used(ProfileId)
            end,
            ProfilesWithService
        )
    ),
    ?LOG_INFO(#{groupidswithprofile => GroupIdsWithProfile}, #{domain => [dog_trainer]}),
    GroupIdsWithProfile2 = lists:flatten([
        element(2, GroupId)
     || GroupId <- GroupIdsWithProfile
    ]),
    GroupNamesWithProfile = [
        element(2, dog_group:get_name_by_id(GroupId))
     || GroupId <- GroupIdsWithProfile2
    ],
    ?LOG_INFO(#{groupnameswithprofile => GroupNamesWithProfile}, #{domain => [dog_trainer]}),
    dog_profile_update_agent:add_to_queue(GroupNamesWithProfile),
    ok.
