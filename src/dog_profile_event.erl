-module(dog_profile_event).

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
    ProfileId = maps:get(<<"id">>, Val),
    imetrics:add_m(event, profile_update),
    ?LOG_INFO(#{profileid => ProfileId}, #{domain => [dog_trainer]}),
    %TODO Fix race condition
    timer:sleep(1000),
    GroupIds = dog_group:get_ids_with_profile_id(ProfileId),
    ?LOG_INFO(#{groupids => GroupIds}, #{domain => [dog_trainer]}),
    lists:foreach(
        fun(GroupId) ->
            {ok, GroupName} = dog_group:get_name_by_id(GroupId),
            ?LOG_INFO(GroupName, #{domain => [dog_trainer]}),
            GroupType = <<"group">>,
            dog_iptables:update_group_iptables(GroupName, GroupType)
        end,
        GroupIds
    ),
    ok.
