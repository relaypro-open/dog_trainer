-module(dog_ruleset_event).

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
    imetrics:add_m(event, ruleset_update),
    PossibleProfileId = maps:get(<<"profile_id">>, Val, []),
    case PossibleProfileId of
        [] ->
            pass;
        ProfileId ->
            %TODO Fix race condition
            timer:sleep(1000),
            GroupIds = dog_group:get_ids_with_profile_id(ProfileId),
            ?LOG_INFO(#{groupids => GroupIds}, #{domain => [dog_trainer]}),
            lists:foreach(
                fun(GroupId) ->
                    {ok, GroupName} = dog_group:get_name_by_id(GroupId),
                    ?LOG_INFO(GroupName, #{domain => [dog_trainer]}),
                    GroupType = <<"group">>,
                    dog_iptables:update_group_iptables(GroupName, GroupType),
                    dog_iptables:update_group_ec2_sgs(GroupName)
                end,
                GroupIds
            )
    end,
    ok.
