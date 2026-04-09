-module(dog_host_event).

-include("dog_trainer.hrl").

-export([
    on_create/1,
    on_update/2,
    on_delete/1
]).

-spec on_create(NewVal :: map()) -> ok.
on_create(NewVal) ->
    handle_config_change(null, NewVal),
    handle_active_change(null, NewVal),
    handle_interfaces_change(null, NewVal),
    ok.

-spec on_update(OldVal :: map(), NewVal :: map()) -> ok.
on_update(OldVal, NewVal) ->
    handle_config_change(OldVal, NewVal),
    handle_active_change(OldVal, NewVal),
    handle_interfaces_change(OldVal, NewVal),
    ok.

-spec on_delete(OldVal :: map()) -> ok.
on_delete(OldVal) ->
    handle_config_change(OldVal, null),
    handle_active_change(OldVal, null),
    ok.

%% Config change: environment, group, hostkey, location, name
handle_config_change(OldVal, NewVal) ->
    Hostkeys =
        case NewVal of
            null ->
                [];
            _ ->
                case maps:get(<<"hostkey">>, NewVal, []) of
                    [] -> [];
                    Hostkey -> [Hostkey]
                end
        end,
    OldGroupNames =
        case OldVal of
            null ->
                [];
            _ ->
                [maps:get(<<"group">>, OldVal, [])]
        end,
    NewGroupNames =
        case NewVal of
            null ->
                [];
            _ ->
                [maps:get(<<"group">>, NewVal, [])]
        end,
    GroupNames = lists:flatten(OldGroupNames ++ NewGroupNames),
    case Hostkeys of
        [] ->
            pass;
        _ ->
            lists:foreach(
                fun(Hostkey) ->
                    dog_config:publish_host_config(Hostkey)
                end,
                Hostkeys
            )
    end,
    case GroupNames of
        [] ->
            pass;
        _ ->
            imetrics:add_m(event, host_config_update),
            ?LOG_INFO(#{groupnames => GroupNames}, #{domain => [dog_trainer]}),
            dog_profile_update_agent:add_to_queue(GroupNames)
    end.

%% Active change: name, active, hostkey
handle_active_change(_OldVal, NewVal) ->
    Hostkeys =
        case NewVal of
            null ->
                [];
            _ ->
                case maps:get(<<"hostkey">>, NewVal, []) of
                    [] -> [];
                    Hostkey -> [Hostkey]
                end
        end,
    case Hostkeys of
        [] ->
            pass;
        _ ->
            GroupNames = lists:map(
                fun(Hostkey) ->
                    case dog_host:get_by_hostkey(Hostkey) of
                        {ok, Host} ->
                            maps:get(<<"group">>, Host);
                        {error, notfound} ->
                            ?LOG_ERROR(#{hostkey => Hostkey}, #{domain => [dog_trainer]}),
                            []
                    end
                end,
                Hostkeys
            ),
            ?LOG_INFO(#{groupnames => GroupNames}, #{domain => [dog_trainer]}),
            case GroupNames of
                [<<>>] ->
                    pass;
                [[]] ->
                    pass;
                _ ->
                    imetrics:add_m(event, host_active_update),
                    dog_profile_update_agent:add_to_queue(GroupNames)
            end
    end.

%% Interfaces change
handle_interfaces_change(_OldVal, NewVal) ->
    case NewVal of
        null ->
            pass;
        _ ->
            OldInterfaces =
                case _OldVal of
                    null -> undefined;
                    _ -> maps:get(<<"interfaces">>, _OldVal, undefined)
                end,
            NewInterfaces = maps:get(<<"interfaces">>, NewVal, undefined),
            case OldInterfaces =/= NewInterfaces of
                true ->
                    imetrics:add_m(event, host_interfaces_update),
                    ?LOG_INFO(#{message => "dog_ipset_update_agent:queue_add()"}, #{
                        domain => [dog_trainer]
                    }),
                    dog_ipset_update_agent:queue_add(<<"dog_host_interface_event">>);
                false ->
                    pass
            end
    end.
