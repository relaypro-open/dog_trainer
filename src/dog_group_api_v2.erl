-module(dog_group_api_v2).

-include_lib("kernel/include/logger.hrl").

-define(VALIDATION_TYPE, <<"group">>).
-define(TYPE_TABLE, group).

%API
-export([
    create/1,
    delete/1,
    get_all/0,
    get_by_id/1,
    get_by_name/1,
    replace/2,
    replace_profile_by_profile_id/2,
    replace_profile_by_profile_id/3,
    update/2
]).

get_by_id(Id) ->
    dog_group:get_by_id(Id).

get_by_name(Name) ->
    dog_group:get_by_name(Name).

all_active() ->
    dog_group:all_active().

-spec create(Group :: map()) -> {ok, Key :: iolist()}.
create(Group@0) when is_map(Group@0) ->
    GroupName = maps:get(<<"name">>, Group@0),
    GroupResult = get_by_name(GroupName),
    DefaultMap = #{
        <<"hash4_ipsets">> => <<"">>,
        <<"hash6_ipsets">> => <<"">>,
        <<"hash4_iptables">> => <<"">>,
        <<"hash6_iptables">> => <<"">>,
        <<"ipset_hash">> => <<"">>,
        <<"external_ipv4_addresses">> => [],
        <<"external_ipv6_addresses">> => [],
        <<"profile_version">> => <<"latest">>
    },
    case GroupResult of
        {error, notfound} ->
            Timestamp = dog_time:timestamp(),
            Group@1 = maps:put(<<"created">>, Timestamp, Group@0),
            %            ProfileIdValidated =
            %                case maps:get(<<"profile_id">>, Group@0, <<"">>) of
            %                    <<"">> ->
            %                        <<"">>;
            %                    ProfileId ->
            %                        case dog_profile:get_by_id(ProfileId) of
            %                            {error, notfound} ->
            %                                <<"">>;
            %                            {ok, Profile} ->
            %                                case maps:find(<<"id">>, Profile) of
            %                                    error ->
            %                                        <<"">>;
            %                                    {ok, Id} ->
            %                                        Id
            %                                end
            %                        end
            %                end,
            %            Group@2 = maps:put(<<"profile_id">>, ProfileIdValidated, Group@1),
            NewMap = maps:merge(DefaultMap, Group@1),
            ?LOG_DEBUG("NewMap: ~p", [NewMap]),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewMap) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:insert(X, NewMap, #{return_changes => always})
                        end
                    ),
                    NewVal = maps:get(<<"new_val">>, hd(maps:get(<<"changes">>, R))),
                    {ok, NewVal};
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {ok, _} ->
            {error, exists}
    end.

-spec delete(GroupId :: binary()) -> (ok | {error, Error :: iolist()}).
delete(Id) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, Id),
            reql:delete(X)
        end
    ),
    ?LOG_DEBUG("delete R: ~p~n", [R]),
    Deleted = maps:get(<<"deleted">>, R),
    case Deleted of
        1 -> ok;
        _ -> {error, #{<<"error">> => <<"error">>}}
    end.

-spec get_all() -> {ok, list()}.
get_all() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Groups =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    Groups@1 = lists:append(Groups, [all_active()]),
    {ok, Groups@1}.

-spec replace(Id :: binary(), ReplaceMap :: map()) ->
    {'false', 'no_replaced' | 'notfound' | binary()}
    | {'true', binary()}
    | {'validation_error', binary()}.
replace(Id, ReplaceMap) ->
    case get_by_id(Id) of
        {ok, OldExternal} ->
            NewItem = maps:merge(OldExternal, ReplaceMap),
            NewItem2 = dog_time:merge_timestamp(NewItem),
            NewItem3 = maps:put(<<"id">>, Id, NewItem2),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewItem3) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:get(X, Id),
                            reql:replace(X, NewItem3, #{return_changes => always})
                        end
                    ),
                    ?LOG_DEBUG("replaced R: ~p~n", [R]),
                    Replaced = maps:get(<<"replaced">>, R),
                    Unchanged = maps:get(<<"unchanged">>, R),
                    case {Replaced, Unchanged} of
                        {1, 0} ->
                            NewVal = maps:get(<<"new_val">>, hd(maps:get(<<"changes">>, R))),
                            {true, NewVal};
                        {0, 1} ->
                            OldVal = maps:get(<<"old_val">>, hd(maps:get(<<"changes">>, R))),
                            {false, OldVal};
                        _ ->
                            {false, no_updated}
                    end;
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {error, Error} ->
            {false, Error}
    end.

-spec replace_profile_by_profile_id(OldId :: binary(), NewId :: binary()) -> list().
replace_profile_by_profile_id(OldId, NewId) ->
    GroupIds = dog_group:get_ids_with_profile_id(OldId),
    Results = lists:map(
        fun(GroupId) ->
            update(GroupId, #{<<"profile_id">> => NewId})
        end,
        GroupIds
    ),
    Results.

-spec replace_profile_by_profile_id(OldId :: binary(), NewId :: binary(), ProfileName :: iolist()) ->
    list().
replace_profile_by_profile_id(OldId, NewId, ProfileName) ->
    ?LOG_DEBUG("OldId: ~p, NewId: ~p, ProfileName: ~p", [OldId, NewId, ProfileName]),
    GroupIds = dog_group:get_ids_with_profile_id(OldId),
    Results = lists:map(
        fun(GroupId) ->
            update(GroupId, #{<<"profile_id">> => NewId, <<"profile_name">> => ProfileName})
        end,
        GroupIds
    ),
    Results.

-spec update(GroupId :: binary(), UpdateMap :: map()) -> {atom(), any()}.
update(Id, UpdateMap) ->
    case get_by_id(Id) of
        {ok, OldGroup} ->
            NewGroup = maps:merge(OldGroup, UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewGroup) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:get(X, Id),
                            reql:replace(X, NewGroup, #{return_changes => always})
                        end
                    ),
                    ?LOG_DEBUG("update R: ~p~n", [R]),
                    Replaced = maps:get(<<"replaced">>, R),
                    Unchanged = maps:get(<<"unchanged">>, R),
                    case {Replaced, Unchanged} of
                        {1, 0} ->
                            NewVal = maps:get(<<"new_val">>, hd(maps:get(<<"changes">>, R))),
                            {true, NewVal};
                        {0, 1} ->
                            OldVal = maps:get(<<"old_val">>, hd(maps:get(<<"changes">>, R))),
                            {false, OldVal};
                        _ ->
                            {false, no_updated}
                    end;
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {error, Error} ->
            {false, Error}
    end.
