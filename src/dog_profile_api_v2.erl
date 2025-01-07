-module(dog_profile_api_v2).

-include_lib("kernel/include/logger.hrl").
-include("dog_trainer.hrl").

-define(VALIDATION_TYPE, <<"profile">>).
-define(TYPE_TABLE, profile).

%API
-export([
    create/1,
    delete/1,
    get_all/0,
    get_all_active/0,
    get_all_any/0,
    get_by_id/1,
    get_by_name/1,
    update/2,
    update/3,
    to_hcl/1,
    to_hcl_by_id/1,
    where_used/1
]).

-export([]).

-spec create(Profile :: map()) -> {'ok', iolist()} | {atom(), binary()}.
create(Profile) ->
    ?LOGT_DEBUG("Profile: ~p~n", [{profile,Profile}]),
    %Timestamp = dog_time:timestamp(),
    case dog_json_schema:validate(?VALIDATION_TYPE, Profile) of
        ok ->
            %Profile@1 = maps:put(<<"created">>, Timestamp, Profile),
            {ok, R} = dog_rethink:run(
                fun(X) ->
                    reql:db(X, dog),
                    reql:table(X, ?TYPE_TABLE),
                    reql:insert(X, Profile, #{return_changes => always})
                end
            ),
            NewVal = maps:get(<<"new_val">>, hd(maps:get(<<"changes">>, R))),
            {ok, NewVal};
        {error, Error} ->
            ?LOGT_ERROR("~p", [{error,Error}]),
            Response = dog_parse:validation_error(Error),
            {validation_error, Response}
    end.

-spec delete(GroupId :: binary()) -> (ok | {error, Error :: map()}).
delete(Id) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, Id),
            reql:delete(X)
        end
    ),
    ?LOGT_DEBUG("delete R: ~p~n", [{r,R}]),
    Deleted = maps:get(<<"deleted">>, R),
    case Deleted of
        1 ->
            ok;
        _ ->
            {error, #{<<"error">> => <<"error">>}}
    end.

-spec where_used(ProfileId :: binary()) -> {ok, list()}.
where_used(ProfileId) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, group),
            reql:has_fields(X, [<<"profile_id">>]),
            reql:filter(X, fun(Y) ->
                reql:bracket(Y, <<"profile_id">>),
                reql:eq(Y, ProfileId)
            end),
            reql:get_field(X, <<"id">>)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Groups =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Groups}.

-spec get_all() -> {'ok', list()}.
get_all() ->
    get_all_any().

-spec get_all_any() -> {'ok', list()}.
get_all_any() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Profiles =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Profiles}.

-spec get_all_active() -> {'ok', list()}.
get_all_active() ->
    {ok, ActiveIds} = dog_profile:all_active(),
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Profiles =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    ActiveProfiles = lists:filter(
        fun(Profile) -> lists:member(maps:get(<<"id">>, Profile), ActiveIds) end, Profiles
    ),
    {ok, ActiveProfiles}.

-spec get_by_id(Id :: binary()) -> {ok, map()} | {error, notfound}.
get_by_id(Id) ->
    R = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, Id)
        end
    ),
    case R of
        {ok, null} ->
            ?LOGT_DEBUG("profile id null return value: ~p", [{id,Id}]),
            {error, notfound};
        {ok, Profile} ->
            {ok, Profile}
    end.

-spec get_by_name(Name :: binary()) -> {ok, map()} | {error, atom()}.
get_by_name(Name) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get_all(X, Name, #{index => <<"name">>})
        end
    ),
    {ok, R3} = rethink_cursor:all(R),
    Result = lists:flatten(R3),
    case Result of
        [] ->
            ?LOGT_ERROR("error, profile name not found: ~p", [{name,Name}]),
            {error, notfound};
        _ ->
            Profile = hd(Result),
            {ok, Profile}
    end.

-spec update(Id :: binary(), UpdateMap :: map()) -> {atom(), Id :: iolist()} | {false, atom()}.
update(Id, UpdateMap) ->
    {Result, Profile} = update(Id, UpdateMap, true),
    {Result, Profile}.

-spec update(Id :: binary(), UpdateMap :: map(), InPlace :: boolean()) ->
    {atom(), Id :: iolist()} | {false, atom()}.
update(Id, UpdateMap, _InPlace) ->
    {Result, Profile} = update_in_place(Id, UpdateMap),
    {Result, Profile}.

-spec update_in_place(Id :: binary(), UpdateMap :: map()) ->
    {false, atom()} | {validation_error, iolist()} | {true, binary()}.
update_in_place(Id, UpdateMap) ->
    ?LOG_INFO("update_in_place"),
    case get_by_id(Id) of
        {ok, OldProfile} ->
            NewProfile = maps:merge(OldProfile, UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewProfile) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:get(X, Id),
                            reql:update(X, UpdateMap, #{return_changes => always})
                        end
                    ),
                    ?LOGT_DEBUG("update R: ~p~n", [{r,R}]),
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

-spec to_hcl_by_id(ProfileId :: iolist()) -> iolist().
to_hcl_by_id(ProfileId) ->
    {ok, Profile} = get_by_id(ProfileId),
    to_hcl(Profile). 

-spec to_hcl(Profile :: map()) -> binary().
to_hcl(Profile) ->
    Bindings = #{
                 'TerraformName' => dog_common:to_terraform_name(maps:get(<<"name">>, Profile)), 
                 'Name' => maps:get(<<"name">>, Profile), 
                 'Environment' => <<"qa">>
                },
    {ok, Snapshot} = eel:compile(<<
        "resource \"dog_profile\" \"<%= TerraformName .%>\" {\n"
        "  name = \"<%= Name .%>\"\n"
        "  provider = dog.<%= Environment .%>\n"
        "}\n"
        "\n"
    >>),
    {ok, RenderSnapshot} = eel_renderer:render(Bindings, Snapshot),
    {IoData, _} = {eel_evaluator:eval(RenderSnapshot), RenderSnapshot},
    erlang:iolist_to_binary(IoData).
