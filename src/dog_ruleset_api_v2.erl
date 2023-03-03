-module(dog_ruleset_api_v2).

-include_lib("kernel/include/logger.hrl").

-define(VALIDATION_TYPE, <<"ruleset">>).
-define(TYPE_TABLE, ruleset).

%API
-export([
    create/1,
    delete/1,
    get_all/0,
    get_by_id/1,
    get_by_name/1,
    get_schema/0,
    update/2
]).

-export([
    all/0,
    all_active/0,
    init/0,
    to_text/1,
    where_used/1
]).

-spec init() -> any().
init() ->
    pass.

-spec create(Rule :: map()) -> {ok | error, Key :: iolist() | name_exists}.
create(RuleMap@0) ->
    Name = maps:get(<<"name">>, RuleMap@0),
    {ok, ExistingRules} = get_all(),
    ExistingNames = [maps:get(<<"name">>, Rule) || Rule <- ExistingRules],
    case lists:member(Name, ExistingNames) of
        false ->
            case dog_json_schema:validate(?VALIDATION_TYPE, RuleMap@0) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:insert(X, RuleMap@0, #{return_changes => always})
                        end
                    ),
                    NewVal = maps:get(<<"new_val">>, hd(maps:get(<<"changes">>, R))),
                    {ok, NewVal};
                {error, Error} ->
                    ?LOG_ERROR("~p", [Error]),
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        true ->
            {error, name_exists}
    end.

-spec get_all() -> {'ok', list()}.
get_all() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Ruless =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Ruless}.

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
            ?LOG_ERROR("error, ruleset name not found: ~p", [Name]),
            {error, notfound};
        _ ->
            Rules = hd(Result),
            {ok, Rules}
    end.

-spec all_active() -> {ok, Ruless :: list()}.
all_active() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, group),
            reql:get_field(X, <<"ruleset_id">>)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Ruless =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Ruless}.

-spec all() -> {ok, Ruless :: list()}.
all() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get_field(X, <<"id">>)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Ruless =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Ruless}.

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
            ?LOG_DEBUG("ruleset id null return value: ~p", [Id]),
            {error, notfound};
        {ok, Rules} ->
            {ok, Rules}
    end.

-spec update(RuleId :: binary(), UpdateMap :: map()) -> {atom(), any()}.
update(Id, UpdateMap@0) ->
    ?LOG_DEBUG("UpdateMap: ~p~n", [UpdateMap@0]),
    case get_by_id(Id) of
        {ok, OldService} ->
            NewService = maps:merge(OldService, UpdateMap@0),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewService) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:get(X, Id),
                            reql:update(X, UpdateMap@0, #{return_changes => always})
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

-spec delete(RuleId :: binary()) -> (ok | {error, Error :: map()}).
delete(Id) ->
    case where_used(Id) of
        {ok, []} ->
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
            end;
        {ok, Profiles} ->
            ?LOG_INFO("ruleset ~p not deleted, associated with profile: ~p~n", [Id, Profiles]),
            {error, #{<<"errors">> => #{<<"associated with profile">> => Profiles}}}
    end.

-spec rule_to_text(Rule :: map(), Keys :: list()) -> iolist().
rule_to_text(Rule, Keys) ->
    Values = lists:map(
        fun(L) ->
            Value = maps:get(L, Rule),
            ?LOG_DEBUG("Key: ~p Value: ~p~n", [L, Value]),
            case L of
                <<"group">> ->
                    case Value of
                        <<" ">> ->
                            Value;
                        <<"any">> ->
                            Value;
                        <<"ANY">> ->
                            Value;
                        _ ->
                            case dog_group:get_name_by_id(Value) of
                                {ok, Group} ->
                                    Group;
                                {error, _} ->
                                    case dog_zone:get_name_by_id(Value) of
                                        {ok, Zone} ->
                                            Zone;
                                        {error, _} ->
                                            throw(group_not_found)
                                    end
                            end
                    end;
                <<"service">> ->
                    case Value of
                        <<" ">> ->
                            Value;
                        _ ->
                            dog_service:get_name_by_id(Value)
                    end;
                _ ->
                    Value
            end
        end,
        Keys
    ),

    ValuesStrings = [dog_common:to_list(X) || X <- Values],
    io_lib:format("~s", [string:join(ValuesStrings, "\t")]).

-spec to_text(Rules :: map()) -> {'ok', iolist()}.
to_text(Rules) ->
    Keys = [
        <<"group">>,
        <<"service">>,
        <<"states">>,
        <<"action">>,
        <<"active">>
    ],
    Header = lists:map(fun(L) -> dog_common:to_list(L) end, Keys),
    Rules1 = maps:get(<<"rules">>, Rules),
    Inbound = maps:get(<<"inbound">>, Rules1),
    Outbound = maps:get(<<"outbound">>, Rules1),
    InboundList = lists:map(fun(Rule) -> rule_to_text(Rule, Keys) end, Inbound),
    OutboundList = lists:map(fun(Rule) -> rule_to_text(Rule, Keys) end, Outbound),
    Text = io_lib:format("Inbound:~n~s~n~s~n~nOutbound:~n~s~n~s~n", [
        string:join(Header, "\t"),
        string:join(InboundList, "\n"),
        string:join(Header, "\t"),
        string:join(OutboundList, "\n")
    ]),
    {ok, Text}.

-spec where_used(RulesId :: binary()) -> {ok, list()}.
where_used(RulesId) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, profile),
            reql:has_fields(X, [<<"ruleset_id">>]),
            reql:filter(X, fun(Y) ->
                reql:bracket(Y, <<"ruleset_id">>),
                reql:eq(Y, RulesId)
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

-spec get_schema() -> binary().
get_schema() ->
    dog_json_schema:get_file(?VALIDATION_TYPE).
