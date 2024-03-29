-module(dog_ruleset).

-include_lib("kernel/include/logger.hrl").

-define(VALIDATION_TYPE, <<"ruleset">>).
-define(TYPE_TABLE, ruleset).

%API
-export([
    create/1,
    delete/1,
    get_by_id/1,
    get_by_name/1,
    get_by_profile_id/1,
    get_id_by_profile_id/1,
    get_all/0,
    get_all_grouped_by_id/0,
    get_schema/0,
    update/2
]).

-export([
    all/0,
    all_active/0,
    init/0,
    to_hcl/1,
    to_hcl_by_id/1,
    to_text/1,
    where_used/1
]).

-spec init() -> any().
init() ->
    pass.

-spec create(Group :: map()) -> {ok | error, Key :: iolist() | name_exists}.
create(RulesMap) ->
    RulesMap@0 = RulesMap,
    ?LOG_DEBUG(#{rulesmap@0 => RulesMap@0}),
    Name = maps:get(<<"name">>, RulesMap@0),
    {ok, ExistingRules} = get_all(),
    ExistingNames = [maps:get(<<"name">>, Rules) || Rules <- ExistingRules],
    case lists:member(Name, ExistingNames) of
        false ->
            case dog_json_schema:validate(?VALIDATION_TYPE, RulesMap@0) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:insert(X, RulesMap@0)
                        end
                    ),
                    Key = hd(maps:get(<<"generated_keys">>, R)),
                    ?LOG_DEBUG("create R: ~p~n", [R]),
                    {ok, Key};
                {error, Error} ->
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
    Rules =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    RulesReplaced = lists:map(
        fun(Rule) ->
            Rule
        end,
        Rules
    ),
    {ok, RulesReplaced}.

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
            ?LOG_ERROR("error, rules name not found: ~p", [Name]),
            {error, notfound};
        _ ->
            Rules = hd(Result),
            {ok, Rules}
    end.

-spec all_active() -> {ok, Rules :: list()}.
all_active() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:has_fields(X, [<<"profile_id">>])
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Rules =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Rules}.

-spec all() -> {ok, Rules :: list()}.
all() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get_field(X, <<"id">>)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Rules =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Rules}.

-spec get_by_id(Id :: binary()) -> {ok, map()} | {error, notfound}.
get_by_id(Id) ->
    ?LOG_DEBUG(#{id => Id}),
    R = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, Id)
        end
    ),
    case R of
        {ok, null} ->
            ?LOG_DEBUG("rules id null return value: ~p", [Id]),
            {error, notfound};
        {ok, Rules} ->
            {ok, Rules}
    end.

-spec update(Id :: binary(), UpdateMap :: map()) ->
    {false, atom()} | {validation_error, iolist()} | {true, binary()}.
update(Id, UpdateMap) ->
    ?LOG_DEBUG(#{id => Id, updatemap => UpdateMap}),
    ?LOG_INFO("update_in_place"),
    case get_by_id(Id) of
        {ok, OldRules} ->
            NewRules = maps:merge(OldRules, UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewRules) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:get(X, Id),
                            reql:update(X, UpdateMap)
                        end
                    ),
                    ?LOG_DEBUG("update R: ~p~n", [R]),
                    Replaced = maps:get(<<"replaced">>, R),
                    Unchanged = maps:get(<<"unchanged">>, R),
                    case {Replaced, Unchanged} of
                        {1, 0} -> {true, Id};
                        {0, 1} -> {false, Id};
                        _ -> {false, no_updated}
                    end;
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {error, Error} ->
            {false, Error}
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
    ?LOG_DEBUG("delete R: ~p~n", [R]),
    Deleted = maps:get(<<"deleted">>, R),
    case Deleted of
        1 -> ok;
        _ -> {error, #{<<"error">> => <<"error">>}}
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
where_used(RulesetId) ->
    ?LOG_DEBUG(#{rulesetid => RulesetId}),
    {ok, Ruleset} = get_by_id(RulesetId),
    ProfileId = maps:get(<<"profile_id">>, Ruleset),
    {ok, ProfileId}.

-spec get_all_grouped_by_id() -> map().
get_all_grouped_by_id() ->
    {ok, All} = dog_ruleset_api_v2:get_all(),
    maps:from_list([{maps:get(<<"id">>, Ruleset), Ruleset} || Ruleset <- All]).

-spec get_by_profile_id(ProfileId :: iolist()) -> {ok, Ruleset :: map()} | {error, notfound}.
get_by_profile_id(ProfileId) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get_all(X, ProfileId, #{index => <<"profile_id">>})
        end
    ),
    {ok, R3} = rethink_cursor:all(R),
    Result = lists:flatten(R3),
    case Result of
        [] ->
            ?LOG_ERROR("error, no ruleset associated with profile: ~p", [ProfileId]),
            {error, notfound};
        _ ->
            Rules = hd(Result),
            {ok, Rules}
    end.

-spec get_id_by_profile_id(ProfileId :: iolist()) -> RulesetId :: iolist() | {error, notfound}.
get_id_by_profile_id(ProfileId) ->
    case get_by_profile_id(ProfileId) of
        {ok, Profile} ->
            maps:get(<<"id">>, Profile);
        {error, notfound} ->
            {error, notfound}
    end.

-spec get_schema() -> binary().
get_schema() ->
    dog_json_schema:get_file(?VALIDATION_TYPE).

-spec to_hcl_by_id(RulesetId :: iolist()) -> iolist().
to_hcl_by_id(RulesetId) ->
    dog_ruleset_api_v2:to_hcl_by_id(RulesetId).

-spec to_hcl(Ruleset :: map()) -> binary().
to_hcl(Ruleset) ->
    dog_ruleset_api_v2:to_hcl(Ruleset).
