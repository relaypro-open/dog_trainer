-module(dog_ruleset_api_v2).
-include_lib("kernel/include/logger.hrl").

-define(VALIDATION_TYPE, <<"ruleset">>).
-define(TYPE_TABLE, ruleset).

%API
-export([
    create/1,
    delete/1,
    get_all/0,
    get_all_names/0,
    get_all_active/0,
    get_all_active_names/0,
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
    where_used/1,
    rule_names_to_ids/4,
    ids_to_names/1,
    nti/1,
    names_to_ids/1,
    to_hcl/1,
    to_hcl_by_id/1
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
                    ?LOG_ERROR(#{"error" => Error}),
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
    {ok, Rules}.

-spec get_all_names() -> {'ok', list()}.
get_all_names() ->
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
            ids_to_names(Rule)
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
            ?LOG_ERROR(#{"message" => "error, ruleset name not found", "name" => Name}),
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
            ?LOG_DEBUG(#{"id" => Id, "message" => "ruleset id null return value"}),
            {error, notfound};
        {ok, Rules} ->
            {ok, Rules}
    end.

-spec update(RuleId :: binary(), UpdateMap :: map()) -> {atom(), any()}.
update(Id, UpdateMap@0) ->
    ?LOG_DEBUG(#{"update_map@0" => UpdateMap@0}),
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
                    ?LOG_DEBUG(#{"message" => "update R", "r" => R}),
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
            ?LOG_DEBUG(#{"message" => "delete R", "r" => R}),
            Deleted = maps:get(<<"deleted">>, R),
            case Deleted of
                1 -> ok;
                _ -> {error, #{<<"error">> => <<"error">>}}
            end;
        {ok, Profiles} ->
            ?LOG_INFO(#{"profiles" => Profiles, "id" => Id, "message" => "ruleset not deleted, associated with profile"}),
            {error, #{<<"errors">> => #{<<"associated with profile">> => Profiles}}}
    end.

-spec rule_to_text(Rule :: map(), Keys :: list()) -> iolist().
rule_to_text(Rule, Keys) ->
    Values = lists:map(
        fun(L) ->
            Value = maps:get(L, Rule),
            ?LOG_DEBUG(#{"value" => Value, "key" => L}),
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

-spec ids_to_names(Rulesets :: list()) -> {'ok', Rulesests :: list()}.
ids_to_names(Rulesets) when is_list(Rulesets)->
    RulesReplaced = lists:map(
        fun(Ruleset) ->
            ids_to_names(Ruleset)
        end,
        Rulesets
    ),
    {ok, RulesReplaced};
ids_to_names(Profile) when is_map(Profile) ->
    ?LOG_DEBUG(#{"profile" => Profile}),
    case Profile of
        _ when not is_map(Profile) ->
            Profile;
        _ ->
            Inbound = nested:get([<<"rules">>, <<"inbound">>], Profile, []),
            Outbound = nested:get([<<"rules">>, <<"outbound">>], Profile, []),
            ServicesById = dog_service:get_all_grouped_by_id(),
            ZonesById = dog_zone:get_all_grouped_by_id(),
            GroupsById = dog_group:get_all_grouped_by_id(),
            InboundReplaced =
                case Inbound of
                    [] ->
                        [];
                    _ ->
                        rule_ids_to_names(Inbound, ServicesById, ZonesById, GroupsById)
                end,
            OutboundReplaced =
                case Outbound of
                    [] ->
                        [];
                    _ ->
                        rule_ids_to_names(Outbound, ServicesById, ZonesById, GroupsById)
                end,
            NewRules = #{
                <<"inbound">> => InboundReplaced,
                <<"outbound">> => OutboundReplaced
            },
            maps:update(<<"rules">>, NewRules, Profile)
    end.

rule_ids_to_names(Rules, ServicesById, ZonesById, GroupsById) ->
    lists:map(
        fun(Rule) ->
            ServiceName =
                case maps:get(<<"service">>, Rule) of
                    <<"any">> ->
                        <<"any">>;
                    ServiceId ->
                        Service = maps:get(ServiceId, ServicesById),
                        maps:get(<<"name">>, Service)
                end,
            RuleServiceReplaced = maps:update(<<"service">>, ServiceName, Rule),
            ZonesGroupsReplaced =
                case maps:get(<<"group_type">>, Rule) of
                    <<"ANY">> ->
                        RuleServiceReplaced;
                    <<"ZONE">> ->
                        ZoneName =
                            case maps:get(<<"group">>, Rule) of
                                <<"any">> ->
                                    <<"any">>;
                                ZoneId ->
                                    maps:get(<<"name">>, maps:get(ZoneId, ZonesById))
                            end,
                        maps:update(<<"group">>, ZoneName, RuleServiceReplaced);
                    <<"GROUP">> ->
                        GroupName =
                            case maps:get(<<"group">>, Rule) of
                                <<"any">> ->
                                    <<"any">>;
                                GroupId ->
                                    maps:get(<<"name">>, maps:get(GroupId, GroupsById))
                            end,
                        maps:update(<<"group">>, GroupName, RuleServiceReplaced);
                    <<"ROLE">> ->
                        GroupName =
                            case maps:get(<<"group">>, Rule) of
                                <<"any">> ->
                                    <<"any">>;
                                GroupId ->
                                    maps:get(<<"name">>, maps:get(GroupId, GroupsById))
                            end,
                        maps:update(<<"group">>, GroupName, RuleServiceReplaced)
                end,
            ZonesGroupsReplaced
        end,
        Rules
    ).

nti(ProfileId) ->
    {ok, Profile} = get_by_id(ProfileId),
    %Itn = ids_to_names(Profile),
    names_to_ids(Profile).

-spec names_to_ids(Profile :: map()) -> Profile :: {ok | error, map()}.
names_to_ids(Profile) ->
    Inbound = nested:get([<<"rules">>, <<"inbound">>], Profile, []),
    Outbound = nested:get([<<"rules">>, <<"outbound">>], Profile, []),
    ServicesByName = dog_service:get_all_grouped_by_name(),
    ZonesByName = dog_zone:get_all_grouped_by_name(),
    GroupsByName = dog_group:get_all_grouped_by_name(),
    InboundReplaced =
        case Inbound of
            [] ->
                [];
            _ ->
                rule_names_to_ids(Inbound, ServicesByName, ZonesByName, GroupsByName)
        end,
    OutboundReplaced =
        case Outbound of
            [] ->
                [];
            _ ->
                rule_names_to_ids(Outbound, ServicesByName, ZonesByName, GroupsByName)
        end,
    NewRules = #{
        <<"inbound">> => InboundReplaced,
        <<"outbound">> => OutboundReplaced
    },
    maps:update(<<"rules">>, NewRules, Profile).

rule_names_to_ids(Rules, ServicesByName, ZonesByName, GroupsByName) ->
    lists:map(
        fun(Rule) ->
            ServiceId =
                case maps:get(<<"service">>, Rule) of
                    <<"any">> ->
                        <<"any">>;
                    ServiceName ->
                        maps:get(<<"id">>, maps:get(ServiceName, ServicesByName))
                end,
            RuleServiceReplaced = maps:update(<<"service">>, ServiceId, Rule),
            ZonesGroupsReplaced =
                case maps:get(<<"group_type">>, Rule) of
                    <<"ANY">> ->
                        RuleServiceReplaced;
                    <<"ZONE">> ->
                        ZoneId =
                            case maps:get(<<"group">>, Rule) of
                                <<"any">> ->
                                    <<"any">>;
                                ZoneName ->
                                    maps:get(<<"id">>, maps:get(ZoneName, ZonesByName))
                            end,
                        maps:update(<<"group">>, ZoneId, RuleServiceReplaced);
                    <<"GROUP">> ->
                        GroupId =
                            case maps:get(<<"group">>, Rule) of
                                <<"any">> ->
                                    <<"any">>;
                                GroupName ->
                                    maps:get(<<"id">>, maps:get(GroupName, GroupsByName))
                            end,
                        maps:update(<<"group">>, GroupId, RuleServiceReplaced);
                    <<"ROLE">> ->
                        GroupId =
                            case maps:get(<<"group">>, Rule) of
                                <<"any">> ->
                                    <<"any">>;
                                GroupName ->
                                    maps:get(<<"id">>, maps:get(GroupName, GroupsByName))
                            end,
                        maps:update(<<"group">>, GroupId, RuleServiceReplaced)
                end,
            ZonesGroupsReplaced
        end,
        Rules
    ).

-spec to_hcl_by_id(RulesetId :: iolist()) -> iolist().
to_hcl_by_id(RulesetId) ->
    {ok, RulesetWithIds} = get_by_id(RulesetId),
    Ruleset = ids_to_names(RulesetWithIds),
    to_hcl(Ruleset). 

-spec to_hcl(Ruleset :: map()) -> binary().
to_hcl(Ruleset) ->
    InboundRules = rules_to_hcl(nested:get([<<"rules">>,<<"inbound">>],Ruleset)),
    OutboundRules = rules_to_hcl(nested:get([<<"rules">>,<<"outbound">>],Ruleset)),
    Bindings = #{
                 'TerraformName' => dog_common:to_terraform_name(maps:get(<<"name">>, Ruleset)), 
                 'Name' => maps:get(<<"name">>, Ruleset), 
                 'Environment' => <<"qa">>,
                 'InboundRules' => InboundRules,
                 'OutboundRules' => OutboundRules
                },
    {ok, Snapshot} = eel:compile(<<
        "resource \"dog_ruleset\" \"<%= TerraformName .%>\"\n"
        "  name = \"<%= Name .%>\"\n"
        "  profile_id = dog_profile.<%= Name .%>.id\n"
        "  rules = {\n"
        "    inbound = [\n"
        "<%= InboundRules .%>"
        "    ]\n"
        "    outbound = [\n"
        "<%= OutboundRules .%>"
        "    ]\n"
        "  }\n"
        "  provider = dog.<%= Environment .%>\n"
        "}\n"
    >>),
    {ok, RenderSnapshot} = eel_renderer:render(Bindings, Snapshot),
    {IoData, _} = {eel_evaluator:eval(RenderSnapshot), RenderSnapshot},
    erlang:iolist_to_binary(IoData).

-spec rules_to_hcl(Rules :: map()) -> binary().
rules_to_hcl(Rules) ->
    lists:map(fun(Rule) ->
                      Group = case maps:get(<<"group">>,Rule) of
                                  <<"any">> ->
                                      <<"\"any\"">>;
                                  <<"all-active">> ->
                                      <<"\"all-active\"">>;
                                  OtherGroup ->
                                      case maps:get(<<"group_type">>,Rule) of
                                          <<"ZONE">> ->
                                              erlang:iolist_to_binary([
                                                                       <<"dog_zone">>,<<".">>,OtherGroup,<<".id">>
                                                                      ]);
                                          _Other ->
                                              erlang:iolist_to_binary([
                                                                       <<"dog_group">>,<<".">>,OtherGroup,<<".id">>
                                                                      ])
                                      end
                              end,
                      Service = case maps:get(<<"service">>, Rule) of
                                    <<"any">> ->
                                        <<"any">>;
                                        OtherService ->
                                          erlang:iolist_to_binary([
                                            <<"dog_service">>,<<".">>,OtherService,<<".id">>
                                                                  ])
                                end,
                      Bindings = #{
                                   'Action' => maps:get(<<"action">>,Rule),
                                   'Active' => maps:get(<<"active">>,Rule),
                                   'Comment' => maps:get(<<"comment">>,Rule),
                                   'Environments' =>
                                   io_lib:format("~p",[maps:get(<<"environments">>,Rule)]),
                                   'Group' => Group,
                                   'GroupType' => maps:get(<<"group_type">>,Rule),
                                   'Interface' => maps:get(<<"interface">>,Rule),
                                   'Log' => maps:get(<<"log">>,Rule),
                                   'LogPrefix' => maps:get(<<"log_prefix">>,Rule),
                                   'Service' => Service,
                                   'States' =>
                                   io_lib:format("~p",[maps:get(<<"states">>,Rule)]),
                                   'Type' => maps:get(<<"type">>,Rule)
                                  },
                      {ok, Snapshot} = eel:compile(<<
                                                     "      {\n"
                                                     "        action       = \"<%= Action .%>\"\n"
                                                     "        active       = \"<%= Active .%>\"\n"
                                                     "        comment      = \"<%= Comment .%>\"\n"
                                                     "        environments = <%= Environments .%>\n"
                                                     "        group        = <%= Group .%>\n"
                                                     "        group_type   = \"<%= GroupType .%>\"\n"
                                                     "        interface    = \"<%= Interface .%>\"\n"
                                                     "        log          = \"<%= Log .%>\"\n"
                                                     "        log_prefix   = \"<%= LogPrefix .%>\"\n"
                                                     "        service      = <%= Service .%>\n"
                                                     "        states       = <%= States .%>\n"
                                                     "        type         = \"<%= Type .%>\"\n"
                                                     "      },\n"
                                                   >>),
      {ok, RenderSnapshot} = eel_renderer:render(Bindings, Snapshot),
      {IoData, _} = {eel_evaluator:eval(RenderSnapshot), RenderSnapshot},
      erlang:iolist_to_binary(IoData)
end, Rules).

-spec get_all_active_names() -> {ok, list()}.
get_all_active_names() ->
    {ok, AllActive} = get_all_active(),
    ids_to_names(AllActive).

-spec get_all_active() -> {ok, list()}.
get_all_active() ->
    {ok, Result} = dog_rethink:run( fun(X) ->
     reql:db(X, dog),
     reql:table(X, group),
     reql:has_fields(X, [<<"profile_id">>]),
     reql:get_field(X, <<"profile_id">>)
    end),
    {ok, All} = rethink_cursor:all(Result),
    ActiveGroupProfileIds = lists:flatten(All),

    {ok, Result2} = dog_rethink:run( fun(X) ->
     reql:db(X, dog),
     reql:table(X, ruleset),
     reql:has_fields(X, [<<"profile_id">>])
    end),
    {ok, All2} = rethink_cursor:all(Result2),
    ActiveRulesets = lists:flatten(All2),

    ProfileToRulesetList = lists:map(fun(Ruleset) ->
                      {
                      maps:get(<<"profile_id">>,Ruleset),
                      Ruleset
                      }
      end, ActiveRulesets),
    ProfileToRulesetMap = maps:from_list(ProfileToRulesetList),

    Rulesets = lists:flatten(
                 lists:map(fun(GroupProfileId) -> 
                          maps:get(GroupProfileId, ProfileToRulesetMap,[])
                 end, ActiveGroupProfileIds)
                ),
    RulesetsUnique = sets:to_list(sets:from_list(Rulesets)),
    {ok, RulesetsUnique}.
