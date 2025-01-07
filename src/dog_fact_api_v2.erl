
-module(dog_fact_api_v2).

-include_lib("kernel/include/logger.hrl").
-include("dog_trainer.hrl").

-define(VALIDATION_TYPE, <<"fact">>).
-define(TYPE_TABLE, fact).

%API
-export([
    create/1,
    delete/1,
    get_all/0,
    get_by_id/1,
    get_by_name/1,
    get_schema/0,
    to_hcl/1,
    to_hcl_by_id/1,
    update/2
]).

-spec create(Fact :: map()) -> {ok | error, Key :: iolist() | name_exists}.
create(FactMap@0) ->
    case dog_json_schema:validate(?VALIDATION_TYPE, FactMap@0) of
        ok ->
            ?LOGT_DEBUG("create: ~p", [{fact_map@0,FactMap@0}]),
            Name = maps:get(<<"name">>, FactMap@0),
            {ok, ExistingFacts} = get_all(),
            ExistingNames = [maps:get(<<"name">>, Fact) || Fact <- ExistingFacts],
            case lists:member(Name, ExistingNames) of
                false ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:insert(X, FactMap@0, #{return_changes => always})
                        end
                    ),
                    NewVal = maps:get(<<"new_val">>, hd(maps:get(<<"changes">>, R))),
                    {ok, NewVal};
                true ->
                    {error, name_exists}
            end;
        {error, Error} ->
            Response = dog_parse:validation_error(Error),
            {validation_error, Response}
    end.

-spec delete(FactId :: binary()) -> ok | {error, Error :: map()}.
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
    Facts =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Facts}.

-spec get_by_id(FactId :: binary()) -> {ok, map()} | {ok, null} | {error, atom()}.
get_by_id(FactId) ->
    dog_fact:get_by_id(FactId).

-spec get_by_name(binary()) -> {'ok', map()} | {'error', notfound}.
get_by_name(Name) ->
    dog_fact:get_by_name(Name).

-spec update(FactId :: binary(), UpdateMap :: map()) -> {atom(), any()}.
update(Id, UpdateMap@0) ->
    case get_by_id(Id) of
        {ok, OldFact} ->
            %Fun = fun(K,V1) when is_map(K) -> map:remove(<<"vars">>,V1) end,
            %NoVarsFact = maps:map(Fun,OldFact),
            NoVarsFact = nested:remove([<<"groups">>,<<"all">>,<<"vars">>],OldFact),
            NewFact = maps:merge(NoVarsFact, UpdateMap@0),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewFact) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:get(X, Id),
                            reql:replace(X, NewFact, #{return_changes => always})
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

-spec get_schema() -> binary().
get_schema() ->
    dog_json_schema:get_file(?VALIDATION_TYPE).

-spec to_hcl_by_id(FactId :: iolist()) -> iolist().
to_hcl_by_id(FactId) ->
    {ok, Fact} = get_by_id(FactId),
    to_hcl(Fact). 

-spec to_hcl(Fact :: map()) -> binary().
to_hcl(Fact) ->
    Bindings = #{
                 'TerraformName' => dog_common:to_terraform_name(maps:get(<<"name">>, Fact)), 
                 'Name' => maps:get(<<"name">>, Fact), 
                 'Environment' => <<"qa">>,
                 'Groups' => to_hcl_group(maps:get(<<"groups">>, Fact))
                },
    {ok, Snapshot} = eel:compile(<<
        "resource \"dog_fact\" \"<%= TerraformName .%>\n"
        "    name = <%= Name .%>\n"
        "    groups = {\n"
        "<%= Groups .%>"
        "}\n"
    >>),
    {ok, RenderSnapshot} = eel_renderer:render(Bindings, Snapshot),
    {IoData, _} = {eel_evaluator:eval(RenderSnapshot), RenderSnapshot},
    erlang:iolist_to_binary(IoData).

-spec to_hcl_group(Group :: map()) -> binary().
to_hcl_group(Group) ->
    All = maps:get(<<"all">>, Group),
    Bindings = #{
                 'Children' => dog_common:format_value(maps:get(<<"children">>, All)), 
                 'Hosts' => dog_common:format_vars(maps:get(<<"hosts">>, All)),
                 'Vars' => dog_common:format_vars(maps:get(<<"vars">>, All, []))
                },
    {ok, Snapshot} = eel:compile(<<
        "      all = {\n"
        "        children = <%= Children .%>\n"
        "        hosts = {\n"
        "<%= case Hosts of %>"
        "<% [] -> <<>> ; %>"
        "<% _ ->  %>"
        "      }\n"
        "    }\n"
        "<% end .%>"
        "<%= case Vars of %>"
        "<% [] -> <<>> ; %>"
        "<% _ ->  %>"
		"    vars = jsonencode({\n"
        "<%= lists:map(fun({Key,Value}) -> %>"
        "      <%= Key .%> = <%= Value .%> \n"
        "<% end, maps:to_list(Vars)) .%>"
        "    })\n"
        "<% end .%>"
        "  }\n"
    >>),
    {ok, RenderSnapshot} = eel_renderer:render(Bindings, Snapshot),
    {IoData, _} = {eel_evaluator:eval(RenderSnapshot), RenderSnapshot},
    erlang:iolist_to_binary(IoData).
