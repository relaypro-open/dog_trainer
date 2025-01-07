-module(dog_host_api_v2).

-include("dog_trainer.hrl").

-define(VALIDATION_TYPE, <<"host">>).
-define(TYPE_TABLE, host).

%API
-export([
    create/1,
    delete/1,
    get_all/0,
    get_all_active/0,
    get_by_hostkey/1,
    get_by_id/1,
    get_by_name/1,
    to_hcl/1,
    to_hcl_by_id/1,
    update/2,
    update_by_hostkey/2
]).

get_by_hostkey(HostKey) ->
    dog_host:get_by_hostkey(HostKey).

get_by_id(Id) ->
    dog_host:get_by_id(Id).

-spec get_by_name(binary()) -> {ok, map()} | {error, notfound}.
get_by_name(Name) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get_all(X, Name, #{index => <<"name">>}),
            reql:filter(X, #{<<"active">> => <<"active">>})
        end
    ),
    {ok, R3} = rethink_cursor:all(R),
    Result = lists:flatten(R3),
    case Result of
        [] -> {error, notfound};
        _ -> {ok, hd(Result)}
    end.

-spec create(Host :: map()) -> {ok | error, Key :: iolist() | name_exists}.
create(HostMap@0) ->
    case dog_json_schema:validate(?VALIDATION_TYPE, HostMap@0) of
        ok ->
            Hostkey = maps:get(<<"hostkey">>, HostMap@0, notfound),
            case Hostkey of
                notfound ->
                    ?LOG_DEBUG("No hostkey found"),
                    {error, no_hostkey};
                _ ->
                    ?LOGT_DEBUG("HostMap@0: ~p", [{host_map@0,HostMap@0}]),
                    case dog_host:get_by_hostkey(Hostkey) of
                        {ok, _ExistingHost} ->
                            {error, exists};
                        {error, notfound} ->
                            {ok, ExistingHosts} = get_all(),
                            ExistingHostkeys = [maps:get(<<"hostkey">>, Host) || Host <- ExistingHosts],
                            DefaultValuesHostMap = #{
                                                     <<"active">> => <<"new">>,
                                                     <<"environment">> => <<"*">>,
                                                     <<"hash_alert_sent">> => <<"">>,
                                                     <<"hash_fail_count">> => 0,
                                                     <<"hostkey">> => <<"">>,
                                                     <<"ipset_hash_timestamp">> => <<"">>,
                                                     <<"iptables_hash_timestamp">> => <<"">>,
                                                     <<"keepalive_alert_sent">> => <<"">>,
                                                     <<"keepalive_timestamp">> => <<"">>,
                                                     <<"location">> => <<"*">>
                                                    },
                            MergedHostMap = maps:merge(DefaultValuesHostMap, HostMap@0),
                            case lists:member(Hostkey, ExistingHostkeys) of
                                false ->
                                    {ok, R} = dog_rethink:run(
                                                fun(X) ->
                                                        reql:db(X, dog),
                                                        reql:table(X, ?TYPE_TABLE),
                                                        reql:insert(X, MergedHostMap, #{return_changes => always})
                                                end
                                               ),
                                    NewVal = maps:get(<<"new_val">>, hd(maps:get(<<"changes">>, R))),
                                    {ok, NewVal};
                                true ->
                                    {error, name_exists}
                            end
                    end
            end;
        {error, Error} ->
            Response = dog_parse:validation_error(Error),
            {validation_error, Response}
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
    Hosts =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Hosts}.

-spec get_all_active() -> {ok, list()}.
get_all_active() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:filter(X, #{<<"active">> => <<"active">>})
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Hosts =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Hosts}.

-spec delete(Id :: binary()) -> (ok | error).
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
        _ -> error
    end.

-spec update(Id :: binary(), UpdateMap :: map()) ->
    {ok, iolist()} | {false, iolist()} | {false, no_updated} | {validation_error, iolist()}.
update(Id, UpdateMap) ->
    case dog_host:get_by_id(Id) of
        {ok, OldHost} ->
            NewHost = maps:merge(maps:remove(<<"vars">>,OldHost), UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewHost) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:get(X, Id),
                            reql:replace(X, NewHost, #{return_changes => always})
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

-spec update_by_hostkey(HostKey :: binary(), UpdateMap :: map()) -> no_return().
update_by_hostkey(HostKey, UpdateMap) ->
    case dog_host:get_id_by_hostkey(HostKey) of
        {ok, Id} ->
            update(Id, UpdateMap);
        {error, Reason} ->
            ?LOGT_INFO("Update for unknown host: ~p, Reason: ~p", [{host_key,HostKey}, {reason,Reason}]),
            create(UpdateMap)
    end.

-spec to_hcl_by_id(HostId :: iolist()) -> iolist().
to_hcl_by_id(HostId) ->
    {ok, Host} = get_by_id(HostId),
    to_hcl(Host). 

-spec to_hcl(Host :: map()) -> binary().
to_hcl(Host) ->
    Bindings = #{
                 'TerraformName' => dog_common:to_terraform_name(maps:get(<<"name">>, Host)), 
                 'Name' => maps:get(<<"name">>, Host), 
                 'Environment' => <<"qa">>,
                 'Group' => maps:get(<<"group">>, Host), 
                 'HostKey' => maps:get(<<"hostkey">>, Host), 
                 'Location' => maps:get(<<"location">>, Host), 
                 'Vars' => dog_common:format_vars(maps:get(<<"vars">>, Host,[])), 
                 'Provider' => <<"dog">>
                },
    {ok, Snapshot} = eel:compile(<<
        "resource \"dog_host\" \"<%= TerraformName .%>\" {\n"
        "  environment = \"<%= Environment .%>\"\n"
        "  group       = dog_group.<%= Group .%>.name\n"
        "  hostkey     = \"<%= HostKey .%>\"\n"
        "  location    = \"<%= Location .%>\"\n"
        "  name        = \"<%= Name .%>\"\n"
        "  provider    = dog.<%= Environment .%> \n"
        "<%= case Vars of %>"
        "<% [] -> <<>> ; %>"
        "<% _ ->  %>"
		"  vars = jsonencode({\n"
        "<%= lists:map(fun({Key,Value}) -> %>"
        "    <%= Key .%> = <%= Value .%> \n"
        "<% end, maps:to_list(Vars)) .%>"
        "  })\n"
        "<% end .%>"
        "}\n"
    >>),
    {ok, RenderSnapshot} = eel_renderer:render(Bindings, Snapshot),
    {IoData, _} = {eel_evaluator:eval(RenderSnapshot), RenderSnapshot},
    erlang:iolist_to_binary(IoData).
