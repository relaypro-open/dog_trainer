-module(plural_api_handler_v2).

-include_lib("kernel/include/logger.hrl").

-export([init/2]).
-export([content_types_provided/2]).
-export([allowed_methods/2]).
-export([to_json/2]).

get_handler_module(Path) ->
    handler_lookup(api_handler:extract_module_from_path(Path, 4, 1)).

handler_lookup(<<"externals">>) -> dog_external_api_v2;
handler_lookup(<<"groups">>) -> dog_group_api_v2;
handler_lookup(<<"hosts">>) -> dog_host_api_v2;
handler_lookup(<<"links">>) -> dog_link_api_v2;
handler_lookup(<<"profiles">>) -> dog_profile_api_v2;
handler_lookup(<<"services">>) -> dog_service_api_v2;
handler_lookup(<<"zones">>) -> dog_zone_api_v2;
handler_lookup(<<"ipsets">>) -> dog_ipset_api_v2;
handler_lookup(<<"rulesets">>) -> dog_ruleset_api_v2;
handler_lookup(<<"facts">>) -> dog_fact_api_v2.

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {
        [
            {<<"application/json">>, to_json}
        ],
        Req,
        State
    }.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

to_json(Req, State) ->
    case cowboy_req:method(Req) of
        _ ->
            Path = cowboy_req:path(Req),
            Handler = get_handler_module(Path),
            {ok, Services} = Handler:get_all(),
            Json = jsx:encode(Services),
            Sub = cowboy_req:binding(sub, Req),
            case Handler of
                dog_ruleset_api_v2 ->
                    #{names := Names,
                     active := Active} =
                        cowboy_req:match_qs(
                            [
                                {names, [], undefined},
                                {active, [], undefined}
                            ],
                            Req
                        ),
                    case {Names,Active} of
                        {<<"true">>, <<"true">>} ->
                            {ok, RulesetsActiveNames} = dog_ruleset_api_v2:get_all_active_names(),
                            {jsx:encode(RulesetsActiveNames), Req, State};
                        {<<"true">>, undefined} ->
                            {ok, RulesetsNames} = dog_ruleset_api_v2:get_all_names(),
                            {jsx:encode(RulesetsNames), Req, State};
                        {undefined, <<"true">>} ->
                            {ok, RulesetsActive} = dog_ruleset_api_v2:get_all_active(),
                            {jsx:encode(RulesetsActive), Req, State};
                        {<<"false">>, undefined} ->
                            {Json, Req, State};
                        {undefined, undefined} ->
                            {Json, Req, State}
                    end;
                dog_host_api_v2 ->
                    case Sub of
                        <<"names">> ->
                            ObjectHosts = dog_host:get_names_by_ips(),
                            {jsx:encode(ObjectHosts), Req, State};
                        <<"hostkeys">> ->
                            ObjectHosts = dog_host:get_hostkeys_by_ips(),
                            {jsx:encode(ObjectHosts), Req, State};
                        undefined ->
                            #{active := Active} =
                                cowboy_req:match_qs(
                                    [
                                        {active, [], undefined}
                                    ],
                                    Req
                                ),
                            case Active of
                                <<"true">> ->
                                    {ok, Rulesets} = dog_host_api_v2:get_all_active(),
                                    {jsx:encode(Rulesets), Req, State};
                                <<"false">> ->
                                    {Json, Req, State};
                                undefined ->
                                    {Json, Req, State}
                            end
                    end;
                dog_zone_api_v2 ->
                    case Sub of
                        <<"ips">> ->
                            {ok, Ips} = dog_zone:get_all_ips(),
                            {jsx:encode(Ips), Req, State};
                        undefined ->
                            {Json, Req, State}
                    end;
                _ ->
                    case Sub of
                        <<"schema">> ->
                            Schema = Handler:get_schema(),
                            {Schema, Req, State};
                        undefined ->
                            {Json, Req, State}
                    end
            end
    end.
