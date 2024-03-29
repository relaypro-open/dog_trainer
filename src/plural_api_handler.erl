-module(plural_api_handler).

-include_lib("kernel/include/logger.hrl").

-export([init/2]).
-export([content_types_provided/2]).
-export([allowed_methods/2]).
-export([to_json/2]).

get_handler_module(Path) ->
    handler_lookup(api_handler:extract_module_from_path(Path, 3, 1)).

handler_lookup(<<"externals">>) -> dog_external;
handler_lookup(<<"groups">>) -> dog_group;
handler_lookup(<<"hosts">>) -> dog_host;
handler_lookup(<<"links">>) -> dog_link;
handler_lookup(<<"profiles">>) -> dog_profile;
handler_lookup(<<"services">>) -> dog_service;
handler_lookup(<<"zones">>) -> dog_zone;
handler_lookup(<<"rulesets">>) -> dog_ruleset;
handler_lookup(<<"facts">>) -> dog_fact.

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
                dog_host ->
                    case Sub of
                        <<"names">> ->
                            ObjectHosts = dog_host:get_names_by_ips(),
                            {jsx:encode(ObjectHosts), Req, State};
                        <<"hostkeys">> ->
                            ObjectHosts = dog_host:get_hostkeys_by_ips(),
                            {jsx:encode(ObjectHosts), Req, State};
                        undefined ->
                            {Json, Req, State}
                    end;
                dog_zone ->
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
