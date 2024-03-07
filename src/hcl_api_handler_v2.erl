-module(hcl_api_handler_v2).

-include("dog_trainer.hrl").
-include_lib("kernel/include/logger.hrl").

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([from_json/2]).
-export([resource_exists/2]).
-export([extract_module_from_path/3]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

extract_module_from_path(Path, Start, Length) ->
    Path@0 = erlang:binary_to_list(Path),
    Path@1 = lists:flatten(
        lists:join("/", lists:sublist(string:split(Path@0, "/", all), Start, Length))
    ),
    Path@2 = erlang:list_to_binary(Path@1),
    Path@2.

get_handler_module(Path) ->
    handler_lookup(extract_module_from_path(Path, 5, 1)).

handler_lookup(<<"group">>) -> dog_group_api_v2;
handler_lookup(<<"host">>) -> dog_host_api_v2;
handler_lookup(<<"link">>) -> dog_link_api_v2;
handler_lookup(<<"profile">>) -> dog_profile_api_v2;
handler_lookup(<<"service">>) -> dog_service_api_v2;
handler_lookup(<<"zone">>) -> dog_zone_api_v2;
handler_lookup(<<"ruleset">>) -> dog_ruleset_api_v2;
handler_lookup(<<"fact">>) -> dog_fact_api_v2.

content_types_provided(Req, State) ->
    {
        [
            {<<"text/plain">>, to_hcl}
        ],
        Req,
        State
    }.

content_types_accepted(Req, State) ->
    case cowboy_req:method(Req) of
        <<"PUT">> ->
            {[{<<"application/json">>, from_json}], Req, State };
        <<"POST">> ->
            {[{<<"application/json">>, from_json}], Req, State }
    end.

allowed_methods(Req, State) ->
    {[<<"PUT">>,<<"POST">>], Req, State}.

resource_exists(Req@0, State@0) ->
    {false, Req@0, State@0}.

from_json(Req@0, State@0) ->
    Body = cowboy_req:read_body(Req@0),
    Path = cowboy_req:path(Req@0),
    Handler = get_handler_module(Path),
    {ok, NewContent, Req@1} = Body,
    Map = jsx:decode(NewContent, [return_maps]),
    State@1 = maps:put(<<"object">>, Map, State@0),
    State@2 = maps:put(<<"content">>, NewContent, State@1),
    cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"plain/text">>
        },
        Handler:to_hcl(Map),
        Req@1
    ),
    {stop, Req@1, State@2}.
