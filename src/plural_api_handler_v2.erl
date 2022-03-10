-module(plural_api_handler_v2).

-include("dog_trainer.hrl").

-export([init/2]).
-export([content_types_provided/2]).
-export([allowed_methods/2]).
-export([to_json/2]).

get_handler_module(Path) ->
    handler_lookup(api_handler:extract_module_from_path(Path,4,1)).

handler_lookup(<<"externals">>) -> dog_external;
handler_lookup(<<"groups">>) -> dog_group;
handler_lookup(<<"hosts">>) -> dog_host_api_v2;
handler_lookup(<<"links">>) -> dog_link;
handler_lookup(<<"profiles">>) -> dog_profile;
handler_lookup(<<"services">>) -> dog_service;
handler_lookup(<<"zones">>) ->  dog_zone.

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, to_json}
	], Req, State}.

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
            case Sub of
                undefined ->
                  {Json, Req, State}; 
                <<"schema">> ->
                  Schema = Handler:get_schema(),
                  {Schema, Req, State};
                <<"ips">> ->
                  {ok, Ips} = Handler:get_all_ips(),
                  {jsx:encode(Ips), Req, State}
            end
    end.
