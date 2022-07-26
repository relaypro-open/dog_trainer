-module(publish_api_handler).

-include("dog_trainer.hrl").

-export([init/2]).
-export([content_types_provided/2]).
-export([allowed_methods/2]).
-export([to_json/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, to_json}
	], Req, State}.

allowed_methods(Req, State) ->
        {[<<"GET">>], Req, State}.

to_json(Req, State) ->
    Json = case cowboy_req:match_qs([{group, [], plain}], Req) of
        #{group := GroupName} ->
            ?LOG_INFO("add_to_queue: ~p",[GroupName]),
            dog_profile_update_agent:add_to_queue([GroupName]),
            %ok = dog_profile:publish_ruleset(GroupName),
            jsx:encode(ok);
        _ ->
            jsx:encode(error)
    end,
    {Json, Req, State}.
