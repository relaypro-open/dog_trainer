-module(healthcheck_api_handler).

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
  RethinkdbHost = application:get_env(dog_trainer, rethinkdb_host,"localhost"),
  RethinkdbPort = application:get_env(dog_trainer, rethinkdb_port,28015),
  RethinkdbUser = application:get_env(dog_trainer, rethinkdb_username,"admin"),
  RethinkdbPassword = application:get_env(dog_trainer, rethinkdb_password,""),
  Conn = gen_rethink:connect(
           #{host => RethinkdbHost, 
             port => RethinkdbPort,
             user => binary:list_to_bin(RethinkdbUser),
             password => binary:list_to_bin(RethinkdbPassword)
            }
          ),
  case Conn of
    {ok, _Connection} -> 
      Result = #{<<"health">> => <<"ok">>},
      Json = jsx:encode(Result),
      {Json, Req, State};
    _ -> 
      Result = #{<<"health">> => <<"unable to access db">>},
      Json = jsx:encode(Result),
      {Json, Req, State}
  end.
