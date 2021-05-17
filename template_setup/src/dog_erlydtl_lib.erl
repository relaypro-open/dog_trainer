-module(dog_erlydtl_lib).
-behaviour(erlydtl_library).

-export([
         version/0,
         inventory/1,
         get_secret/2
        ]).

version() ->
  1.

-spec inventory(filters) -> list().
inventory(filters) ->
  [];
inventory(tags) ->
  [get_secret].


-spec get_secret(ArgumentProplist :: list(), RenderArguments :: list()) -> iolist().
get_secret(ArgumentProplist,_RenderArguments) ->
  Type = proplists:get_value(type,ArgumentProplist,<<"credstash">>),
  Name = proplists:get_value(name,ArgumentProplist,<<"undefined">>),
  Table = proplists:get_value(table,ArgumentProplist,<<"credential-store">>),
  get_secret(Type,Name,Table).

get_secret(<<"credstash">>,Name, Table) ->
  {ok, Conf} = erlcloud_aws:profile(),
  Key = element(109,Conf),
  Secret = element(110,Conf),
  _Config = erlcloud_ec2:configure(Key,Secret),
  {ok, Value} = credstash:get_secret(Name, Table, env),
  Value.
