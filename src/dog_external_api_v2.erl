-module(dog_external_api_v2).

-include("dog_trainer.hrl").
%% ------------------------------------------------------------------

-define(VALIDATION_TYPE, <<"external">>).
-define(TYPE_TABLE, external).

%% API Function Exports
%% ------------------------------------------------------------------
-export([
        create/1,
        delete/1,
        get_all/0,
        get_by_id/1,
        get_by_name/1, 
        replace/2
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec create(External :: map()) -> {ok | error, Key :: iolist() | name_exists }.
create(ExternalMap@0) ->
  Name = maps:get(<<"name">>, ExternalMap@0),
  {ok, ExistingExternals} = get_all(),
  ExistingNames = case ExistingExternals of
                    [] ->
                      [];
                    EE ->
                      [maps:get(<<"name">>,External) || External <- EE]
                  end,
  DefaultValuesExternalMap = #{
                               <<"state">> => <<"active">>
                              },
  MergedExternalMap = maps:merge(DefaultValuesExternalMap, ExternalMap@0),
  MergedExternalMap2  = dog_time:merge_timestamp(MergedExternalMap),
  case lists:member(Name, ExistingNames) of
    false ->
      {ok, R} = dog_rethink:run(
                  fun(X) -> 
                      reql:db(X, dog),
                      reql:table(X, ?TYPE_TABLE),
                      reql:insert(X, MergedExternalMap2,#{return_changes => always})
                  end),
      NewVal = maps:get(<<"new_val">>,hd(maps:get(<<"changes">>,R))),
      {ok, NewVal};
    true ->
      {error, name_exists}
  end.

-spec delete(EnvName :: binary()) -> ok | error.
delete(EnvName) ->
  dog_external:delete(EnvName).
    
-spec get_all() -> {ok, list()}.
get_all() ->
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, ?TYPE_TABLE)
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Externals = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    {ok, Externals}.

-spec get_by_name(Name :: binary()) -> {ok, map()} | {error, atom()}.
get_by_name(Name) ->
  dog_external:get_by_name(Name).

-spec get_by_id(Id :: binary()) -> {ok, map()} | {error, atom()}.
get_by_id(Id) ->
  dog_external:get_by_id(Id).

-spec replace(Id :: binary(), UpdateMap :: map()) -> {true, iolist()} | {false, iolist()} | {false, no_replaced} | {validation_error, iolist()} .
replace(Id, UpdateMap) ->
    case get_by_id(Id) of
        {ok, OldExternal} ->
            NewExternal = maps:merge(OldExternal,UpdateMap),
            NewExternal2  = dog_time:merge_timestamp(NewExternal),
            NewExternal3 = maps:put(<<"id">>, Id, NewExternal2),
            case dog_json_schema:validate(?VALIDATION_TYPE,NewExternal3) of
                ok ->
                    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
                    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
                    {ok, R} = dog_rethink:run(
                          fun(X) -> 
                                  reql:db(X, dog),
                                  reql:table(X, ?TYPE_TABLE),
                                  reql:get(X, Id),
                                  reql:replace(X,NewExternal3,#{return_changes => always})
                              end),
                    lager:debug("replaced R: ~p~n", [R]),
                    Replaced = maps:get(<<"replaced">>, R),
                    Unchanged = maps:get(<<"unchanged">>, R),
                    case {Replaced,Unchanged} of
                      {1,0} -> 
                        NewVal = maps:get(<<"new_val">>,hd(maps:get(<<"changes">>,R))),
                        {true,NewVal};
                      {0,1} -> 
                        OldVal = maps:get(<<"old_val">>,hd(maps:get(<<"changes">>,R))),
                        {false,OldVal};
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
