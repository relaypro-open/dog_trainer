-module(dog_service_api_v2).

-include("dog_trainer.hrl").

-define(VALIDATION_TYPE, <<"service">>).
-define(TYPE_TABLE, service).

%API
-export([
         create/1,
         delete/1,
         get_all/0,
         get_by_id/1,
         get_by_name/1, 
         update/2
        ]).

-export([
        get/1, 
        init/0
        ]).

-spec create(Service :: map()) -> {ok | error, Key :: iolist() | name_exists }.
create(ServiceMap@0) ->
    Name = maps:get(<<"name">>, ServiceMap@0),
    {ok, ExistingServices} = get_all(),
    ExistingNames = [maps:get(<<"name">>,Service) || Service <- ExistingServices],
    case lists:member(Name, ExistingNames) of
        false -> 
            case dog_json_schema:validate(?VALIDATION_TYPE,ServiceMap@0) of
                ok ->
                    {ok, R} = dog_rethink:run(
                          fun(X) -> 
                                  reql:db(X, dog),
                                  reql:table(X, ?TYPE_TABLE),
                                  reql:insert(X, ServiceMap@0,#{return_changes => always})
                          end),
                    lager:debug("create R: ~p~n", [R]),
                    NewVal = maps:get(<<"new_val">>,hd(maps:get(<<"changes">>,R))),
                    {ok, NewVal};
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        true ->
            {error, name_exists}
    end.

-spec delete(ZoneId :: binary()) -> ok | {error, Error :: map()}.
delete(Id) ->
  case dog_service:in_active_profile(Id) of
        {false,[]} -> 
            {ok, R} = dog_rethink:run(
                                      fun(X) -> 
                                              reql:db(X, dog),
                                              reql:table(X, ?TYPE_TABLE),
                                              reql:get(X, Id),
                                              reql:delete(X)
                                      end),
            lager:debug("delete R: ~p~n",[R]),
            Deleted = maps:get(<<"deleted">>, R),
            case Deleted of
                1 -> ok;
                _ -> {error,#{<<"error">> => <<"error">>}}
            end;
        {true,Profiles} ->
            lager:info("service ~p not deleted, in profiles: ~p~n",[Id,Profiles]),
            {error,#{<<"errors">> => #{<<"in active profile">> => Profiles}}}
     end.

-spec get(Name :: binary()) -> [map()].
get(Name) ->
  dog_service:get(Name).

-spec get_all() -> {ok, list()}.
get_all() ->
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, ?TYPE_TABLE)
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Services = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    {ok, Services}.

-spec get_by_id(Id :: binary()) -> {ok, map()} | {error, atom()}.
get_by_id(Id) ->
  dog_service:get_by_id(Id).

-spec get_by_name(Name :: binary()) -> {ok, map()} | {error, atom()}.
get_by_name(Name) ->
  dog_service:get_by_name(Name).

-spec init() -> any(). 
init() ->
  pass.

-spec update(Id :: binary(), UpdateMap :: map()) -> {atom(), any()} .
update(Id, UpdateMap) ->
    case get_by_id(Id) of
        {ok, OldService} ->
            NewService = maps:merge(OldService,UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE,NewService) of
                ok ->
                    {ok, R} = dog_rethink:run(
                          fun(X) -> 
                                  reql:db(X, dog),
                                  reql:table(X, ?TYPE_TABLE),
                                  reql:get(X, Id),
                                  reql:update(X,UpdateMap,#{return_changes => always})
                          end),
                    lager:debug("update R: ~p~n", [R]),
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
