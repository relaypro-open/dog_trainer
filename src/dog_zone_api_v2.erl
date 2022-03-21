-module(dog_zone_api_v2).

-include("dog_trainer.hrl").

-define(VALIDATION_TYPE, <<"zone">>).
-define(TYPE_TABLE, zone).

%API
-export([
         create/1,
         delete/1,
         get_all/0,
         get_by_id/1,
         get_by_name/1, 
         update/2
        ]).

-spec create(Group :: map()) -> {ok | error, Key :: iolist() | name_exists }.
create(ZoneMap@0) ->
    {ok, ZoneMap@1} = dog_zone:cleanup(ZoneMap@0),
    Name = maps:get(<<"name">>, ZoneMap@1),
    {ok, ExistingZones} = get_all(),
    ExistingNames = [maps:get(<<"name">>,Zone) || Zone <- ExistingZones],
    case lists:member(Name, ExistingNames) of
        false ->
            {ok, R} = dog_rethink:run(
                                      fun(X) -> 
                                              reql:db(X, dog),
                                              reql:table(X, ?TYPE_TABLE),
                                              reql:insert(X, ZoneMap@1,#{return_changes => always})
                                      end),
            NewVal = maps:get(<<"new_val">>,hd(maps:get(<<"changes">>,R))),
            {ok, NewVal};
        true ->
            {error, name_exists}
    end.

-spec delete(ZoneId :: binary()) -> ok | {error, Error :: map()}.
delete(Id) ->
  case dog_zone:in_active_profile(Id) of
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
            lager:info("zone ~p not deleted, in profiles: ~p~n",[Id,Profiles]),
            {error,#{ <<"errors">> => #{<<"in active profile">> => Profiles}}}
     end.

-spec get_all() -> {ok, list()}.
get_all() ->
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, ?TYPE_TABLE)
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Zones = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    {ok, Zones}.

-spec get_by_id(ZoneId :: binary()) -> {ok, map()} | {ok, null} | {error, atom()}.
get_by_id(ZoneId) ->
  dog_zone:get_by_id(ZoneId).

-spec get_by_name(binary()) -> {'ok', map()} | {'error', notfound}.
get_by_name(Name) ->
  dog_zone:get_by_name(Name).

-spec update(ZoneId :: binary(), UpdateMap :: map()) -> {atom(), any()} .
update(Id, UpdateMap@0) ->
    lager:debug("UpdateMap: ~p~n", [UpdateMap@0]),
    {ok, UpdateMap@1} = dog_zone:cleanup(UpdateMap@0),
    case get_by_id(Id) of
        {ok, OldService} ->
            NewService = maps:merge(OldService,UpdateMap@1),
            case dog_json_schema:validate(?VALIDATION_TYPE,NewService) of
                ok ->
                    {ok, R} = dog_rethink:run(
                          fun(X) -> 
                                  reql:db(X, dog),
                                  reql:table(X, ?TYPE_TABLE),
                                  reql:get(X, Id),
                                  reql:update(X,UpdateMap@1,#{return_changes => always})
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
