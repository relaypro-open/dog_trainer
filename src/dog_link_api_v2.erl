-module(dog_link_api_v2).

-include("dog_trainer.hrl").

-define(VALIDATION_TYPE, <<"link">>).
-define(TYPE_TABLE, link).

%API
-export([
         create/1,
         delete/1,
         get/1, 
         get_all/0,
         get_by_id/1,
         get_by_name/1, 
         update/2
        ]).

-spec create(Group :: map()) -> {ok | error, Key :: iolist() | name_exists }.
create(LinkMap@0) ->
    Name = maps:get(<<"name">>, LinkMap@0),
    {ok, ExistingLinks} = get_all(),
    ExistingNames = [maps:get(<<"name">>,Link) || Link <- ExistingLinks],
    case lists:member(Name, ExistingNames) of
        false -> 
            case dog_json_schema:validate(?VALIDATION_TYPE,LinkMap@0) of
                ok ->
                    {ok, R} = dog_rethink:run(
                          fun(X) -> 
                                  reql:db(X, dog),
                                  reql:table(X, ?TYPE_TABLE),
                                  reql:insert(X, LinkMap@0,#{return_changes => always})
                          end),
                    lager:debug("create R: ~p~n", [R]),
                    create_empty_external(Name),
                    NewVal = maps:get(<<"new_val">>,hd(maps:get(<<"changes">>,R))),
                    {ok, NewVal};
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        true ->
            {error, name_exists}
    end.

create_empty_external(EnvName) ->
  ExternalMap = dog_external:empty_external(EnvName),
  dog_external:create(ExternalMap).

-spec delete(Id :: binary()) -> ok | {error, Error :: map() }.
delete(Id) ->
  case dog_link:is_enabled(Id) of
        true ->
            lager:info("link ~p not deleted, is enabled~n",[Id]),
            {error,#{<<"errors">> => #{<<"unable to delete">> => <<"link enabled">>}}};
        false -> 
            delete_related_external(Id),
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
            end
    end.

-spec delete_related_external(Id :: binary()) -> (ok | error).
delete_related_external(Id) ->
    lager:debug("Id: ~p",[Id]),
    {ok, Link} = get_by_id(Id),
    LinkName = maps:get(<<"name">>,Link),
    dog_external:delete(LinkName).

-spec get(Name :: binary()) -> [map()].
get(Name) ->
   {ok, LinkDefinition} = get_by_name(Name),
   lager:debug("LinkDefinition: ~p",[LinkDefinition]),
   Link = maps:get(<<"links">>,LinkDefinition),
   Link.

-spec get_all() -> {ok, list()}.
get_all() ->
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, ?TYPE_TABLE)
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Links = case lists:flatten(Result) of
                [] -> 
                  [];
                Else -> 
                  Else
            end,
    {ok, Links}.

-spec get_by_id(Id :: binary()) -> {ok, map()} | {error, atom()}.
get_by_id(Id) ->
  {ok, R} = dog_rethink:run(
  fun(X) -> 
      reql:db(X, dog), 
      reql:table(X, ?TYPE_TABLE),
      reql:get(X, Id)
  end),
  case R of
     null -> 
          {error, notfound};
     _ -> 
      Link = R,
      {ok, Link}
  end.

-spec get_by_name(Name :: binary()) -> {ok, map()} | {error, atom()}.
get_by_name(Name) ->
    {ok, R} = dog_rethink:run(
                               fun(X) -> 
                                       reql:db(X, dog), 
                                       reql:table(X, ?TYPE_TABLE),
                                       reql:get_all(X, Name, #{index => <<"name">>})
                              end),
    {ok, R3} = rethink_cursor:all(R),
    Result = lists:flatten(R3),
    case Result of
        [] -> 
            lager:error("error, link name not found: ~p",[Name]),
            {error, notfound};
        _ -> 
          Link = hd(Result),
          {ok, Link}
    end.

-spec update(Id :: binary(), UpdateMap :: map()) -> {atom(), any()} .
update(Id, UpdateMap) ->
    case get_by_id(Id) of
        {ok, OldLink} ->
            NewLink = maps:merge(OldLink,UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE,NewLink) of
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
