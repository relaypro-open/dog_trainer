-module(dog_host_api_v2).

%-include("dog_trainer.hrl").

-define(VALIDATION_TYPE, <<"host">>).
-define(TYPE_TABLE, host).

%API
-export([
         create/1,
         delete/1,
         get_all/0,
         get_by_hostkey/1,
         get_by_id/1, 
         get_by_name/1,
         update/2,
         update_by_hostkey/2
        ]).

get_by_hostkey(HostKey) ->
  dog_host:get_by_hostkey(HostKey).

get_by_id(Id) ->
  dog_host:get_by_id(Id).

get_by_name(Name) ->
  dog_host:get_by_name(Name).

-spec create(Group :: map()) -> {ok | error, Key :: iolist() | name_exists }.
create(HostMap@0) ->
  %Name = maps:get(<<"name">>, HostMap@0),
  Hostkey = maps:get(<<"hostkey">>, HostMap@0, notfound),
  case Hostkey of
    notfound ->
      lager:debug("No hostkey found"),
      {error, no_hostkey};
    _ -> 
      lager:debug("HostMap@0: ~p",[HostMap@0]),
      case dog_host:get_by_hostkey(Hostkey) of
        {ok, _ExistingHost} ->
          {error,exists};
        {error,notfound} ->
          DefaultValuesHostMap = #{
                                   <<"active">> => <<"new">>,
                                   <<"environment">> => <<"*">>,
                                   <<"hash_alert_sent">> => <<"">>,
                                   <<"hash_fail_count">> => 0,
                                   <<"hostkey">> => <<"">>,
                                   <<"ipset_hash_timestamp">> => <<"">>,
                                   <<"iptables_hash_timestamp">> => <<"">>,
                                   <<"keepalive_alert_sent">> =>  <<"">>,
                                   <<"keepalive_timestamp">> => <<"">>,
                                   <<"location">> => <<"*">>
                                  },
          MergedHostMap = maps:merge(DefaultValuesHostMap, HostMap@0),
          {ok, R} = dog_rethink:run(
            fun(X) -> 
                reql:db(X, dog),
                reql:table(X, ?TYPE_TABLE),
                reql:insert(X,MergedHostMap,#{return_changes => always})
            end),
          NewVal = maps:get(<<"new_val">>,hd(maps:get(<<"changes">>,R))),
          {ok, NewVal}
      end
  end.
                  %{ok, Host} = get_by_id(hd(maps:get(<<"generated_keys">>,R))),

-spec update(Id :: binary(), UpdateMap :: map()) -> {ok, iolist()} | {false, iolist()} | {false, no_updated} | {validation_error, iolist()} .
update(Id, UpdateMap) ->
  case dog_host:get_by_id(Id) of
    {ok, OldService} ->
      NewService = maps:merge(OldService,UpdateMap),
      case dog_json_schema:validate(?VALIDATION_TYPE,NewService) of
        ok ->
          {ok,R} = dog_rethink:run(
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
              %{true,R};
            {0,1} -> 
              OldVal = maps:get(<<"old_val">>,hd(maps:get(<<"changes">>,R))),
              {false,OldVal};
              %{false,R};
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

-spec update_by_hostkey(HostKey :: binary(), UpdateMap :: map()) -> no_return().
update_by_hostkey(HostKey, UpdateMap) ->
  case dog_host:get_id_by_hostkey(HostKey) of
        {ok, Id} -> 
            update(Id, UpdateMap);
        {error, Reason} -> 
            lager:info("Update for unknown host: ~p, Reason: ~p",[HostKey,Reason]),
            create(UpdateMap)
    end.

-spec delete(Id :: binary()) -> (ok | error).
delete(Id) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
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
        _ -> error
    end.

-spec get_all() -> {ok, list()}.
get_all() ->
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, ?TYPE_TABLE)
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Hosts = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    {ok, Hosts}.
