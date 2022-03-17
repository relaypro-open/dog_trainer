-module(dog_profile_api_v2).

-include("dog_trainer.hrl").
-include_lib("erlcloud/include/erlcloud_ec2.hrl").

-define(VALIDATION_TYPE, <<"profile">>).
-define(TYPE_TABLE, profile).

%API
-export([
         create/1,
         delete/1,
         get_all/0,
         get_by_id/1,
         get_by_name/1, 
         update/2,
         update/3
        ]).

-spec create(Profile :: map()) -> {'ok', iolist() } | {atom(), binary()}.
create(Profile@0) ->
    lager:debug("Profile@0: ~p~n",[Profile@0]),
    Timestamp = dog_time:timestamp(),
    case dog_json_schema:validate(?VALIDATION_TYPE,Profile@0) of
        ok ->
            Profile@1 = maps:put(<<"created">>,Timestamp,Profile@0),
            {ok, R} = dog_rethink:run(
                                      fun(X) ->
                                              reql:db(X, dog),
                                              reql:table(X, ?TYPE_TABLE),
                                              reql:insert(X, Profile@1,#{return_changes => always})
                                      end),
            NewVal = maps:get(<<"new_val">>,hd(maps:get(<<"changes">>,R))),
            {ok, NewVal};
        {error, Error} ->
            lager:error("~p",[Error]),
            Response = dog_parse:validation_error(Error),
            {validation_error, Response}
    end.

-spec delete(GroupId :: binary()) -> (ok | {error, Error :: map()}).
delete(Id) ->
  dog_profile:delete(Id).

-spec get_all() -> {'ok',list()}.
get_all() ->
  dog_profile:get_all().

-spec get_by_id(Id :: binary()) -> {ok, map()} | {error, notfound}.
get_by_id(Id) ->
  dog_profile:get_by_id(Id).

-spec get_by_name(ProfileName :: binary()) -> {'ok', map() } | {error, atom()} .
get_by_name(ProfileName) ->
    dog_profile:get_by_name(ProfileName).

-spec update(Id :: binary(), UpdateMap :: map()) -> {atom(), Id :: iolist()} | {false, atom()}.
update(Id, UpdateMap) ->
    update(Id,UpdateMap,false).

-spec update(Id :: binary(), UpdateMap :: map(), InPlace :: boolean() ) -> {atom(), Id :: iolist()} | {false, atom()}.
update(Id, UpdateMap, InPlace) ->
    lager:debug("Id: ~p~n",[Id]),
    lager:debug("UpdateMap: ~p~n",[UpdateMap]),
    case get_by_id(Id) of
        {ok, Profile@0} ->
            Profile@1 = maps:merge(Profile@0,UpdateMap),
            Profile@2 = maps:remove(<<"id">>,Profile@1),
            case InPlace of
                false -> 
                    case create(Profile@2) of
                        {validation_error, Error} ->
                            {validation_error, Error};
                        {ok, Profile2} ->
                            case maps:find(<<"name">>,UpdateMap) of
                                error ->
                                    case lists:any(fun(X) -> element(1,X) /= ok end, dog_group_api_v2:replace_profile_by_profile_id(Id,Profile2)) of
                                        true ->
                                            {true, Profile2};
                                        false ->
                                            {false, error_replacing_profile}
                                    end;
                                {ok,UpdateName} ->
                                    lager:debug("UpdateName: ~p",[UpdateName]),
                                    case dog_group_api_v2:replace_profile_by_profile_id(Id,Profile2,UpdateName) of
                                        [] -> % This profile not associated with any group
                                            {true, Profile2};
                                        [{true, _}] ->
                                            {true, Profile2}
                                    end
                            end
                    end;
                true ->
                   update_in_place(Id,Profile@2) 
            end;
        {error, Error} ->
            {false, Error}
    end.

-spec update_in_place(Id :: binary(), UpdateMap :: map()) -> {false, atom()} | {validation_error, iolist()} | {true, binary()}.
update_in_place(Id, UpdateMap) ->
    lager:info("update_in_place"),
    case get_by_id(Id) of
        {ok,OldProfile} ->
            NewProfile = maps:merge(OldProfile,UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE,NewProfile) of
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
