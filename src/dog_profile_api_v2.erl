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

-export([
        nti/1
        ]).

-spec create(Profile :: map()) -> {'ok', iolist() } | {atom(), binary()}.
create(Profile) ->
    Profile@0 = names_to_ids(Profile),
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
    {ok, ActiveIds} = dog_profile:all_active(),
    {ok, R} = dog_rethink:run(
                              fun(X) ->
                                      reql:db(X, dog),
                                      reql:table(X, ?TYPE_TABLE)
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Profiles = case lists:flatten(Result) of
                   [] -> [];
                   Else -> Else
               end,
    ActiveProfiles = lists:filter(fun(Profile) -> lists:member(maps:get(<<"id">>,Profile),ActiveIds)
                 end, Profiles),
    ProfilesReplaced = lists:map(fun(Profile) ->
       ids_to_names(Profile) 
    end, ActiveProfiles),
    {ok, ProfilesReplaced}.

-spec get_by_id(Id :: binary()) -> {ok, map()} | {error, notfound}.
get_by_id(Id) ->
  {Result, Profile} = dog_profile:get_by_id(Id),
  {Result, ids_to_names(Profile)}.

-spec get_by_name(ProfileName :: binary()) -> {'ok', map() } | {error, atom()} .
get_by_name(ProfileName) ->
  {Result, Profile} = dog_profile:get_by_name(ProfileName),
  {Result, ids_to_names(Profile)}.

-spec update(Id :: binary(), UpdateMap :: map()) -> {atom(), Id :: iolist()} | {false, atom()}.
update(Id, UpdateMap) ->
    update(Id,UpdateMap,true).

-spec update(Id :: binary(), UpdateMap :: map(), InPlace :: boolean() ) -> {atom(), Id :: iolist()} | {false, atom()}.
update(Id, UpdateMap, _InPlace) ->
    update_in_place(Id,UpdateMap).

-spec update_in_place(Id :: binary(), UpdateMap :: map()) -> {false, atom()} | {validation_error, iolist()} | {true, binary()}.
update_in_place(Id, UpdateMapWithNames) ->
    UpdateMap = names_to_ids(UpdateMapWithNames),
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

-spec ids_to_names(Profile :: map()) -> Profile :: {ok | error , map()}.
ids_to_names(Profile) ->
  case Profile of 
    _ when not is_map(Profile) ->
      Profile;
    _ ->
      Inbound = nested:get([<<"rules">>,<<"inbound">>],Profile,[]),
      Outbound = nested:get([<<"rules">>,<<"outbound">>],Profile,[]),
      ServicesById = dog_service:get_all_grouped_by_id(),
      ZonesById = dog_zone:get_all_grouped_by_id(),
      GroupsById = dog_group:get_all_grouped_by_id(),
      InboundReplaced = case Inbound of 
                          []  ->
                            [];
                          _ -> 
                            rule_ids_to_names(Inbound,ServicesById,ZonesById,GroupsById)
                        end,
      OutboundReplaced = case Outbound of
                           [] ->
                             [];
                           _ -> 
                             rule_ids_to_names(Outbound,ServicesById,ZonesById,GroupsById)
                         end,
      NewRules = #{<<"inbound">> => InboundReplaced, 
                    <<"outbound">> => OutboundReplaced},
      maps:update(<<"rules">>,NewRules,Profile)
  end.

rule_ids_to_names(Rules,ServicesById,ZonesById,GroupsById) ->
  lists:map(fun(Rule) ->
                ServiceName = case maps:get(<<"service">>,Rule) of
                  <<"any">> -> 
                                  <<"any">>;
                  ServiceId -> 
                                  maps:get(<<"name">>, maps:get(ServiceId,ServicesById))
                end,
                RuleServiceReplaced = maps:update(<<"service">>,ServiceName,Rule),
                ZonesGroupsReplaced = case maps:get(<<"group_type">>, Rule) of
                  <<"ANY">> ->
                                          RuleServiceReplaced;
                  <<"ZONE">> ->
                              ZoneName = case maps:get(<<"group">>,Rule) of
                                <<"any">> -> 
                                                <<"any">>;
                                ZoneId -> 
                                                maps:get(<<"name">>, maps:get(ZoneId,ZonesById))
                              end,
                              maps:update(<<"group">>,ZoneName,RuleServiceReplaced);
                  <<"ROLE">> ->
                                GroupName = case maps:get(<<"group">>,Rule) of
                                  <<"any">> -> 
                                                  <<"any">>;
                                  GroupId -> 
                                                  maps:get(<<"name">>, maps:get(GroupId,GroupsById))
                                end,
                                maps:update(<<"group">>,GroupName,RuleServiceReplaced)
                end,
                ZonesGroupsReplaced
  end, Rules).

nti(ProfileId) ->
  {ok, Profile} = get_by_id(ProfileId),
  %Itn = ids_to_names(Profile),
  names_to_ids(Profile).

-spec names_to_ids(Profile :: map()) -> Profile :: {ok | error , map()}.
names_to_ids(Profile) ->
  Inbound = nested:get([<<"rules">>,<<"inbound">>],Profile,[]),
  Outbound = nested:get([<<"rules">>,<<"outbound">>],Profile,[]),
  ServicesByName = dog_service:get_all_grouped_by_name(),
  ZonesByName = dog_zone:get_all_grouped_by_name(),
  GroupsByName = dog_group:get_all_grouped_by_name(),
  InboundReplaced = case Inbound of
                      [] -> 
                        [];
                      _ -> 
                        rule_names_to_ids(Inbound,ServicesByName,ZonesByName,GroupsByName)
                    end,
  OutboundReplaced = case Outbound of
                       [] -> 
                         [];
                       _ -> rule_names_to_ids(Outbound,ServicesByName,ZonesByName,GroupsByName)
                     end,
  NewRules = #{<<"inbound">> => InboundReplaced, 
                <<"outbound">> => OutboundReplaced},
  maps:update(<<"rules">>,NewRules,Profile).

rule_names_to_ids(Rules,ServicesByName,ZonesByName,GroupsByName) ->
  lists:map(fun(Rule) ->
                ServiceId = case maps:get(<<"service">>,Rule) of
                  <<"any">> -> 
                                  <<"any">>;
                  ServiceName -> 
                                  maps:get(<<"id">>, maps:get(ServiceName,ServicesByName))
                end,
                RuleServiceReplaced = maps:update(<<"service">>,ServiceId,Rule),
                ZonesGroupsReplaced = case maps:get(<<"group_type">>, Rule) of
                  <<"ANY">> ->
                                          RuleServiceReplaced;
                  <<"ZONE">> ->
                              ZoneId = case maps:get(<<"group">>,Rule) of
                                <<"any">> -> 
                                                <<"any">>;
                                ZoneName -> 
                                                maps:get(<<"id">>, maps:get(ZoneName,ZonesByName))
                              end,
                              maps:update(<<"group">>,ZoneId,RuleServiceReplaced);
                  <<"ROLE">> ->
                                GroupId = case maps:get(<<"group">>,Rule) of
                                  <<"any">> -> 
                                                  <<"any">>;
                                  GroupName -> 
                                                  maps:get(<<"id">>, maps:get(GroupName,GroupsByName))
                                end,
                                maps:update(<<"group">>,GroupId,RuleServiceReplaced)
                end,
                ZonesGroupsReplaced
  end, Rules).
