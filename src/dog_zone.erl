-module(dog_zone).

-include("dog_trainer.hrl").

-define(VALIDATION_TYPE, <<"zone">>).
-define(TYPE_TABLE, zone).

%API
-export([
         create/1,
         delete/1,
         get_by_id/1,
         get_by_name/1, 
         get_name_by_id/1,
         get_all/0,
         update/2,
         where_used/1,
         where_used_inbound/1,
         where_used_outbound/1,
         get_schema/0
        ]).

-export([
         all_ipv4s_grouped/0,
         all_ipv4s_grouped_by_id/0,
         all_ipv6s_grouped/0,
         all_ipv6s_grouped_by_id/0,
         get_all_grouped_by_id/0,
         get_all_ips/0,
         get_all_ipv4s_by_id/1,
         get_all_ipv4s_by_name/1,
         get_all_ipv6s_by_id/1,
         get_all_ipv6s_by_name/1,
         in_active_profile/1,
         init/0
        ]).

-spec init() -> no_return(). 
init() ->
  pass.

-spec get_name_by_id(ZoneId :: binary()) -> {ok, binary()} | {ok, null} | {ok, map()} | {error, atom()}.
get_name_by_id(ZoneId) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    R = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, ZoneId),
            reql:get_field(X, <<"name">>)
        end),
    case R of
            {error, Error} -> Error;
            {ok, Result} ->
                {ok, Result}
    end.

-spec get_zone_names() -> list().
get_zone_names() ->
    {ok, Zones} = get_all(),
    ZoneNames = [ maps:get(<<"name">>,Zone) || Zone <- Zones],
    ZoneNames.

-spec get_zone_ids() -> list().
get_zone_ids() ->
    {ok, Zones} = get_all(),
    ZoneIds = [ maps:get(<<"id">>,Zone) || Zone <- Zones],
    ZoneIds.

-spec all_ipv4s_grouped() -> map().
all_ipv4s_grouped() ->
    ZoneNames = get_zone_names(),
    Ipv4s = lists:map(fun(Name) -> {ok, Ips} = get_all_ipv4s_by_name(Name), {Name,Ips} end, ZoneNames),
    maps:from_list(lists:flatten(Ipv4s)).

-spec all_ipv4s_grouped_by_id() -> map().
all_ipv4s_grouped_by_id() ->
    ZoneIds = get_zone_ids(),
    Ipv4s = lists:map(fun(Id) -> {ok, Ips} = get_all_ipv4s_by_id(Id), {Id,Ips} end, ZoneIds),
    maps:from_list(lists:flatten(Ipv4s)).

-spec all_ipv6s_grouped() -> map().
all_ipv6s_grouped() ->
    ZoneNames = get_zone_names(),
    Ipv6s = lists:map(fun(Name) -> {ok, Ips} = get_all_ipv6s_by_name(Name), {Name,Ips} end, ZoneNames),
    maps:from_list(lists:flatten(Ipv6s)).

-spec all_ipv6s_grouped_by_id() -> map().
all_ipv6s_grouped_by_id() ->
    ZoneIds = get_zone_ids(),
    Ipv6s = lists:map(fun(Id) -> {ok, Ips} = get_all_ipv6s_by_id(Id), {Id,Ips} end, ZoneIds),
    maps:from_list(lists:flatten(Ipv6s)).

-spec get_all_ipv4s_by_id(ZoneId :: binary()) -> {ok, list()}. 
get_all_ipv4s_by_id(Id) ->
    case get_by_id(Id) of
        {ok, Zone} ->
            ZoneAddresses = maps:get(<<"ipv4_addresses">>, Zone),
            {ok, dog_ips:filter_ipv4(ZoneAddresses)};
        {error, notfound} ->
            {ok, []}
    end.

-spec get_all_ipv4s_by_name(binary()) -> {ok, list()}. 
get_all_ipv4s_by_name(Name) ->
    case get_by_name(Name) of
        {ok, Zone} ->
            ZoneAddresses = maps:get(<<"ipv4_addresses">>, Zone),
            {ok, dog_ips:filter_ipv4(ZoneAddresses)};
        {error, notfound} ->
            {ok, []}
    end.

-spec get_all_ips() -> {ok, list()}. 
get_all_ips() ->
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, ?TYPE_TABLE),
                                      reql:pluck(X, [<<"ipv4_addresses">>,<<"ipv6_addresses">>])
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Interfaces = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    Ips = lists:map(fun(X) -> [maps:get(<<"ipv4_addresses">>,X),maps:get(<<"ipv6_addresses">>,X)] end, Interfaces),
    {ok, lists:flatten(Ips)}.


-spec get_all_ipv6s_by_id(binary()) -> {ok, list()}. 
get_all_ipv6s_by_id(Id) ->
    {ok, Zone} = get_by_id(Id),
    ZoneAddresses = maps:get(<<"ipv6_addresses">>, Zone),
    {ok, dog_ips:filter_ipv6(ZoneAddresses)}.

-spec get_all_ipv6s_by_name(binary()) -> {ok, list()}. 
get_all_ipv6s_by_name(Name) ->
    case get_by_name(Name) of
        {ok, Zone} ->
            ZoneAddresses = maps:get(<<"ipv6_addresses">>, Zone),
            Ipv6Addresses = dog_ips:filter_ipv6(ZoneAddresses),
            {ok, Ipv6Addresses};
        {error,_} ->
            {ok,[]}
    end.

-spec get_document_by_id(ZoneId :: binary()) -> {ok, map()} | {ok, null} | {error, atom()}.
get_document_by_id(Id) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
                       fun(X) -> 
                               reql:db(X, dog), 
                               reql:table(X, ?TYPE_TABLE),
                               reql:get(X, Id)
                      end),
    case R of
        null -> {error, notfound};
        _ -> {ok, R}
    end.

-spec get_by_name(binary()) -> {'ok', map()} | {'error', notfound}.
get_by_name(Name) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
                               fun(X) -> 
                                       reql:db(X, dog), 
                                       reql:table(X, ?TYPE_TABLE),
                                       reql:get_all(X, Name, #{index => <<"name">>})
                               end),
    {ok, R3} = rethink_cursor:all(R),
    Result = lists:flatten(R3),
    case Result of
        [] -> {error, notfound};
        _ ->
            {ok, hd(Result)}
    end.

-spec get_by_id(ZoneId :: binary()) -> {ok, map()} | {ok, null} | {error, atom()}.
get_by_id(ZoneId) ->
    case get_document_by_id(ZoneId) of
        {ok, Zone} -> 
            {ok, Zone};
        {error, Error} -> 
            lager:error("zone id not found: ~p",[ZoneId]),
            {error, Error}
    end.

-spec get_all() -> {ok, list()}.
get_all() ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, ?TYPE_TABLE),
                                      reql:pluck(X, [<<"name">>,<<"id">>])
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Zones = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    {ok, Zones}.

-spec get_all_grouped_by_id() -> map().
get_all_grouped_by_id() ->
    {ok, All} = get_all(),
    maps:from_list([{maps:get(<<"id">>,Zone), Zone} || Zone <- All]).


-spec cleanup(Group :: map()) -> {ok, map()}.
cleanup(Group) ->
    Ipv6 = maps:get(<<"ipv6_addresses">>,Group),
    Ipv6Lower = [string:lowercase(Addr) || Addr <- Ipv6],
    GroupLower = maps:update(<<"ipv6_addresses">>,Ipv6Lower,Group),
    {ok, GroupLower}.

-spec create(Group :: map()) -> {ok | error, Key :: iolist() | name_exists }.
create(ZoneMap@0) ->
    {ok, ZoneMap@1} = cleanup(ZoneMap@0),
    Name = maps:get(<<"name">>, ZoneMap@1),
    {ok, ExistingZones} = get_all(),
    ExistingNames = [maps:get(<<"name">>,Zone) || Zone <- ExistingZones],
    case lists:member(Name, ExistingNames) of
        false ->
            %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
            %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
            {ok, R} = dog_rethink:run(
                                      fun(X) -> 
                                              reql:db(X, dog),
                                              reql:table(X, ?TYPE_TABLE),
                                              reql:insert(X, ZoneMap@1)
                                      end),
            Key = hd(maps:get(<<"generated_keys">>,R)),
            {ok, Key};
        true ->
            {error, name_exists}
    end.

-spec update(ZoneId :: binary(), UpdateMap :: map()) -> {atom(), any()} .
update(Id, UpdateMap@0) ->
    lager:debug("UpdateMap: ~p~n", [UpdateMap@0]),
    {ok, UpdateMap@1} = cleanup(UpdateMap@0),
    case get_by_id(Id) of
        {ok, OldService} ->
            NewService = maps:merge(OldService,UpdateMap@1),
            case dog_json_schema:validate(?VALIDATION_TYPE,NewService) of
                ok ->
                    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
                    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
                    {ok, R} = dog_rethink:run(
                          fun(X) -> 
                                  reql:db(X, dog),
                                  reql:table(X, ?TYPE_TABLE),
                                  reql:get(X, Id),
                                  reql:update(X,UpdateMap@1)
                          end),
                    lager:debug("update R: ~p~n", [R]),
                    Replaced = maps:get(<<"replaced">>, R),
                    Unchanged = maps:get(<<"unchanged">>, R),
                    case {Replaced,Unchanged} of
                        {1,0} -> {true,Id};
                        {0,1} -> {false,Id};
                        _ -> {false, no_updated}
                    end;
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {error, Error} ->
            {false, Error}
    end.

-spec delete(ZoneId :: binary()) -> ok | {error, Error :: map()}.
delete(Id) ->
    case in_active_profile(Id) of
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

%r.db('dog').table('profile')
%  .filter(function(group) {
%        return group("rules")("inbound")("group").contains("058e7bc0-1ea1-463d-babf-73a39110edad")}).getField("id").union(
%        r.db('dog').table('profile')
%          .filter(function(group) {
%                return group("rules")("outbound")("group").contains("058e7bc0-1ea1-463d-babf-73a39110edad")}).getField("id")).distinct()
%                .setIntersection(r.db("dog").table("group")("profile_id").distinct())

%TODO: differentiate between ROLE(Group) and ZONE(Zone) groups. 
-spec where_used_inbound(ZoneId :: binary()) -> {ok, ProfileIds :: list()}.
where_used_inbound(ZoneId) ->
    {ok, R} = dog_rethink:run(
                                fun(X) -> 
                                    reql:db(X, dog),
                                    reql:table(X, profile),
                                    reql:filter(X, fun(Profile) ->
                                        reql:get_field(Profile, <<"rules">>),
                                        reql:get_field(Profile, <<"inbound">>),
                                        reql:get_field(Profile, <<"group">>),
                                        reql:contains(Profile,ZoneId)
                                    end),
                                    reql:get_field(X,<<"id">>)
                                end),
    {ok, Result} = rethink_cursor:all(R),
    ProfileIds = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    lager:info("ProfileIds: ~p~n",[R]),
    {ok, ProfileIds}.

%TODO: differentiate between ROLE(Group) and ZONE(Zone) groups. 
-spec where_used_outbound(ZoneId :: binary()) -> {ok, ProfileIds :: list()}.
where_used_outbound(ZoneId) ->
    {ok, R} = dog_rethink:run(
                                fun(X) -> 
                                    reql:db(X, dog),
                                    reql:table(X, profile),
                                    reql:filter(X, fun(Profile) ->
                                        reql:get_field(Profile, <<"rules">>),
                                        reql:get_field(Profile, <<"outbound">>),
                                        reql:get_field(Profile, <<"group">>),
                                        reql:contains(Profile,ZoneId)
                                    end),
                                    reql:get_field(X,<<"id">>)
                                end),
    {ok, Result} = rethink_cursor:all(R),
    ProfileIds = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    lager:info("ProfileIds: ~p~n",[R]),
    {ok, ProfileIds}.

-spec where_used(ZoneId :: binary() ) -> {ok, ProfileIds :: list()}.
where_used(ZoneId) ->
    {ok, Inbound} = where_used_inbound(ZoneId),
    {ok, Outbound} = where_used_outbound(ZoneId),
    {ok, lists:flatten(sets:to_list(sets:from_list([Inbound,Outbound])))}.

-spec get_schema() -> binary().
get_schema() ->
  dog_json_schema:get_file(?VALIDATION_TYPE).

-spec in_active_profile(Id :: binary()) -> {false, []} | {true, Profiles :: list() }.
in_active_profile(Id) ->
    {ok, Used} = where_used(Id),
    {ok, Active} = dog_profile:all_active(),
    Profiles = sets:to_list(sets:intersection(sets:from_list(Used), sets:from_list(Active))),
    case Profiles of
        [] -> 
            {false,[]};
        _ -> 
            {true, Profiles}
    end.
