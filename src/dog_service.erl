-module(dog_service).

-include("dog_trainer.hrl").

-define(VALIDATION_TYPE, <<"service">>).
-define(TYPE_TABLE, service).

%API
-export([
         create/1,
         delete/1,
         get_by_id/1,
         get_by_name/1, 
         get_all/0,
         get_schema/0,
         update/2
        ]).

-export([
        get/1, 
        get_all_grouped_by_id/0,
        get_all_grouped_by_name/0,
        get_all_in_profile/1,
        get_id_by_name/1,
        get_name_by_id/1,
        in_active_profile/1,
        init/0, 
        where_used/1,
        any_service/0
        ]).

-spec init() -> any(). 
init() ->
  pass.

-spec get(Name :: binary()) -> [map()].
get(Name) ->
   {ok, ServiceDefinition} = get_by_name(Name),
   lager:debug("ServiceDefinition: ~p",[ServiceDefinition]),
   S = maps:get(<<"services">>,ServiceDefinition),
   Services = lists:map(fun(Service) -> parse_service(Service) end, S),
   Services.

-spec get_name_by_id(Id :: binary()) -> [iolist()].
get_name_by_id(<<"any">>) ->
    <<"ANY">>;
get_name_by_id(Id) ->
   lager:debug("Id: ~p",[Id]),
   case get_by_id(Id) of
       {ok, ServiceDefinition} -> 
           lager:debug("ServiceDefinition: ~p",[ServiceDefinition]),
           Name = maps:get(<<"name">>,ServiceDefinition),
           Name;
       {error, Error} ->
            lager:error("error, service id not found: ~p, ~p",[Id, Error]),
            {error, Error}
            %throw(service_not_found)
   end.

-spec get_id_by_name(Name :: binary()) -> [iolist()].
get_id_by_name(Name) ->
   {ok, ServiceDefinition} = get_by_name(Name),
   lager:debug("ServiceDefinition: ~p",[ServiceDefinition]),
   Id = maps:get(<<"id">>,ServiceDefinition),
   Id.

-spec get_by_name(Name :: binary()) -> {ok, map()} | {error, atom()}.
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
        [] -> 
            lager:error("error, service name not found: ~p",[Name]),
            {error, notfound};
            %throw(service_not_found);
        _ -> {ok, hd(Result)}
    end.

-spec get_by_id(Id :: binary()) -> {ok, map()} | {error, atom()}.
get_by_id(Id) ->
    case Id of
        <<"any">> ->
            {ok, any_service()};
        _ ->
            %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
            %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
            {ok, R} = dog_rethink:run(
            fun(X) -> 
                reql:db(X, dog), 
                reql:table(X, ?TYPE_TABLE),
                reql:get(X, Id)
            end),
            case R of
              null -> 
                    {error, notfound};
                    %throw(service_not_found);
               _ -> {ok, R}
            end 
    end.

-spec parse_service(map()) -> map().
parse_service(Json) ->
    Protocol = parse_protocol(maps:get(<<"protocol">>,Json)),
    Ports = parse_ports(maps:get(<<"ports">>,Json)),
    M = maps:new(), 
    M1 = maps:put(<<"protocol">>, Protocol, M),
    M2 = maps:put(<<"ports">>, Ports, M1),
    M2.

-spec parse_protocol(Protocol :: binary()) -> 'error' | binary().
parse_protocol(Protocol) ->
    case Protocol of
        <<"tcp">> -> "tcp";
        <<"udp">> -> "udp";
        <<"icmp">> -> "icmp";
        _ -> error
    end.

-spec parse_ports([any()]) -> [[any()]]. 
parse_ports(Ports) ->
    M = lists:map(fun(Port) ->  [case X of $- -> $:; _ -> X end || X <- binary_to_list(Port) ] end, Ports),
    M.

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
            lager:info("service ~p not deleted, in profiles: ~p~n",[Id,Profiles]),
            {error,#{<<"errors">> => #{<<"in active profile">> => Profiles}}}
     end.

-spec update(Id :: binary(), UpdateMap :: map()) -> {atom(), any()} .
update(Id, UpdateMap) ->
    case get_by_id(Id) of
        {ok, OldService} ->
            NewService = maps:merge(OldService,UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE,NewService) of
                ok ->
                    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
                    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
                    {ok, R} = dog_rethink:run(
                          fun(X) -> 
                                  reql:db(X, dog),
                                  reql:table(X, ?TYPE_TABLE),
                                  reql:get(X, Id),
                                  reql:update(X,UpdateMap)
                          end),
                    lager:debug("update R: ~p~n", [R]),
                    Replaced = maps:get(<<"replaced">>, R),
                    Unchanged = maps:get(<<"unchanged">>, R),
                    case {Replaced,Unchanged} of
                        {1,0} -> {true,Id};
                        {0,1} -> {false,Id};
                        _ -> {false, no_update}
                    end;
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {error, Error} ->
            {false, Error}
    end.            


-spec create(Group :: map()) -> {ok | error, Key :: iolist() | name_exists }.
create(ServiceMap@0) ->
    Name = maps:get(<<"name">>, ServiceMap@0),
    {ok, ExistingServices} = get_all(),
    ExistingNames = [maps:get(<<"name">>,Service) || Service <- ExistingServices],
    case lists:member(Name, ExistingNames) of
        false -> 
            case dog_json_schema:validate(?VALIDATION_TYPE,ServiceMap@0) of
                ok ->
                    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
                    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
                    {ok, R} = dog_rethink:run(
                          fun(X) -> 
                                  reql:db(X, dog),
                                  reql:table(X, ?TYPE_TABLE),
                                  reql:insert(X, ServiceMap@0)
                          end),
                    Key = hd(maps:get(<<"generated_keys">>,R)),
                    lager:debug("create R: ~p~n", [R]),
                    {ok, Key};
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        true ->
            {error, name_exists}
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
    Services = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    {ok, Services}.

-spec get_all_grouped_by_id() -> map().
get_all_grouped_by_id() ->
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, service)
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Services = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    maps:from_list([{maps:get(<<"id">>,Service), Service} || Service <- Services]).

-spec get_all_grouped_by_name() -> map().
get_all_grouped_by_name() ->
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, service)
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Services = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    maps:from_list([{maps:get(<<"name">>,Service), Service} || Service <- Services]).

-spec where_used(ServiceId :: binary()) -> {ok, list()}.
where_used(ServiceId) ->
    {ok, Profiles} = dog_profile:get_all(),
    ProfileServices = lists:map(fun(Profile) -> 
        ProfileId = maps:get(<<"id">>,Profile),
        Services = get_all_in_profile(ProfileId),
        lager:debug("Services: ~p",[Services]),
        {ProfileId, Services}
              end, Profiles),
    FilteredProfiles = lists:filtermap(fun({_Id, Services}) -> 
                                               lists:member(ServiceId,Services) 
                                       end, ProfileServices),
    {ok, [erlang:element(1,P) || P <- FilteredProfiles]}.

get_all_in_profile(ProfileId) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    R = dog_rethink:run(
      fun(X) -> 
        reql:db(X, dog), 
        reql:table(X, profile),
        reql:get(X, ProfileId),
        reql:pluck(X, [ #{<<"rules">> => #{<<"inbound">> => <<"service">>, <<"outbound">> => <<"service">>}} ])
    end),
    case R of
      {error, _} -> 
            {error, notfound};
      {ok, Result} ->
            Inbound = maps:get(<<"inbound">>, maps:get(<<"rules">>, Result)),
            InboundList = lists:map(fun(X) -> maps:get(<<"service">>,X) end, Inbound ),
            Outbound = maps:get(<<"outbound">>, maps:get(<<"rules">>, Result)),
            OutboundList = lists:map(fun(X) -> maps:get(<<"service">>,X) end, Outbound),
            ServiceList = dog_ips:uniq(InboundList ++ OutboundList),
            ServiceList
    end. 

any_service() ->
    #{
       <<"id">> => <<"any">>,
       <<"name">> => <<"any">>,
       <<"services">> => [
                        #{
                             <<"ports">> => [ <<"0:65535">> ],
                             <<"protocol">> => <<"any">>
                        }
                       ]
    }.

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
