-module(dog_external_update_agent).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-include("dog_trainer.hrl").
-include_lib("kernel/include/logger.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    periodic_publish/0,
    publish_to_external/1,
    publish_to_outbound_exchange/2,
    publish_to_outbound_exchanges/1,
    queue_length/0,
    queue_update/1,
    start_link/0
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% ------------------------------------------------------------------
%% test Function Exports
%% ------------------------------------------------------------------
%-export([do_periodic_publish/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec periodic_publish() -> OldServer :: ok.
periodic_publish() ->
    ?LOG_INFO("function"),
    gen_server:call(?MODULE, periodic_publish).

-spec queue_update(Ipsets :: list) -> ok.
queue_update(Ipsets) ->
    imetrics:add(external_queue_add),
    gen_server:cast(?MODULE, {add_to_queue, [Ipsets]}).

queue_length() ->
    gen_server:call(?MODULE, queue_length).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State} |
%%          {ok, State, Timeout} |
%%          ignore |
%%          {stop, Reason}
%%----------------------------------------------------------------------

-spec init(_) -> {'ok', []}.
init(_Args) ->
    %CurrentIpset = dog_ipset:read_current_ipset(),
    %{ok, CurrentIpsetHashes} = dog_ipset:get_hashes(),
    %dog_ipset:create(CurrentIpsetHashes),
    PeriodicPublishInterval = application:get_env(
        dog_trainer, external_ipset_periodic_publish_interval_seconds, 5
    ),
    _PublishTimer = erlang:send_after(PeriodicPublishInterval * 1000, self(), periodic_publish),
    State = ordsets:new(),
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, Reply, State} | (terminate/2 is called)
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, State :: ips_state()) -> {reply, ok, any()}.
handle_call(queue_length, _from, State) ->
    QueueLength = length(State),
    {reply, QueueLength, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
-spec handle_cast(_, _) -> {'noreply', _}.
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({add_to_queue, Ipsets}, State) ->
    NewState = Ipsets,
    {noreply, NewState};
handle_cast(Msg, State) ->
    ?LOG_ERROR("unknown_message: Msg: ~p, State: ~p", [Msg, State]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
% TODO: be more specific about Info in spec
-spec handle_info(_, _) -> {'noreply', _}.
handle_info(periodic_publish, State) ->
    {ok, NewState} = do_periodic_publish(State),
    PeriodicPublishInterval = application:get_env(
        dog_trainer, external_ipset_periodic_publish_interval_seconds, 5
    ),
    erlang:send_after(PeriodicPublishInterval * 1000, self(), periodic_publish),
    {noreply, NewState};
handle_info(Info, State) ->
    ?LOG_ERROR("unknown_message: Info: ~p, State: ~p", [Info, State]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
-spec terminate(_, ips_state()) -> {close}.
terminate(Reason, State) ->
    ?LOG_INFO("terminate: Reason: ~p, State: ~p", [Reason, State]),
    {close}.

-spec code_change(_, State :: ips_state(), _) -> {ok, State :: ips_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec do_periodic_publish(_) -> OldServers :: {ok, list()}.
do_periodic_publish(State) ->
    ?LOG_INFO("do_periodic_publish"),
    case State of
        [] ->
            {ok, []};
        _ ->
            ?LOG_INFO("ipset queue: ~p", [State]),
            ?LOG_INFO("length of ipset queue: ~p", [length(State)]),
            LatestInternalIpsets = lists:last(State),
            publish_to_external(LatestInternalIpsets),
            dog_ipset:update_ipsets(),
            {ok, []}
    end.

-spec publish_to_outbound_exchanges(IpsetExternalMap :: map()) -> any().
publish_to_outbound_exchanges(IpsetExternalMap) ->
    {ok, ExternalEnvs} = dog_link:get_all_active_outbound(),
    IdsByGroup = dog_group:get_all_internal_ec2_security_group_ids(),
    %dog_common:merge_maps_of_lists([IdsByGroupMap,AllActiveUnionEc2Sgs]).
    lists:foreach(
        fun(Env) ->
            EnvName = maps:get(<<"name">>, Env),
            ExternalMap = maps:put(<<"ec2">>, IdsByGroup, IpsetExternalMap),
            %ExternalMap = maps:put(<<"ec2">>,jsx:encode(#{}),IpsetExternalMap),
            ?LOG_DEBUG("ExternalMap: ~p~n", [ExternalMap]),
            publish_to_outbound_exchange(EnvName, ExternalMap)
        end,
        ExternalEnvs
    ).

-spec publish_to_outbound_exchange(TargetEnvName :: binary(), IpsetExternalMap :: map()) -> any().
publish_to_outbound_exchange(TargetEnvName, IpsetExternalMap) ->
    ?LOG_INFO("IpsetExternalMap: ~p", [IpsetExternalMap]),
    {ok, LocalEnvName} = application:get_env(dog_trainer, env),
    UserData = #{
        ipsets => jsx:encode(IpsetExternalMap),
        name => LocalEnvName
    },
    Count = 1,
    Pid = erlang:self(),
    Message = term_to_binary([
        {count, Count},
        {local_time, calendar:local_time()},
        {pid, Pid},
        {user_data, UserData}
    ]),
    RoutingKey = binary:list_to_bin(LocalEnvName),
    BrokerConfigName = list_to_atom(binary:bin_to_list(TargetEnvName)),
    %thumper:start_link(BrokerConfigName),
    ?LOG_INFO("~p, ~p, ~p, ~p", [BrokerConfigName, Message, <<"inbound">>, RoutingKey]),
    %Response = thumper:publish_to(BrokerConfigName, Message, <<"inbound">>, RoutingKey),
    PublisherName = erlang:binary_to_atom(<<TargetEnvName/binary, <<"_publisher">>/binary>>),
    Response = turtle:publish(
        PublisherName,
        <<"inbound">>,
        RoutingKey,
        <<"text/json">>,
        Message,
        #{delivery_mode => persistent}
    ),
    imetrics:add(ipset_outbound_publish),
    Response.

-spec publish_to_external(InternalIpsetsMap :: map()) -> any().
publish_to_external(InternalIpsetsMap) ->
    ?LOG_INFO("publishing to external"),
    publish_to_outbound_exchanges(InternalIpsetsMap).
