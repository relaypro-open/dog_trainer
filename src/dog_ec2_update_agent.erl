-module(dog_ec2_update_agent).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-include("dog_trainer.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         add_to_queue/1,
         ec2_security_group/2,
         ec2_security_group_ids/1,
         ec2_security_groups/1,
         periodic_publish/0,
         queue_length/0,
         start_link/0
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2
        ]).

%% ------------------------------------------------------------------
%% test Function Exports
%% ------------------------------------------------------------------


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec add_to_queue(Groups :: list()) -> OldServer :: ok.
add_to_queue(Groups) ->
    gen_server:cast(?MODULE, {add_to_queue, Groups}).

queue_length() ->
    gen_server:call(?MODULE, queue_length, 10000).

periodic_publish() ->
  gen_server:call(?MODULE, periodic_publish,20000).
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
    Ec2SgCacheSeconds = application:get_env(dog_trainer,ec2_sg_cache_seconds,60),
    cache_tab:new(ec2_sgs, [{life_time, Ec2SgCacheSeconds}]),
    PeriodicPublishInterval = application:get_env(dog_trainer,ec2_periodic_publish_interval_seconds,5),
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
-spec handle_call(term(), {pid(), term()}, State::ips_state()) -> {reply, ok, any()}.
handle_call({periodic_publish}, _From, State) ->
  {ok, NewState} = do_periodic_publish(State),
  {reply, ok, NewState};
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
-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({add_to_queue, Groups}, State) ->
  NewState = ordsets:union(ordsets:from_list(Groups), State),  
  {noreply, NewState};
handle_cast(Msg, State) ->
  lager:error("unknown_message: Msg: ~p, State: ~p",[Msg, State]),
  {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
% TODO: be more specific about Info in spec
-spec handle_info(_,_) -> {'noreply',_}.
handle_info(periodic_publish, State) ->
    {ok, NewState} = do_periodic_publish(State),
    PeriodicPublishInterval = application:get_env(dog_trainer,ec2_periodic_publish_interval_seconds,5),
    erlang:send_after(PeriodicPublishInterval * 1000, self(), periodic_publish),
    {noreply, NewState};
handle_info(Info, State) ->
  lager:error("unknown_message: Info: ~p, State: ~p",[Info, State]),
  {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
-spec terminate(_, ips_state()) -> {close}.
terminate(Reason, State) ->
    lager:info("terminate: Reason: ~p, State: ~p", [Reason, State]),
    {close}.

-spec code_change(_, State::ips_state(), _) -> {ok, State::ips_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec get_ec2_security_groups(Region :: binary()) -> {ok, Ec2Sgs :: map()}.
get_ec2_security_groups(Region) ->
    Config = dog_ec2_sg:config(Region),
    {ok, Ec2Sgs} = erlcloud_ec2:describe_security_groups([],[],[],Config),
    ListOfMaps = [maps:from_list(X) || X <- Ec2Sgs],
    Ec2SgsMap = dog_common:list_of_maps_to_map(ListOfMaps,group_id),
    {ok, Ec2SgsMap}.

-spec ec2_security_groups(Region :: binary()) -> Ec2Sgs :: list().
ec2_security_groups(Region) ->
    {ok, Ec2Sgs} = cache_tab:lookup(ec2_sgs, Region, fun() -> get_ec2_security_groups(Region) end),
    Ec2Sgs.

-spec ec2_security_group(Ec2SecurityGroupId :: binary(), Region :: binary()) -> {ok, Ec2Sgs :: list()}.
ec2_security_group(Ec2SecurityGroupId, Region) ->
    Ec2SgsMap = ec2_security_groups(Region),
    Ec2Sg = maps:get(binary:bin_to_list(Ec2SecurityGroupId), Ec2SgsMap,{error,notfound}),
    Ec2Sg.

-spec ec2_security_group_ids(Region :: binary()) -> SgIds :: list().
ec2_security_group_ids(Region) ->
    Ec2Sgs = ec2_security_groups(Region),
    SgIds = maps:keys(Ec2Sgs),
    SgIds.

-spec do_periodic_publish(_) -> OldServers :: {ok, list()}.
do_periodic_publish([]) ->
    imetrics:set_gauge(publish_queue_length, 0),
    {ok, []};
do_periodic_publish(State) ->
    case dog_agent_checker:check() of
        true ->
            lager:info("State: ~p",[State]),
            Sgs = ordsets:to_list(State),
            imetrics:set_gauge(publish_queue_length, length(Sgs)),
            PeriodicPublishMax = application:get_env(dog_trainer,ec2_periodic_publish_max,10),
            {_ConsumedSgs, LeftoverSgs} = case Sgs of
                [] ->
                    {[],[]};
                _ when length(Sgs) >= PeriodicPublishMax ->
                    lists:split(PeriodicPublishMax,Sgs); %only consume N per run
                _ when length(Sgs) < PeriodicPublishMax ->
                    {Sgs,[]}
            end,
            SgsWithoutEmptyProfiles = ordsets:subtract(ordsets:from_list(Sgs),[<<>>]),
            PublishList = lists:map(fun({DogGroup, Region, SgId}) -> 
                dog_ec2_sg:publish_ec2_sg({DogGroup, Region, SgId})
                          end, SgsWithoutEmptyProfiles),
            lager:info("PublishList: ~p",[PublishList]),
            {ok, ordsets:from_list(LeftoverSgs) };
        false ->
            lager:info("Skipping, dog_agent_checker:check() false"),
            {ok, State }
    end.
