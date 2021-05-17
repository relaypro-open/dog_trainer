-module(dog_profile_update_agent).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-include("dog_trainer.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         add_to_queue/1,
         periodic_publish/0,
         queue_length/0
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

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
    {ok, PeriodicPublishInterval} = application:get_env(dog_trainer,profile_periodic_publish_interval_seconds),
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
    {ok, PeriodicPublishInterval} = application:get_env(dog_trainer,profile_periodic_publish_interval_seconds),
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

-spec do_periodic_publish(_) -> OldServers :: {ok, list()}.
do_periodic_publish([]) ->
    imetrics:set_gauge(publish_queue_length, 0),
    {ok, []};
do_periodic_publish(State) ->
    case dog_agent_checker:check() of
        true ->
            lager:info("State: ~p",[State]),
            Groups = ordsets:to_list(State),
            imetrics:set_gauge(publish_queue_length, length(Groups)),
            PeriodicPublishMax = application:get_env(dog_trainer,periodic_publish_max,10),
            {_ConsumedGroups, LeftoverGroups} = case Groups of
                [] ->
                    {[],[]};
                _ when length(Groups) >= PeriodicPublishMax ->
                    lists:split(PeriodicPublishMax,Groups); %only consume N per run
                _ when length(Groups) < PeriodicPublishMax ->
                    {Groups,[]}
            end,
            {Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap} = dog_ipset:id_maps(),
            {MergedIpsets, _InternalIpsets} = dog_ipset:create_ipsets(),
            %Publish ipsets even if the Group doesn't have an associated Profile:
            %NonBlankGroups = lists:filter(fun(Group) -> Group =/= <<>> end, Groups),
            %lager:info("NonBlankGroups: ~p",[NonBlankGroups]),
            %case length(NonBlankGroups) > 0 of
            %  true ->
            %    dog_ipset:update_ipsets(all_envs);
            %  false ->
            %    dog_ipset:update_ipsets(local_env)
            %end,
            dog_ipset:update_ipsets(all_envs),
            EmptyIpsets = [], % Deliberately set to empty set, so agent will not update ipsets.
            GroupsWithoutEmptyProfiles = ordsets:subtract(ordsets:from_list(Groups),[<<>>]),
            PublishList = lists:map(fun(Group) -> 
                Environment = <<"*">>,
                Location = <<"*">>,
                HostKey = <<"*">>,
                RoutingKey = binary:list_to_bin([Environment,<<".">>,Location,<<".">>,Group,<<".">>,HostKey]),
                try 
                    case dog_profile:create_ruleset(RoutingKey, Group, Ipv4RoleMap,Ipv6RoleMap,Ipv4ZoneMap,
                                                    Ipv6ZoneMap,ZoneIdMap,GroupIdMap,ServiceIdMap,MergedIpsets) of
                      {R4IpsetsRuleset, R6IpsetsRuleset, R4IptablesRuleset, R6IptablesRuleset} ->
                        {Group, dog_iptables:publish_to_queue(RoutingKey, R4IpsetsRuleset, R6IpsetsRuleset, R4IptablesRuleset, R6IptablesRuleset, EmptyIpsets)};
                      error ->
                        {Group, error}
                    end
                catch 
                    profile_not_found ->
                        lager:info("profile_not_found in group: ~p",[Group]),
                        {Group, profile_not_found}
                end
                          end, GroupsWithoutEmptyProfiles),
            lager:info("PublishList: ~p",[PublishList]),
            {ok, ordsets:from_list(LeftoverGroups) };
        false ->
            lager:info("Skipping, dog_agent_checker:check() false"),
            {ok, State }
    end.
