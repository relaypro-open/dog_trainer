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
    ec2_classic_security_group/2,
    ec2_classic_security_group_ids/1,
    ec2_classic_security_groups/1,
    ec2_security_group/2,
    ec2_security_group_ids/1,
    ec2_security_groups/1,
    get_ec2_security_groups/1,
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
handle_cast({add_to_queue, Groups}, State) ->
    NewState = ordsets:union(ordsets:from_list(Groups), State),
    {noreply, NewState};
handle_cast(Msg, State) ->
    ?LOG_ERROR(#{msg => Msg, state => State}, #{domain => [dog_trainer]}),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
% TODO: be more specific about Info in spec
-spec handle_info(_, _) -> {'noreply', _}.
handle_info(Info, State) ->
    ?LOG_ERROR(#{info => Info, state => State}, #{domain => [dog_trainer]}),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
-spec terminate(_, ips_state()) -> {close}.
terminate(Reason, State) ->
    ?LOG_INFO(#{reason => Reason, state => State}, #{domain => [dog_trainer]}),
    {close}.

-spec code_change(_, State :: ips_state(), _) -> {ok, State :: ips_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec get_ec2_security_groups(Region :: undefined | string()) -> {ok, list()} | {error, map()}.
get_ec2_security_groups(Region) ->
    Config = dog_ec2_sg:config(Region),
    Result = erlcloud_ec2:describe_security_groups([], [], [], Config),
    ?LOG_DEBUG(#{result => Result}, #{domain => [dog_trainer]}),
    case Result of
        {ok, R} ->
            {ok, R};
        _ ->
            {error, #{}}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec ec2_security_groups(Region :: undefined | string()) -> map().
ec2_security_groups(Region) ->
    case get_ec2_security_groups(Region) of
        {ok, Ec2Sgs} ->
            ListOfMaps = [maps:from_list(X) || X <- Ec2Sgs, proplists:get_value(vpc_id, X) =/= []],
            Ec2SgsMap = dog_common:list_of_maps_to_map(ListOfMaps, group_id),
            Ec2SgsMap;
        _ ->
            #{}
    end.

-spec ec2_security_group(Ec2SecurityGroupId :: binary(), Region :: undefined | string()) ->
    any().
ec2_security_group(Ec2SecurityGroupId, Region) ->
    Ec2SgsMap = ec2_security_groups(Region),
    Ec2Sg = maps:get(binary:bin_to_list(Ec2SecurityGroupId), Ec2SgsMap, {error, notfound}),
    Ec2Sg.

-spec ec2_security_group_ids(Region :: undefined | string()) -> SgIds :: list().
ec2_security_group_ids(Region) ->
    Ec2Sgs = ec2_security_groups(Region),
    SgIds = maps:keys(Ec2Sgs),
    SgIds.

-spec ec2_classic_security_groups(Region :: undefined | string()) -> map().
ec2_classic_security_groups(Region) ->
    case get_ec2_security_groups(Region) of
        {ok, Ec2Sgs} ->
            ListOfMaps = [maps:from_list(X) || X <- Ec2Sgs, proplists:get_value(vpc_id, X) == []],
            Ec2SgsMap = dog_common:list_of_maps_to_map(ListOfMaps, group_id),
            Ec2SgsMap;
        _ ->
            #{}
    end.

-spec ec2_classic_security_group(Ec2SecurityGroupId :: binary(), Region :: undefined | string()) ->
    any().
ec2_classic_security_group(Ec2SecurityGroupId, Region) ->
    Ec2SgsMap = ec2_classic_security_groups(Region),
    Ec2Sg = maps:get(binary:bin_to_list(Ec2SecurityGroupId), Ec2SgsMap, {error, notfound}),
    Ec2Sg.

-spec ec2_classic_security_group_ids(Region :: undefined | string()) -> SgIds :: list().
ec2_classic_security_group_ids(Region) ->
    Ec2Sgs = ec2_classic_security_groups(Region),
    SgIds = maps:keys(Ec2Sgs),
    SgIds.
