-module(dog_keepalive_agent).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-include("dog_trainer.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0
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
-export([do_watch_keepalives/1]).


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
    {ok, KeepAliveAlertSeconds} = application:get_env(dog_trainer,keepalive_alert_seconds),
    _KeepaliveTimer = erlang:send_after(KeepAliveAlertSeconds * 1 * 1000, self(), watch_keepalives),
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
handle_cast(Msg, State) ->
  ?LOG_ERROR("unknown_message: Msg: ~p, State: ~p",[Msg, State]),
  {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
% TODO: be more specific about Info in spec
-spec handle_info(_,_) -> {'noreply',_}.
handle_info(watch_keepalives, State) ->
    ok = do_watch_keepalives(State),
    {ok, PollingIntervalSeconds} = application:get_env(dog_trainer,polling_interval_seconds),
    erlang:send_after(PollingIntervalSeconds * 1000, self(), watch_keepalives),
    {noreply, []};
handle_info(Info, State) ->
  ?LOG_ERROR("unknown_message: Info: ~p, State: ~p",[Info, State]),
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

-spec code_change(_, State::ips_state(), _) -> {ok, State::ips_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @doc Makes a call to the nif to add a resource to
%% watch. Logs on error
-spec do_watch_keepalives(_) -> ok | pass.
do_watch_keepalives(_State) ->
    case dog_agent_checker:check() of
        true ->
            {ok,HostsRetirementCheck} = dog_host:retirement_check(),
            RetiredHostIds = [ maps:get(<<"id">>,H) || H <- HostsRetirementCheck],
            ?LOG_DEBUG("RetiredHostIds: ~p",[RetiredHostIds]),
            case RetiredHostIds of
                [] ->
                    imetrics:set_gauge_m(<<"host_keepalive">>,<<"retirement">>,0),
                    ok;
                _ ->
                    imetrics:set_gauge_m(<<"host_keepalive">>,<<"retirement">>,length(RetiredHostIds)),
                    lists:foreach(fun(HostId) -> 
                                          {ok,Host} = dog_host:get_by_id(HostId),
                                          dog_host:state_event(Host,retirement_timeout,[]) end, RetiredHostIds)
            end,

            {ok,HostsFailedKeepaliveCheck} = dog_host:keepalive_age_check(),
            HostsFailedKeepaliveCheckIds = [ maps:get(<<"id">>,H) || H <- HostsFailedKeepaliveCheck],
            case HostsFailedKeepaliveCheckIds of
                [] ->
                    imetrics:set_gauge_m(<<"host_keepalive">>,<<"inactive">>,0),
                    ok;
                _ ->
                    imetrics:set_gauge_m(<<"host_keepalive">>,<<"inactive">>,length(HostsFailedKeepaliveCheckIds)),
                    lists:foreach(fun(HostId) -> 
                                          {ok,Host} = dog_host:get_by_id(HostId),
                                          dog_host:state_event(Host,keepalive_timeout,[]) end, HostsFailedKeepaliveCheckIds)
            end,

            ok;
        false ->
            imetrics:set_gauge_m(<<"host_keepalive">>,<<"retirement">>,0),
            imetrics:set_gauge_m(<<"host_keepalive">>,<<"inactive">>,0),
            ?LOG_INFO("Skipping, dog_agent_checker:check() false"),
            ok
    end.
