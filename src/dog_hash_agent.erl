-module(dog_hash_agent).
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
    _KeepaliveTimer = erlang:send_after(KeepAliveAlertSeconds * 1 * 1000, self(), watch_hashes),
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
handle_info(watch_hashes, State) ->
    ok = do_watch_hashes(State),
    {ok, PollingIntervalSeconds} = application:get_env(dog_trainer,polling_interval_seconds),
    erlang:send_after(PollingIntervalSeconds * 1000, self(), watch_hashes),
    {noreply, []};
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

%% @doc Makes a call to the nif to add a resource to
%% watch. Logs on error
-spec do_watch_hashes(_) -> ok.
do_watch_hashes(_State) ->
    case dog_agent_checker:check() of
        true ->
            AlertedHosts = dog_host:all_hash_alerted(),
            lager:info("AlertedHosts: ~p",[AlertedHosts]),
            {ok, SuccessChecks, FailedChecks} = dog_host:hash_check(),
            {ok,HostsFailedHashCheck} = dog_host:hash_age_check(),
            FailedHosts = [ maps:get(<<"name">>,H) || H <- HostsFailedHashCheck],
            lager:info("FailedHosts: ~p",[FailedHosts]),
            NewAlertHosts = ordsets:subtract(lists:sort(FailedHosts),lists:sort(AlertedHosts)),
            lager:info("NewAlertHosts: ~p",[NewAlertHosts]),
            case NewAlertHosts of
                [] ->
                    pass;
                _ ->
                    dog_host:send_hash_alert(ordsets:to_list(NewAlertHosts),FailedChecks),
                    lists:foreach(fun(HostName) -> dog_host:update_hash_alert_sent(HostName,true) end, NewAlertHosts)
            end,
            NewRecoverHosts = ordsets:subtract(lists:sort(AlertedHosts),lists:sort(FailedHosts)),
            lager:info("NewRecoverHosts: ~p",[NewRecoverHosts]),
            case NewRecoverHosts of
                [] ->
                    pass;
                _ ->
                    dog_host:send_hash_recover(ordsets:to_list(NewRecoverHosts),SuccessChecks),
                    lists:foreach(fun(HostName) -> dog_host:update_hash_alert_sent(HostName,false) end, NewRecoverHosts)
            end,
            ok;
        false ->
            lager:info("Skipping, dog_agent_checker:check() false"),
            ok
    end.
