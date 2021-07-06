-module(dog_agent_checker).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-include("dog_trainer.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         check/0
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% ------------------------------------------------------------------
%% test Function Exports
%% ------------------------------------------------------------------
-export([go/0,
         periodic_reset/0,
         stop/0,
         value/0]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec check() -> boolean().
check() ->
    gen_server:call(?MODULE, check).

-spec periodic_reset() -> ok.
periodic_reset() ->
    gen_server:call(?MODULE, periodic_reset).

-spec go() -> ok.
go() ->
    gen_server:call(?MODULE, go).
%% ------------------------------------------------------------------
%% test functions definitions
%% ------------------------------------------------------------------

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).

-spec value() -> ok.
value() ->
    gen_server:call(?MODULE, value).

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
    LastAgentUpdateCheckIntervalSeconds = application:get_env(dog_trainer,last_agent_update_check_interval_seconds,60),
    _PublishTimer = erlang:send_after(LastAgentUpdateCheckIntervalSeconds * 1000, self(), periodic_reset),
    State = 0,
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
-spec handle_call(term(), {pid(), term()}, boolean()) -> {reply, ok, boolean()}.
handle_call(check, _from, State) ->
    MaxIntervalSinceLastAgentUpdate = application:get_env(dog_trainer,max_interval_since_last_agent_update,2),
    case State of
        _ when State >= MaxIntervalSinceLastAgentUpdate  -> 
            imetrics:set_gauge(no_agent_updates_received, 1),
            {reply, false, State};
        _ when State < MaxIntervalSinceLastAgentUpdate -> 
            imetrics:set_gauge(no_agent_updates_received, 0),
            {reply, true, State}
    end;
handle_call(go, _from, _State) ->
    NewState = 0,
    imetrics:set_gauge(interval_since_last_agent_update, NewState),
    {reply, ok, NewState};
handle_call(stop, _from, _State) ->
    NewState = application:get_env(dog_trainer,max_interval_since_last_agent_update,2),
    imetrics:set_gauge(interval_since_last_agent_update, NewState),
    {reply, ok, NewState};
handle_call(value, _from, State) ->
    {reply, State, State};
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
handle_info(periodic_reset, State) ->
    LastAgentUpdateCheckIntervalSeconds = application:get_env(dog_trainer,last_agent_update_check_interval_seconds,60),
    erlang:send_after(LastAgentUpdateCheckIntervalSeconds * 1000, self(), periodic_reset),
    NewState = State + 1,
    imetrics:set_gauge(interval_since_last_agent_update, NewState),
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

%% @doc Makes a call to the nif to add a resource to
%% watch. Logs on error
