-module(dog_ipset_update_agent).
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
    start_link/0,
    flush_queue/0,
    queue_length/0,
    queue_add/1,
    queue_add_force/1,
    periodic_publish/0
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
    ?LOG_INFO(#{message => "function"}, #{domain => [dog_trainer]}),
    gen_server:call(?MODULE, periodic_publish).

-spec queue_add_force(Source :: iolist()) -> ok.
queue_add_force(Source) ->
    imetrics:add_m(ipset_queue_add_force, Source),
    gen_server:cast(?MODULE, {add_to_queue, [force]}).

-spec queue_add(Source :: iolist()) -> ok.
queue_add(Source) ->
    imetrics:add_m(ipset_queue_add, Source),
    gen_server:cast(?MODULE, {add_to_queue, [Source]}).

-spec flush_queue() -> ok.
flush_queue() ->
    gen_server:cast(?MODULE, flush_queue).

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
    PeriodicPublishInterval = application:get_env(
        dog_trainer, ipset_periodic_publish_interval_seconds, 5
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
handle_cast({add_to_queue, Sources}, State) ->
    NewState = ordsets:union(ordsets:from_list(Sources), State),
    {noreply, NewState};
handle_cast(flush_queue, _State) ->
    NewState = {ok, []},
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
handle_info(periodic_publish, State) ->
    {ok, NewState} = do_periodic_publish(State),
    {ok, PeriodicPublishInterval} = application:get_env(
        dog_trainer, ipset_periodic_publish_interval_seconds
    ),
    erlang:send_after(PeriodicPublishInterval * 1000, self(), periodic_publish),
    {noreply, NewState};
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

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec do_periodic_publish(_) -> OldServers :: {ok, list()}.
do_periodic_publish(State) ->
    ?LOG_INFO(#{message => "do_periodic_publish"}, #{domain => [dog_trainer]}),
    case dog_agent_checker:check() of
        true ->
            case State of
                [] ->
                    {ok, []};
                _ ->
                    ?LOG_INFO(#{state => State}, #{domain => [dog_trainer]}),
                    ?LOG_INFO(#{state => length(State)}, #{domain => [dog_trainer]}),
                    case lists:member(force,State) of
                      true ->
                        ?LOG_DEBUG(#{message => "force ipset update"}, #{domain => [dog_trainer]}),
                        dog_ipset:force_update_ipsets();
                      false ->
                        dog_ipset:update_ipsets()
                    end,
                    {ok, []}
            end;
        false ->
            ?LOG_INFO(#{message => "Skipping, dog_agent_checker:check() false"}, #{domain => [dog_trainer]}),
            {ok, State}
    end.
