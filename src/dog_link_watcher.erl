-module(dog_link_watcher).
-behaviour(gen_requery).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-include("dog_trainer.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         state/1
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

% These are the callback function specific to the gen_requery behavior
-export([handle_connection_up/2,
         handle_connection_down/1,
         handle_query_result/2,
         handle_query_done/1,
         handle_query_error/2]).

%% ------------------------------------------------------------------
%% test Function Exports
%% ------------------------------------------------------------------


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    {ok, _Ref} = gen_requery:start_link(?MODULE, [], []).

state(Ref) ->
    gen_requery:call(Ref, state, infinity).

init([]) ->
    lager:info("init"),
    % The ConnectOptions are provided to gen_rethink:connect_unlinked
    RethinkdbHost = application:get_env(dog_trainer, rethinkdb_host,"localhost"),
    RethinkdbPort = application:get_env(dog_trainer, rethinkdb_port,28015),
    RethinkdbUser = application:get_env(dog_trainer, rethinkdb_username,"admin"),
    RethinkdbPassword = application:get_env(dog_trainer, rethinkdb_password,""),
    RethinkTimeoutMs = application:get_env(dog_trainer, rethink_timeout_ms,1000),
    ConnectOptions = #{host => RethinkdbHost,
			port => RethinkdbPort,
			timeout => RethinkTimeoutMs,
                        user => binary:list_to_bin(RethinkdbUser),
                        password => binary:list_to_bin(RethinkdbPassword)},

    % The State is up to you -- it can be any term
    State = [],

    {ok, ConnectOptions, State}.

%% @doc handle_connection_up/2 is called with a valid Connection pid whenever
%% the managed connection is newly established
handle_connection_up(Connection, State) ->
    {ok,RethinkSquashSec} = application:get_env(dog_trainer,rethink_squash_sec),
    lager:info("handle_connection_up"),
    lager:info("Connection: ~p", [Connection]),
    Reql = reql:db(<<"dog">>),
    reql:table(Reql, <<"link">>),
    reql:changes(Reql, #{<<"include_initial">> => false, <<"squash">> => RethinkSquashSec}),
    {noreply, Reql, State}.

%% @doc handle_connection_down/1 is called when the managed connection goes
%% down unexpectedly. The gen_requery implementation will then enter a
%% reconnect state with exponential backoffs. Your module can still process
%% requests during this time.
handle_connection_down(State) ->
    lager:info("handle_connection_down"),
    {noreply, State}.

handle_query_result(Result, State) ->
    lager:info("Result: ~p", [Result]),
    case Result of
        [] ->
            pass;
        _ ->
          lists:foreach(fun(Entry) ->
            lager:debug("Entry: ~p",[Entry]),
            NewVal = maps:get(<<"new_val">>,Entry,null),
            OldVal = maps:get(<<"old_val">>,Entry,null),
            NewState = case {OldVal, NewVal} of
              {null, NewVal} ->
                           #{env_name => maps:get(<<"name">>,NewVal),
                            new_enabled_state => maps:get(<<"enabled">>,NewVal),
                            new_direction_state => maps:get(<<"direction">>,NewVal),
                            old_enabled_state => false,
                            old_direction_state => <<"inbound">>}; %this should be ignored
              {OldVal, null} ->
                           #{env_name => maps:get(<<"name">>,OldVal),
                            new_enabled_state => false,
                            new_direction_state => <<"bidirectional">>,
                            old_enabled_state => maps:get(<<"enabled">>,OldVal),
                            old_direction_state => maps:get(<<"direction">>,OldVal)}; %this should be ignored
              {OldVal, NewVal} ->
                           #{env_name => maps:get(<<"name">>,NewVal),
                            new_enabled_state => maps:get(<<"enabled">>,NewVal),
                            new_direction_state => maps:get(<<"direction">>,NewVal),
                            old_enabled_state => maps:get(<<"enabled">>,OldVal),
                            old_direction_state => maps:get(<<"direction">>,OldVal) } %this should be ignored
                end,
              NewEnabledState = maps:get(new_enabled_state,NewState),
              OldEnabledState = maps:get(old_enabled_state,NewState),
              %EnvName = maps:get(env_name,NewState),
              lager:debug("{OldEnabledState,NewEnabledState}: ~p",[{OldEnabledState,NewEnabledState}]),
              dog_external_agent:set_link_state(NewState),
              imetrics:add_m(watcher,link_update)%,
              %case {OldEnabledState,NewEnabledState} of
              %  {false,true} ->
              %    dog_external:restart_external_broker_connection(EnvName);
              %  {true,false} ->
              %    dog_external:stop_external_broker_connection(EnvName);
              %  {_,_} ->
              %    dog_external:restart_external_broker_connection(EnvName)
              %end
          end, Result)
    end,
    dog_ipset:update_ipsets(all_envs), %TODO
    {noreply, [Result|State]}.

handle_query_done(State) ->
    {stop, changefeeds_shouldnt_be_done, State}.

handle_query_error(Error, State) ->
    {stop, Error, State}.

handle_call(state, _From, State) ->
    lager:debug("handle_call changefeed: ~p",[State]),
    {reply, State, State}.

handle_cast(_Msg, State) ->
    lager:debug("handle_cast changefeed: ~p",[State]),
    {noreply, State}.

handle_info(_Info, State) ->
    lager:debug("handle_info changefeed: ~p",[State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
