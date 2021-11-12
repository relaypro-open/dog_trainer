-module(dog_profile_watcher).
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

-spec init(_) -> no_return().
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
    dog_profile:init(),

    {ok, ConnectOptions, State}.

%% @doc handle_connection_up/2 is called with a valid Connection pid whenever
%% the managed connection is newly established
handle_connection_up(Connection, State) ->
    {ok,RethinkSquashSec} = application:get_env(dog_trainer,rethink_squash_sec),
    lager:info("handle_connection_up"),
    lager:info("Connection: ~p", [Connection]),
    Reql = reql:db(<<"dog">>),
    reql:table(Reql, <<"profile">>),
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
            imetrics:add_m(watcher,profile_update),
            lists:foreach(fun(Entry) ->
                lager:info("Entry: ~p", [Entry]),
                ProfileId = case maps:get(<<"new_val">>,Entry) of
                    null ->
                        maps:get(<<"id">>,maps:get(<<"old_val">>,Entry));
                    _ ->
                        maps:get(<<"id">>,maps:get(<<"new_val">>,Entry))
                end,
                timer:sleep(1000), %TODO Fix race condition
                GroupIds = dog_group:get_ids_with_profile_id(ProfileId),
                %{ok,GroupIds} = dog_profile:where_used(ProfileId),
                lager:info("GroupIds: ~p", [GroupIds]),
                lists:foreach(fun(GroupId) ->
                    {ok, GroupName} = dog_group:get_name_by_id(GroupId),
                    lager:info(GroupName),
                    GroupType = <<"role">>,
                    dog_iptables:update_group_iptables(GroupName, GroupType),
                    dog_ec2_sg:publish_ec2_sg_by_name(GroupName)
                end, GroupIds)
            end, Result)
    end,
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
