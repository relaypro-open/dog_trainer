-module(dog_group_watcher).
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
    reql:table(Reql, <<"group">>),
    %reql:get_field(Reql, <<"profile">>),
    reql:pluck(Reql, [<<"name">>,<<"profile_id">>,<<"profile_name">>,<<"profile_version">>,<<"external_ipv4_addresses">>,<<"external_ipv6_addresses">>,<<"ec2_security_group_ids">>]),
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
                GroupName = case maps:get(<<"new_val">>,Entry) of
                    null ->
                        maps:get(<<"name">>,maps:get(<<"old_val">>,Entry));
                    _ ->
                        maps:get(<<"name">>,maps:get(<<"new_val">>,Entry))
                end,
                imetrics:add_m(watcher,group_update),
                GroupType = <<"role">>,
                lager:info("calling update_group_iptables: ~p",[GroupName]),
                dog_iptables:update_group_iptables(GroupName, GroupType),
                dog_iptables:update_group_ec2_sgs(GroupName),
                {ok, R4IpsetsRuleset} = dog_ruleset:read_ruleset_set_v4_from_file(GroupName),
                {ok, R6IpsetsRuleset} = dog_ruleset:read_ruleset_set_v6_from_file(GroupName),
                {ok, R4IptablesRuleset} = dog_ruleset:read_ruleset_unset_v4_from_file(GroupName),
                {ok, R6IptablesRuleset} = dog_ruleset:read_ruleset_unset_v6_from_file(GroupName),
                Hash4Ipsets = dog_profile:create_hash(R4IpsetsRuleset),
                Hash6Ipsets = dog_profile:create_hash(R6IpsetsRuleset),
                Hash4Iptables = dog_profile:create_hash(R4IptablesRuleset),
                Hash6Iptables = dog_profile:create_hash(R6IptablesRuleset),
                {ok, _} = dog_group:set_hash4_ipsets(GroupName, Hash4Ipsets),
                {ok, _} = dog_group:set_hash6_ipsets(GroupName, Hash6Ipsets),
                {ok, _} = dog_group:set_hash4_iptables(GroupName, Hash4Iptables),
                {ok, _} = dog_group:set_hash6_iptables(GroupName, Hash6Iptables)
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
