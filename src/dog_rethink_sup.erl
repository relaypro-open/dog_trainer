%%%-------------------------------------------------------------------
%% @doc dog top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dog_rethink_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RethinkdbHost = application:get_env(dog_trainer, rethinkdb_host,"localhost"),
    RethinkdbPort = application:get_env(dog_trainer, rethinkdb_port,28015),
    RethinkdbUser = application:get_env(dog_trainer, rethinkdb_username,"admin"),
    RethinkdbPassword = application:get_env(dog_trainer, rethinkdb_password,""),
    RethinkTimeoutMs = application:get_env(dog_trainer, rethink_timeout_ms,1000),
    %lager:error("RethinkdbHost: ~p,RethinkdbPort: ~p,RethinkdbUser: ~p,RethinkdbPassword: ~p",[RethinkdbHost,RethinkdbPort,RethinkdbUser,RethinkdbPassword]),
    DbSetupResult = rethink_db_setup:setup_rethinkdb(RethinkdbHost,RethinkdbPort,RethinkdbUser,RethinkdbPassword),
    lager:info("RethinkDB setup: ~p~n",[DbSetupResult]),
    ConnectOptions = #{host => RethinkdbHost,
                       port => RethinkdbPort,
                       timeout => RethinkTimeoutMs,
                       user => binary:list_to_bin(RethinkdbUser),
                       password => binary:list_to_bin(RethinkdbPassword)},
    {ok, {#{strategy => one_for_one, intensity => 100, period => 3600}, 
          [#{id => {gen_rethink_session, dog_session},
             start => {gen_rethink_session, start_link, [dog_session, ConnectOptions]}}]}}.

%%====================================================================
%% Internal functions============================================
%%====================================================================
