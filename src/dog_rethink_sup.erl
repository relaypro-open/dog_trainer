%%%-------------------------------------------------------------------
%% @doc dog top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dog_rethink_sup).

-behaviour(supervisor).

-include("dog_trainer.hrl").

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
    application:load(dog_trainer),
    {ok, RethinkdbHost} = application:get_env(dog_trainer, rethinkdb_host),
    {ok, RethinkdbPort} = application:get_env(dog_trainer, rethinkdb_port),
    {ok, RethinkdbUser} = application:get_env(dog_trainer, rethinkdb_username),
    {ok, RethinkdbPassword} = application:get_env(dog_trainer, rethinkdb_password),
    {ok, RethinkTimeoutMs} = application:get_env(dog_trainer, rethink_timeout_ms),
    ?LOG_ERROR("RethinkdbHost: ~p,RethinkdbPort: ~p,RethinkdbUser: ~p,RethinkdbPassword: ~p",[RethinkdbHost,RethinkdbPort,RethinkdbUser,RethinkdbPassword]),
    DbSetupResult = rethink_db_setup:setup_rethinkdb(
        RethinkdbHost, RethinkdbPort, RethinkdbUser, RethinkdbPassword
    ),
    ?LOG_INFO("RethinkDB setup: ~p~n", [DbSetupResult]),
    ConnectOptions = #{
        host => RethinkdbHost,
        port => RethinkdbPort,
        timeout => RethinkTimeoutMs,
        user => binary:list_to_bin(RethinkdbUser),
        password => binary:list_to_bin(RethinkdbPassword)
    },
    {ok,
        {#{strategy => one_for_one, intensity => 100, period => 3600}, [
            #{
                id => {gen_rethink_session, dog_session},
                start => {gen_rethink_session, start_link, [dog_session, ConnectOptions]}
            }
        ]}}.

%%====================================================================
%% Internal functions============================================
%%====================================================================
