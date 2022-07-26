-module(dog_db_pool_sup).
-behaviour(supervisor).

-include("dog_trainer.hrl").

%% API
-export([start_link/0]).
-export([run/2, run/3]).

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
init([]) ->
    {ok, Pools} = application:get_env(dog_trainer, pools),
    ?LOG_INFO("Pools: ~p",[Pools]),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
                                  PoolArgs = [{name, {local, Name}},
                                              {worker_module, dog_poolboy_worker}] ++ SizeArgs,
                                  poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                          end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

run(PoolName, Fun) ->
    poolboy:transaction(PoolName, fun(Worker) ->
                                          gen_server:call(Worker, {run, Fun})
                                  end).
run(PoolName, Fun, Timeout) ->
    poolboy:transaction(PoolName, fun(Worker) ->
                                          gen_server:call(Worker, {run, Fun, Timeout})
                                  end).
