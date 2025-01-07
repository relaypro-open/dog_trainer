-module(dog_file_transfer_pool_sup).
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
init([]) ->
    {ok, Pools} = application:get_env(dog_trainer, file_transfer_pools),
    ?LOGT_INFO("Pools: ~p", [{pools,Pools}]),
    PoolSpecs = lists:map(
        fun({Name, SizeArgs, WorkerArgs}) ->
            PoolArgs =
                [
                    {name, {local, Name}},
                    {worker_module, dog_file_transfer_worker}
                ] ++ SizeArgs,
            poolboy:child_spec(Name, PoolArgs, WorkerArgs)
        end,
        Pools
    ),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
