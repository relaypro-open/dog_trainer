%%%-------------------------------------------------------------------
%% @doc dog_trainer top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dog_watcher_sup).

-include("dog_trainer.hrl"). 


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
    {ok, {{one_for_one, 100, 6000},
       [
       {profile_watcher,
            {dog_profile_watcher, start_link, []},
            permanent,
            5000,
            worker,
            [dog_profile_watcher]}
	,
       {service_watcher,
            {dog_service_watcher, start_link, []},
            permanent,
            5000,
            worker,
            [dog_service_watcher]}
	,
       {group_watcher,
            {dog_group_watcher, start_link, []},
            permanent,
            5000,
            worker,
            [dog_group_watcher]}
	,
       {zone_watcher,
            {dog_zone_watcher, start_link, []},
            permanent,
            5000,
            worker,
            [dog_zone_watcher]}
       ,
       {host_config_watcher,
            {dog_host_config_watcher, start_link, []},
            permanent,
            5000,
            worker,
            [dog_host_config_watcher]},
       {host_interfaces_watcher,
            {dog_host_interfaces_watcher, start_link, []},
            permanent,
            5000,
            worker,
            [dog_host_interfaces_watcher]},
       {host_active_watcher,
            {dog_host_active_watcher, start_link, []},
            permanent,
            5000,
            worker,
            [dog_host_active_watcher]},
       {link_watcher,
            {dog_link_watcher, start_link, []},
            permanent,
            5000,
            worker,
            [dog_link_watcher]}
	]	
	}}.

%%====================================================================
%% Internal functions
%%====================================================================
