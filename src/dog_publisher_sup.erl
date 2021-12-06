%%%-------------------------------------------------------------------
%% @doc dog_trainer top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dog_publisher_sup).

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
    {ok, {{one_for_one, 5, 60},
       [
	{profile_update_agent,
            {dog_profile_update_agent, start_link, []},
            permanent,
            5000,
            worker,
            [dog_profile_update_agent]},
	{ipset_update_agent,
            {dog_ipset_update_agent, start_link, []},
            permanent,
            5000,
            worker,
            [dog_ipset_update_agent]},
	{dog_agent_checker,
            {dog_agent_checker, start_link, []},
            permanent,
            5000,
            worker,
            [dog_agent_checker]},
	{dog_ec2_update_agent,
            {dog_ec2_update_agent, start_link, []},
            permanent,
            5000,
            worker,
            [dog_ec2_update_agent]}
	]	
	}}.

%%====================================================================
%% Internal functions
%%====================================================================
