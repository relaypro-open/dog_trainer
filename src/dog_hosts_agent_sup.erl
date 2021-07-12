%%%-------------------------------------------------------------------
%% @doc dog_trainer top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dog_hosts_agent_sup).

-behaviour(supervisor).

%% API
-export([
        start_link/0
        ]).

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
        {keepalive_agent,
            {dog_keepalive_agent, start_link, []},
            permanent,
            5000,
            worker,
            [dog_keepalive_agent]}
       ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
