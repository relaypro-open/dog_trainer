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
    {ok, {{one_for_one, 100, 6000}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
