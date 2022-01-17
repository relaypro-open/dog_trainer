-module(dog_trainer_sup).
-export([start_link/0, init/1]).
-behaviour(supervisor).

-define(SUP(M), #{id => M,
                  start => {M, start_link, []},
                  type => supervisor}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
                                 ?SUP(dog_db_pool_sup),
                                 ?SUP(dog_rethink_sup),
                                 ?SUP(dog_watcher_sup),
                                 ?SUP(dog_publisher_sup),
                                 ?SUP(dog_turtle_sup),
                                 ?SUP(dog_api_router),
                                 ?SUP(dog_hosts_agent_sup),
                                 ?SUP(dog_external_sup)
                                ]}}.
