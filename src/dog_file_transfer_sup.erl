-module(dog_file_transfer_sup).

-include("dog_trainer.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

-export([
    start_link/0,
    init/1
]).
-behaviour(supervisor).

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
            {dog_file_transfer_agent, {dog_file_transfer_agent, start_link, []}, permanent, 5000, worker, [
                dog_file_transfer_agent
            ]}
    ],
    {ok, {{one_for_all, 10, 60}, ChildSpecs}}.
