-module(dog_file_transfer_sup).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([
         start_link/0, 
         init/1
       ]).
-behaviour(supervisor).

-define(SERVER, ?MODULE).

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
                 ],
    {ok, { {one_for_all, 10, 60}, ChildSpecs} }.

