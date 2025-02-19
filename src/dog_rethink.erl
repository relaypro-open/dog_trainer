-module(dog_rethink).

-include("dog_trainer.hrl").

-export([run/1]).

-spec run(Fun :: fun()) ->
    {ok, Response :: pid()}
    | {ok, Response :: map()}
    | {ok, null}
    | {error, Error :: atom()}
    | {error, {atom(), Reason :: binary()}}.
run(Fun) ->
    {ok, RethinkTimeout} = application:get_env(dog_trainer, rethink_timeout_ms),
    {ok, Conn} = gen_rethink_session:get_connection(dog_session),
    gen_rethink:run(Conn, Fun, RethinkTimeout).
    %dog_db_pool_sup:run(?RETHINKDB_POOL, Fun, RethinkTimeout).
