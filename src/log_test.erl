-module(log_test).

-include_lib("kernel/include/logger.hrl").
-include("dog_trainer.hrl").

-export([
    log/0
]).

log() ->
    Foo = "bar",
    ?LOG_DEBUG("Time: ~p", [erlang:system_time(second)], #{meta => "data"}).
