-module(dog_time).

-export([
         merge_timestamp/1,
         timestamp/0
        ]).

-spec timestamp() -> number().
timestamp() ->
    erlang:system_time(second).

-spec merge_timestamp(Map :: map()) -> map().
merge_timestamp(Map) ->
    Timestamp = dog_time:timestamp(),
    MergedMap  = maps:merge(Map ,#{<<"timestamp">> => Timestamp}),
    MergedMap.
