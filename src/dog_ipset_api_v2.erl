-module(dog_ipset_api_v2).

-include("dog_trainer.hrl").

%API
-export([
         get_all/0
       ]).

-spec get_all() -> {ok, list()}.
get_all() ->
    Ipsets = dog_ipset:ipsets_map(),
    {ok, #{<<"ipsets">> => Ipsets} }.
