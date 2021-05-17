-module(dog_utils).

-include("dog_trainer.hrl").

%API
-export([
         re_filter/2
        ]).

-spec re_filter(List :: [iolist()], Re :: string()) -> [iolist()].
re_filter(List,Re) ->
  lists:filter(fun(String) -> 
                   case re:run(String,Re) of 
                     {match,_} -> true; 
                     nomatch -> false 
                   end 
               end,List).
