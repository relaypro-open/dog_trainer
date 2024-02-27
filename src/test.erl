-module(test).

-include("dog_trainer.hrl").

-export([
         test/0
        ]).

%test() -> dog_rethink:run( fun(X) ->
%                         reql:db(X, dog), 
%                         reql:table(X, host), 
%                         reql:get(X, <<"a13e672e-79c7-4aac-992c-4587512177cc">>), 
%                         reql:update(X, fun(Y) -> 
%                                                reql:literal(reql:bracket(Y, vars),
%                                                             #{<<"test">> => <<"best">>})
%                                        end, 
%                                     #{return_changes => always})
%                 end).
%
%dog_rethink:run( fun(X) -> 
%                         reql:db(X, dog), 
%                         reql:table(X, host), 
%                         reql:get(X, <<"a13e672e-79c7-4aac-992c-4587512177cc">>), 
%                         reql:update(X, reql:literal(X, "{\"vars\": {\"test\": \"best\"}}"), #{return_changes => always}) 
%                 end).

%test() ->
%    dog_rethink:run( fun(x) ->
%                             reql:db(x, dog), 
%                             reql:table(x, host), 
%                             reql:get(x, <<"a13e672e-79c7-4aac-992c-4587512177cc">>), 
%                             reql:update(x, fun(y) -> 
%                                                    reql:literal(y, <<"{\"vars\": {\"test\": \"best\"}}">>)
%                                            end)
%                     end).
%test() ->
%    dog_rethink:run( fun(X) ->
%                             reql:db(X, dog), 
%                             reql:table(X, host), 
%                             reql:get(X, <<"a13e672e-79c7-4aac-992c-4587512177cc">>), 
%                             reql:update(X, fun(Y) -> 
%                                                    reql:literal(Y, <<"{\"vars\": {\"test\": \"best\"}}">>)
%                                            end)
%                     end).

%test() ->
%    dog_rethink:run( fun(X) ->
%                             reql:db(X, dog), 
%                             reql:table(X, host), 
%                             reql:get(X, <<"714f6f22-ad5c-4e5b-a86e-902fc9d239fe">>), 
%                             reql:update(X, #{<<"vars">> => reql:literal( #{<<"xest">> => <<"best">>}) })
%                     end).

%test() ->
%    dog_rethink:run( fun(X) ->
%                             reql:db(X, dog), 
%                             reql:table(X, host), 
%                             reql:get(X, <<"714f6f22-ad5c-4e5b-a86e-902fc9d239fe">>), 
%                             reql:update(X, reql:binary(#{vars => <<"{\"test\": \"best\"}">>}))
% end)

%test2() ->
%    Update = #{<<"vars">> => 
%               #{
%                 <<"crest">> => <<"test">>
%                }, 
%               <<"location">> => <<"office">> },
%    UpdateWithoutVars = maps:without([<<"vars">>],Update),
%    dog_rethink:run( fun(X) ->
%                             reql:db(X, dog), 
%                             reql:table(X, host), 
%                             reql:get(X, <<"714f6f22-ad5c-4e5b-a86e-902fc9d239fe">>), 
%                             reql:update(X, UpdateWithoutVars)
%                     end),
%    Vars = maps:get(<<"vars">>,Update),
%    dog_rethink:run( fun(X) ->
%                             reql:db(X, dog), 
%                             reql:table(X, host), 
%                             reql:get(X, <<"714f6f22-ad5c-4e5b-a86e-902fc9d239fe">>), 
%                             reql:update(X, #{<<"vars">> => reql:literal(Vars) })
%                     end).


%dog_fact:update(<<"14721a70-bc34-416d-be9d-4d23475311a7">>, #{ <<"groups">> => #{<<"all">> => #{<<"vars">> =>#{<<"xest">> => <<"best">>}}}}).
%
test() ->
    NewFact = jsn:as_map(jsx:decode(<<"{
  \"name\" : \"dev_qa\",
  \"groups\": 
  { 
     \"all\": {
     	\"vars\": {
        	\"key\":\"value\",
          	\"key2\": \"value2\"
        },
     	\"hosts\": {
          \"host1\": {
            \"key\": \"value\",
            \"key2\": \"value2\"
          },
          \"host2\": {
            \"key2\": \"value2\"
          }
        },
        \"children\": [
            \"test\"
        ]
     },
     \"app\":
     {
     	\"vars\": {
        	\"key\":\"value\"
        },
     	\"hosts\": {
          \"host1\": {
            \"key\": \"value\"
          }
        }
        \"children\": [
            \"test2\"
        ]
     }
  }
}">>)),
    Groups = maps:get(<<"groups">>, NewFact),
    ?LOG_DEBUG("Groups: ~p~n", [Groups]),
    GroupsLiteral = maps:map(fun(_Key,Value) -> 
                                     maps:update(<<"vars">>, 
                                                   fun(X) -> reql:literal(X) end, Value)
                             end, Groups),
    GroupsLiteral.
