-module(dog_test).

-include("dog_trainer.hrl"). 


-export([ setup/0, cleanup/1]).

setup() ->
    ?LOG_INFO("dog_test:setup()"),
    application:start(sasl),
    application:start(asn1),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(inets),
    application:start(ibrowse),
    ok. 

cleanup(_Ok) ->
    application:stop(ibrowse),
    application:stop(inets),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto),
    application:stop(public_key),
    application:stop(asn1),
    application:stop(sasl),
    ok.

