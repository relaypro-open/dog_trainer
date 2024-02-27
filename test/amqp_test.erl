-module(amqp_test).

-include(
    "_build/default/lib/amqp_client/include/amqp_client.hrl"
).

-export([test/0]).

test() ->
    APN = #amqp_params_network{
        host =
            "dog-ubuntu-server.lxd",
        port = 5673,
        virtual_host = <<"dog">>,
        username = <<"dog_trainer">>,
        password = <<"327faf06-c3f5-11e7-9765-7831c1be5b34">>,
        ssl_options =
            [
                {cacertfile, "/var/consul/data/pki/certs/ca.crt"},
                {certfile, "/var/consul/data/pki/certs/server.crt"},
                {keyfile, "/var/consul/data/pki/private/server.key"},
                {verify, verify_peer},
                {server_name_indication, "localhost"},
                {fail_if_no_peer_cert, true}
            ]
    },
    {ok, Connection} = amqp_connection:start(APN),
    {ok, Channel} =
        amqp_connection:open_channel(Connection),

    %% Declare a queue
    #'queue.declare_ok'{queue = Q} =
        amqp_channel:call(Channel, #'queue.declare'{}),

    %% Publish a message
    Payload = <<"foobar">>,
    Publish = #'basic.publish'{
        exchange = <<>>,
        routing_key = Q
    },
    amqp_channel:cast(
        Channel,
        Publish,
        #amqp_msg{payload = Payload}
    ),

    %% Get the message back from the queue
    Get = #'basic.get'{queue = Q},
    {#'basic.get_ok'{delivery_tag = Tag}, Content} =
        amqp_channel:call(Channel, Get),

    lager:info("Content: ~p", [Content]),

    %% Do something with the message payload
    %% (some work here)
    %% Ack the message
    amqp_channel:cast(
        Channel,
        #'basic.ack'{delivery_tag = Tag}
    ),

    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    ok.
