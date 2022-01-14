-module(turtle_test_sup).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([
		%add_conn_config/0,
        close_conn/0,
         start_link/0, 
         init/1,
		%conn_sup/1,
		%new_connection/0,
		new_publisher/0,
        new_service/0,
		external_publisher_spec/0,
        registration_connection_spec/0
       ]).
-behaviour(supervisor).

-define(SERVER, ?MODULE).

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
                  external_publisher_spec()
                 ],
    {ok, { {one_for_all, 10, 60}, ChildSpecs} }.

new_publisher() ->
    turtle_conn:new(turtle_connection_config()),
    turtle_publisher:new(dog_turtle_sup,external_publisher_spec()).
    %supervisor:start_child(turtle_sup,turtle_test_sup:external_publisher_spec()).

new_service() ->
    turtle_service:new(dog_turtle_sup,registration_connection_spec()).

close_conn() ->
    turtle_conn:close(external).

external_publisher_spec() ->
    PublisherName = external_publisher,
    ConnName = external, %% As given abov<<"registration">>e
    AMQPDecls = [
		  #'exchange.declare' {exchange = <<"inbound">>, type = <<"topic">>, durable = true},
		  #'exchange.declare' {exchange = <<"outbound">>, type = <<"fanout">>, durable = true}
    ],
    AMQPPoolChildSpec =
        turtle_publisher:child_spec(PublisherName, ConnName, AMQPDecls,
            #{ confirms => true, passive => false, rpc => false }),
	AMQPPoolChildSpec.

registration_connection_spec() ->
    Exch = <<"registration">>,
    Q = <<"registration.request">>,
    Config = #{
      name => registration,
      connection => default,
      function => fun dog_ips:loop/4,
      handle_info => fun dog_ips:handle_info/2,
      init_state => #{ },
      declarations =>
          [#'exchange.declare' { exchange = Exch, type = <<"topic">>, durable = true },
           #'queue.declare' { queue = Q, durable = false },
           #'queue.bind' { queue = Q, exchange = Exch, routing_key = <<"registration">> }],
      subscriber_count => 1,
      prefetch_count => 1,
      consume_queue => Q,
      passive => false
    },

    ServiceSpec = turtle_service:child_spec(Config),
	ServiceSpec.

turtle_connection_config() ->
    #{
            conn_name => external,

            username => "dog_trainer",
            password => "327faf06-c3f5-11e7-9765-7831c1be5b34",
            virtual_host => "dog",
            ssl_options => [
                           {cacertfile, "/var/consul/data/pki/certs/ca.crt"},
                           {certfile, "/var/consul/data/pki/certs/server.crt"},
                           {keyfile, "/var/consul/data/pki/private/server.key"},
                           {verify, verify_peer},
                           {server_name_indication, disable},
                           {fail_if_no_peer_cert, true}
                          ],
            deadline => 300000,
            connections => [
                {main, [
                  {"dog-broker-dev1.nocell.io", 5673 } 
                ]}
            ]
}.

%new_connection() ->
%	Spec = conn_sup(turtle_connection_config()),
%    supervisor:start_child(turtle_sup,Spec).
%
%conn_sup(#{conn_name := Name} = Ps) ->
%    #{ id => Name,
%       start => {turtle_conn, start_link, [Name, Ps]},
%       restart => permanent,
%       shutdown => 5000,
%       type => worker
%     }.
%
%add_conn_config() ->
%	CurrentConfig = application:get_env(turtle, connection_config, []),
%	NewConfig = CurrentConfig ++ [turtle_connection_config()],
%	application:set_env(turtle,connection_config,NewConfig).
