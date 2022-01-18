-module(dog_turtle_sup).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([
		 get_pid/1,
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
                  config_publisher_spec(),
                  ips_service_spec(),
                  ipset_publisher_spec(),
                  iptables_publisher_spec()
                 ],
    {ok, { {one_for_all, 10, 60}, ChildSpecs} }.

ipset_publisher_spec() ->
    PublisherName = ipset_publisher,
    ConnName = default, 
    AMQPDecls = [
      #'exchange.declare' {exchange = <<"ipsets">>, type = <<"fanout">>, durable = true}
    ],
    AMQPPoolChildSpec =
        turtle_publisher:child_spec(PublisherName, ConnName, AMQPDecls,
            #{ confirms => true, passive => false, rpc => false }),
	AMQPPoolChildSpec.

config_publisher_spec() ->
    PublisherName = config_publisher,
    ConnName = default, 
    AMQPDecls = [
      #'exchange.declare' {exchange = <<"config">>, type = <<"direct">>, durable = true}
    ],
    AMQPPoolChildSpec =
        turtle_publisher:child_spec(PublisherName, ConnName, AMQPDecls,
            #{ confirms => true, passive => false, rpc => false }),
	AMQPPoolChildSpec.

iptables_publisher_spec() ->
    PublisherName = iptables_publisher,
    ConnName = default, 
    AMQPDecls = [
      #'exchange.declare' {exchange = <<"iptables">>, type = <<"topic">>, durable = true}
    ],
    AMQPPoolChildSpec =
        turtle_publisher:child_spec(PublisherName, ConnName, AMQPDecls,
            #{ confirms => true, passive => false, rpc => false }),
	AMQPPoolChildSpec.

ips_service_spec() ->
    Q = <<"ips">>,
    Config = #{
      name => ips,
      connection => default,
      function => fun dog_ips:loop/4,
      handle_info => fun dog_ips:handle_info/2,
      init_state => #{ },
      declarations =>
		  [
		  #'exchange.declare' {exchange = <<"ips">>, type = <<"topic">>, durable = true},
		  #'queue.declare' {queue = <<"ips">>, auto_delete = false, durable = true},
		  #'queue.bind' {queue = <<"ips">>, exchange = <<"ips">>, routing_key = <<"#">>},
		  #'exchange.declare' {exchange = <<"file_transfer">>, type = <<"topic">>, durable = true},
		  #'queue.declare' {queue = <<"file_transfer">>, auto_delete = false, durable = true},
		  #'queue.bind' {queue = <<"file_transfer">>, exchange = <<"file_transfer">>, routing_key = <<"#">>}
		], subscriber_count => 1,
      prefetch_count => 1,
      consume_queue => Q,
      passive => false
    },

    ServiceSpec = turtle_service:child_spec(Config),
        ServiceSpec.

-spec get_pid(atom()) -> {'error','deleted' | 'terminated'} | {'ok',pid()}.
get_pid(Name) ->                                
    case whereis(Name) of                       
        Pid when is_pid(Pid) ->                 
            {ok, Pid};                          
        _ ->                                    
            Children = supervisor:which_children(?MODULE),
            case lists:keyfind(Name, 1, Children) of
                {_N, Pid, _, _} when is_pid(Pid) ->
                    {ok, Pid};                  
                {_N, _, _, _} ->              
                    {error, terminated};        
                false ->                        
                    {error, deleted}            
            end                                 
    end.   
