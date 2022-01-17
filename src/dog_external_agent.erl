-module(dog_external_agent).
-behaviour(gen_server).

-include("dog_trainer.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
         set_link_state/1,
         loop/4
         %subscribe_to_queue/1
        ]).

%% Test Function Exports
%%
-export([
         %delete_active_queue/1,
         %inbound_queue_spec/1,
        %outbound_queue_spec/1,
        %unbind_exchange/1
        inbound_service_spec/1,
        outbound_publisher_spec/1
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link(Link :: map()) ->
  {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start_link(Link) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Link], []).

%-spec subscribe_to_queue(QueueDefinition ::
%                    map()) -> atom().
%
%subscribe_to_queue(#{broker := Broker,
%                name := Name, queue := QueueName}) ->
%    Callback = fun (A, B, C) ->
%               subscriber_callback(A, B, C)
%           end,
%    case dog_thumper_sup:ensure_consumer(up, Name, Broker,
%                     QueueName, Callback)
%    of
%      {ok, _ChildPid} -> 
%        ok;
%      {error, {already_up, _ChildPid}} -> 
%        ok
%    end.
%
%-spec unsubscribe_to_queue(Name :: atom()) -> atom().
%
%unsubscribe_to_queue(Name) ->
%    case dog_thumper_sup:ensure_consumer(down, Name )
%    of
%      ok -> 
%        ok;
%      {error, already_down} -> 
%        ok
%    end.

%turtle publisher callback
loop(_RoutingKey, _CType, Payload, State) ->
    Proplist = binary_to_term(Payload),
    UserData = proplists:get_value(user_data, Proplist),
    lager:debug("UserData: ~p~n",[UserData]),
    Ipsets = maps:get(ipsets, UserData),
    lager:debug("Ipsets: ~p~n",[Ipsets]),
    IpsetsDecoded = jsx:decode(Ipsets),
    lager:debug("IpsetsDecoded: ~p~n",[IpsetsDecoded]),
    ExternalEnv = jsn:as_map(IpsetsDecoded),
    lager:debug("ExternalEnv: ~p",[ExternalEnv]),
    imetrics:add(external_ipset_update),
    ExternalEnvName = maps:get(<<"name">>,ExternalEnv),
    lager:info("external ipsets receieved: ~p",[ExternalEnvName]),
    {ok,ExistingExternal} = dog_external:get_by_name(ExternalEnvName),
    ExistingExternalId = maps:get(<<"id">>,ExistingExternal),
    dog_external:replace(ExistingExternalId,ExternalEnv), %TODO: create on link creation, set empty, inactive
    dog_ipset:update_ipsets(local_env),
    {ack,State}.

%-spec delete_queue(QueueDefinition ::
%                map()) -> atom().

%delete_queue(#{broker := Broker, name := Name,
%            queue := QueueName}) ->
%    Op = {'queue.delete',
%      [{queue, QueueName},
%       {auto_delete, true}, {durable, true}]},
%    lager:debug("Deleting queue: ~p", [Name]),
%    case dog_thumper_sup:amqp_op(Broker, Name, [Op]) of
%      ok -> 
%        ok;
%      {error, Reason} -> 
%        lager:error("Reason: ~p", [Reason]), 
%        error
%    end.

%-spec create_queue(QueueDefinition ::
%                map()) -> atom().
%create_queue(#{broker := Broker, name := Name,
%            queue := QueueName}) ->
%    Op = {'queue.declare',
%      [{queue, QueueName},
%       {auto_delete, true}, {durable, true}]},
%    lager:debug("Creating queue: ~p", [Name]),
%    Response = dog_thumper_sup:amqp_op(Broker, Name, [Op]),
%    lager:info("Response: ~p", [Response]),
%    case Response of
%      ok -> 
%        ok;
%      Error -> 
%        lager:error("Error: ~p", [Error]), Error
%    end.
%
%-spec bind_queue(QueueDefintion :: map()) -> ok | {error, Reason :: iolist()}.
%
%bind_queue(#{broker := Broker, name := ConsumerName, queue := QueueName, routing_key := RoutingKey, exchange := ExchangeName } ) ->
%    Op = {'queue.bind',
%      [{queue, QueueName},
%       {exchange, ExchangeName}, {routing_key, RoutingKey} ]},
%    lager:debug("Binding queue: ~p to: ~p", [ConsumerName,ExchangeName]),
%    case dog_thumper_sup:amqp_op(Broker, ConsumerName, [Op]) of
%      ok -> 
%        lager:debug("ok");
%      {error, Reason} -> 
%        lager:debug("Reason: ~p", [Reason]),
%        {error, Reason}
%    end.
%
%-spec unbind_queue(QueueDefintion :: map() ) -> ok | {error, Reason :: iolist()}.
%
%unbind_queue(#{broker := Broker, name := ConsumerName, queue := QueueName, routing_key := RoutingKey, exchange := ExchangeName } ) ->
%    Op = {'queue.unbind',
%      [{queue, QueueName},
%       {exchange, ExchangeName}, {routing_key, RoutingKey} ]},
%    lager:debug("Unbinding queue: ~p to: ~p", [ConsumerName,ExchangeName]),
%    case dog_thumper_sup:amqp_op(Broker, ConsumerName, [Op])
%    of
%      ok -> 
%        lager:debug("ok");
%      {error, Reason} -> 
%        lager:debug("Reason: ~p", [Reason]),
%        {error, Reason}
%    end.
%
%-spec unbind_exchange(QueueDefintion :: map() ) -> ok | {error, Reason :: iolist()}.
%
%unbind_exchange(#{broker := Broker, name := ConsumerName, queue := QueueName, routing_key := RoutingKey, exchange := ExchangeName } ) ->
%    Op = {'exchange.unbind',
%      [{queue, QueueName},
%       {exchange, ExchangeName}, {routing_key, RoutingKey} ]},
%    lager:debug("Unbinding queue: ~p to: ~p", [ConsumerName,ExchangeName]),
%    case dog_thumper_sup:amqp_op(Broker, ConsumerName, [Op])
%    of
%      ok -> 
%        lager:debug("ok");
%      {error, Reason} -> 
%        lager:debug("Reason: ~p", [Reason]),
%        {error, Reason}
%    end.

%-spec outbound_queue_spec(EnvName :: binary()) -> map().
%
%outbound_queue_spec(EnvName) ->
%    BaseName = <<"outbound">>,
%    Sep = <<"_">>,
%    QueueName = <<BaseName/bitstring,Sep/bitstring,EnvName/bitstring>>,
%    Name = list_to_atom(binary:bin_to_list(QueueName)),
%    #{broker => default, name => Name, queue => QueueName, routing_key => <<"fanout">>, exchange => BaseName }.
%
%-spec inbound_queue_spec(EnvName :: binary()) -> map().
%
%inbound_queue_spec(EnvName) ->
%    BaseName = <<"inbound">>,
%    Sep = <<"_">>,
%    QueueName = <<BaseName/bitstring,Sep/bitstring,EnvName/bitstring>>,
%    Name = list_to_atom(binary:bin_to_list(QueueName)),
%    #{broker => default, name => Name, queue => QueueName, routing_key => EnvName, exchange => BaseName }.

-spec set_link_state(NewState :: map() ) -> ok | error.
set_link_state(
                           #{env_name := EnvName,
                            new_enabled_state := NewEnabledState,
                            new_direction_state := NewDirectionState,
                            old_enabled_state := EnabledState,
                            old_direction_state := DirectionState}
                ) ->
  lager:info("New link state: EnvName: ~p, EnabledState: ~p, NewEnabledState: ~p, DirectionState: ~p, NewDirectionState: ~p",
             [EnvName,EnabledState,NewEnabledState,DirectionState,NewDirectionState]),
  %InboundQueueSpec = inbound_queue_spec(EnvName),
  %OutboundQueueSpec = outbound_queue_spec(EnvName),
  ExternalId = case dog_external:get_by_name(EnvName) of
    {error, notfound} ->
                   none;
    {ok,External} ->
                   maps:get(<<"id">>,External,none)
               end,
  case EnabledState of
    false ->
      case NewEnabledState of
        true ->
          dog_external:setup_external_broker_connections(EnvName),
          case NewDirectionState of
            <<"bidirectional">> ->
              create_active_inbound_queue(EnvName),
              create_active_outbound_queue(EnvName),
              ok = dog_external:set_active_by_id(ExternalId),
              ok;
            <<"inbound">> ->
              create_active_inbound_queue(EnvName),
              ok = dog_external:set_active_by_id(ExternalId),
              ok;
            <<"outbound">> ->
              create_active_outbound_queue(EnvName),
              ok
          end;
        false ->
          ok
      end;
    true ->
      case NewEnabledState of
        true ->
          case DirectionState of
            <<"bidirectional">> ->
              case NewDirectionState of
                <<"bidirectional">> ->
                  ok;
                <<"inbound">> ->
                  delete_active_outbound_queue(EnvName),
                  ok;
                <<"outbound">> ->
                  delete_active_inbound_queue(EnvName),
                  ok = dog_external:set_inactive_by_id(ExternalId),
                  ok
              end;
            <<"inbound">> ->
              case NewDirectionState of
                <<"bidirectional">> ->
                  create_active_outbound_queue(EnvName),
                  ok;
                <<"inbound">> ->
                  ok;
                <<"outbound">> ->
                  delete_active_inbound_queue(EnvName),
                  create_active_outbound_queue(EnvName),
                  ok = dog_external:set_inactive_by_id(ExternalId),
                  ok
              end;
            <<"outbound">> ->
              case NewDirectionState of
                <<"bidirectional">> ->
                  create_active_inbound_queue(EnvName),
                  ok = dog_external:set_active_by_id(ExternalId),
                  ok;
                <<"inbound">> ->
                  delete_active_inbound_queue(EnvName),
                  create_active_inbound_queue(EnvName),
                  ok = dog_external:set_active_by_id(ExternalId),
                  ok;
                <<"outbound">> ->
                  ok
              end
          end;
        false ->
          case DirectionState of
            <<"bidirectional">> ->
              delete_active_inbound_queue(EnvName),
              delete_active_outbound_queue(EnvName),
              ok;
            <<"inbound">> ->
              delete_active_inbound_queue(EnvName),
              ok;
            <<"outbound">> ->
              delete_active_outbound_queue(EnvName),
              ok
          end,
          ok = dog_external:set_inactive_by_id(ExternalId)
      end
  end.


-spec delete_active_inbound_queue(EnvName :: binary()) -> ok | { error, Reason :: iolist()}.
delete_active_inbound_queue(EnvName) ->
  turtle_service:close(EnvName).

-spec create_active_inbound_queue(EnvName :: binary()) -> ok | { error, Reason :: iolist()}.
create_active_inbound_queue(EnvName) ->
  %create_queue(QueueSpec),
  %bind_queue(QueueSpec),
  %subscribe_to_queue(QueueSpec),
  %larger:debug("EnvName: ~p",[EnvName]),
  turtle_service:new(dog_turtle_sup,inbound_service_spec(EnvName)),
  ok.

inbound_service_spec(EnvName) ->
    Name = binary_to_atom(EnvName),
    Exchange = <<"inbound">>,
    QueueName = <<<<"inbound">>/binary,<<"_">>/binary,EnvName/binary>>,
    Config = #{
      name => Name,
      connection => default,
      function => fun dog_external:loop/4,
      handle_info => fun dog_external_agent:handle_info/2,
      init_state => #{ },
      declarations =>
          [#'exchange.declare' { exchange = Exchange , type = <<"topic">>, durable = true },
           #'queue.declare' { queue = QueueName, durable = true, auto_delete = true },
           #'queue.bind' { queue = QueueName, exchange = Exchange, routing_key = EnvName }],
%    #{broker => default, name => Name, queue => QueueName, routing_key => EnvName, exchange => BaseName }.
      subscriber_count => 1,
      prefetch_count => 1,
      consume_queue => QueueName,
      passive => false
    },
    ServiceSpec = turtle_service:child_spec(Config),
	ServiceSpec.

outbound_publisher_spec(EnvName) ->
    Name = binary_to_atom(EnvName),
    PublisherName = binary_to_atom(<<EnvName/binary, <<"_publisher">>/binary>>),
    ConnName = Name,
    %Exchange = <<"outbound">>,
    %QueueName = <<<<"outbound">>/binary,<<"_">>/binary,EnvName/binary>>,
    AMQPDecls = [
		  %#'exchange.declare' {exchange = Exchange, type = <<"fanout">>, durable = true},
          % #'queue.declare' { queue = QueueName, durable = true, auto_delete = true },
          % #'queue.bind' { queue = QueueName, exchange = Exchange, routing_key = <<"fanout">> }
%    #{broker => default, name => Name, queue => QueueName, routing_key => <<"fanout">>, exchange => BaseName }.
    ],
    AMQPPoolChildSpec =
        turtle_publisher:child_spec(PublisherName, ConnName, AMQPDecls,
            #{ confirms => true, passive => false, rpc => false }),
	AMQPPoolChildSpec.

-spec delete_active_outbound_queue(EnvName :: binary()) -> ok | { error, Reason :: iolist()}.
delete_active_outbound_queue(EnvName) ->
  turtle_publisher:close(EnvName),
  ok.

-spec create_active_outbound_queue(EnvName :: binary()) -> ok | { error, Reason :: iolist()}.
create_active_outbound_queue(EnvName) ->
  %create_queue(QueueSpec),
  %bind_queue(QueueSpec),
  turtle_publisher:new(dog_turtle_sup,outbound_publisher_spec(EnvName)),
  ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([Link]) ->
  lager:debug("init"),
  State = #{env_name => maps:get(<<"name">>,Link),
                            new_enabled_state => maps:get(<<"enabled">>,Link),
                            new_direction_state => maps:get(<<"direction">>,Link),
                            old_enabled_state => false,
                            old_direction_state => <<"inbound">>},
  set_link_state(State),
  {ok, State}.

-spec handle_call(term(), {pid(), term()}, State::ips_state()) -> {reply, ok, any()}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Msg, State) ->
  lager:error("unknown_message: Msg: ~p, State: ~p",[Msg, State]),
  {noreply, State}.

-spec handle_info(_,_) -> {'noreply',_}.
handle_info(Info, State) ->
  lager:error("unknown_message: Info: ~p, State: ~p",[Info, State]),
  {noreply, State}.

-spec terminate(_, ips_state()) -> {close}.
terminate(Reason, State) ->
  lager:info("terminate: Reason: ~p, State: ~p", [Reason, State]),
  {close}.

-spec code_change(_, State::ips_state(), _) -> {ok, State::ips_state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
