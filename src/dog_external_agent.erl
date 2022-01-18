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
        ]).

%% Test Function Exports
%%
-export([
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
          dog_external:start_external_broker_connection(EnvName),
          case NewDirectionState of
            <<"bidirectional">> ->
              create_inbound_service(EnvName),
              create_outbound_publisher(EnvName),
              ok = dog_external:set_active_by_id(ExternalId),
              ok;
            <<"inbound">> ->
              create_inbound_service(EnvName),
              ok = dog_external:set_active_by_id(ExternalId),
              ok;
            <<"outbound">> ->
              create_outbound_publisher(EnvName),
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
                  stop_outbound_publisher(EnvName),
                  ok;
                <<"outbound">> ->
                  stop_inbound_service(EnvName),
                  ok = dog_external:set_inactive_by_id(ExternalId),
                  ok
              end;
            <<"inbound">> ->
              case NewDirectionState of
                <<"bidirectional">> ->
                  create_outbound_publisher(EnvName),
                  ok;
                <<"inbound">> ->
                  ok;
                <<"outbound">> ->
                  stop_inbound_service(EnvName),
                  create_outbound_publisher(EnvName),
                  ok = dog_external:set_inactive_by_id(ExternalId),
                  ok
              end;
            <<"outbound">> ->
              case NewDirectionState of
                <<"bidirectional">> ->
                  create_inbound_service(EnvName),
                  ok = dog_external:set_active_by_id(ExternalId),
                  ok;
                <<"inbound">> ->
                  stop_inbound_service(EnvName),
                  create_inbound_service(EnvName),
                  ok = dog_external:set_active_by_id(ExternalId),
                  ok;
                <<"outbound">> ->
                  ok
              end
          end;
        false ->
          case DirectionState of
            <<"bidirectional">> ->
              stop_inbound_service(EnvName),
              stop_outbound_publisher(EnvName),
              ok;
            <<"inbound">> ->
              stop_inbound_service(EnvName),
              ok;
            <<"outbound">> ->
              stop_outbound_publisher(EnvName),
              ok
          end,
          dog_external:stop_external_broker_connection(EnvName),
          ok = dog_external:set_inactive_by_id(ExternalId)
      end
  end.


-spec stop_inbound_service(EnvName :: binary()) -> ok | { error, Reason :: iolist()}.
stop_inbound_service(EnvName) ->
  turtle_service:stop(dog_turtle_sup,binary_to_atom(EnvName)).

-spec create_inbound_service(EnvName :: binary()) -> ok | { error, Reason :: iolist()}.
create_inbound_service(EnvName) ->
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
          [
           #'exchange.declare' { exchange = Exchange , type = <<"topic">>, durable = true },
           #'queue.declare' { queue = QueueName, durable = true, auto_delete = true },
           #'queue.bind' { queue = QueueName, exchange = Exchange, routing_key = EnvName },
		   #'exchange.declare' {exchange = <<"outbound">>, type = <<"fanout">>, durable = true}
          ],
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
    AMQPDecls = [
    ],
    AMQPPoolChildSpec =
        turtle_publisher:child_spec(PublisherName, ConnName, AMQPDecls,
            #{ confirms => true, passive => false, rpc => false }),
	AMQPPoolChildSpec.

-spec stop_outbound_publisher(EnvName :: binary()) -> ok | { error, Reason :: iolist()}.
stop_outbound_publisher(EnvName) ->
  turtle_publisher:stop(dog_turtle_sup,binary_to_atom(EnvName)),
  ok.

-spec create_outbound_publisher(EnvName :: binary()) -> ok | { error, Reason :: iolist()}.
create_outbound_publisher(EnvName) ->
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
