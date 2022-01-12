-module(dog_thumper_sup).
-export([start_link/0, ensure_consumer/5, ensure_consumer/2, init/1,
         amqp_op/3]).
-behaviour(supervisor).

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec ensure_consumer('up',atom(),_,_,_) -> {'error',_} | {'ok','undefined' | pid()} | {'ok','undefined' | pid(),_}.
ensure_consumer(up, Name, Broker, Queue, Callback) ->
    case get_pid(Name) of
        {ok, Pid} ->
             {error, {already_up, Pid}};
        {error, terminated} ->
             supervisor:restart_child(?MODULE, Name);
        {error, deleted} ->
             supervisor:start_child(?MODULE, spec(Name, Broker, Queue, Callback))
    end.

-spec ensure_consumer('down',atom()) -> 'ok' | {'error','already_down' | 'not_found' | 'simple_one_for_one'}.
ensure_consumer(down, Name) ->
    case get_pid(Name) of
        {ok, _Pid} ->
             supervisor:terminate_child(?MODULE, Name);
        {error, terminated} ->
             {error, already_down};
        {error, deleted} ->
             {error, already_down}
    end.

%-spec amqp_op(_,atom(),_) -> {'error' | 'ok' | {'error',_} | {'error',_,'not_implemented'},'bad_state' | 'no_channel' | {'thumper_tx_state',_,_,_}}.
-spec amqp_op(_,atom(),_) -> ok | {error, Reason :: atom()}.
amqp_op(Broker, Consumer, Ops) ->
    case whereis(Consumer) of
        undefined ->
            Config = broker_connection:rabbitmq_config(Broker),
            Tx = [{rabbitmq_config, Config},
                  {tx, Ops}],
            ok = thumper_tx:run(Tx),
            ok;
        _ ->
            case thumper_consumer:get_state(Consumer, 2000) of
                {error, Reason} ->
                    {error, Reason};
                State ->
                    case proplists:get_value(channel, State) of
                        undefined ->
                            {error, no_channel};
                        Channel ->
                            thumper_tx:run([{tx, Ops}], Channel),
                            ok
                    end
            end
    end.

init([]) ->
    {ok, 
        { {one_for_one, 5, 10}, [
                                  spec(ips, default, <<"ips">>,
                                      fun dog_ips:subscriber_callback/3),
                                  spec(mq_tranfer, default, <<"file_transfer">>,
                                      fun mq_tranfer:subscriber_callback/3)
                                 ]} }.

-spec spec(atom(),_,_,_) -> #{'id':=atom(), 'start':={'thumper_consumer','start_link',[any(),...]}}.
spec(Name, Broker, Queue, Callback) ->
    #{id => Name,
      start => {thumper_consumer, start_link, [Name, Broker, Queue, Callback]}}.
    
-spec get_pid(atom()) -> {'error','deleted' | 'terminated'} | {'ok',pid()}.
get_pid(Name) ->
    case whereis(Name) of
        Pid when is_pid(Pid) ->
            {ok, Pid};
        _ ->
            Children = supervisor:which_children(?MODULE),
            case lists:keyfind(Name, 1, Children) of
                {Name, Pid, _, _} when is_pid(Pid) ->
                    {ok, Pid};
                {Name, _, _, _} ->
                    {error, terminated};
                false ->
                    {error, deleted}
            end
    end.
