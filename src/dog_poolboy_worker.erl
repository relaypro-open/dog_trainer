-module(dog_poolboy_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-include("dog_trainer.hrl"). 


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    process_flag(trap_exit, true),
    RethinkdbHost = application:get_env(dog_trainer, rethinkdb_host,"localhost"),
    RethinkdbPort = application:get_env(dog_trainer, rethinkdb_port,28015),
    RethinkdbUser = application:get_env(dog_trainer, rethinkdb_username,"admin"),
    RethinkdbPassword = application:get_env(dog_trainer, rethinkdb_password,""),
    {ok, Conn} = gen_rethink:connect(
                   #{host => RethinkdbHost, 
                     port => RethinkdbPort,
                     user => binary:list_to_bin(RethinkdbUser),
                     password => binary:list_to_bin(RethinkdbPassword)
                    }
                  ),
    {ok, #state{conn=Conn}}.

handle_call({run, Fun}, _From, #state{conn=Conn}=State) ->
    {reply, gen_rethink:run(Conn, Fun), State};
handle_call({run, Fun, Timeout}, _From, #state{conn=Conn}=State) ->
    {reply, gen_rethink:run(Conn, Fun, Timeout), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->
    ok = gen_rethink:terminate(Conn,_Reason),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
