-module(dog_file_transfer_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-include("dog_trainer.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
% -type state() :: list().

-export([
         delete_file/3,
         execute_command/3,
         fetch_file/3,
         send_file/4,
         start_link/0,
         start_link/1
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% ------------------------------------------------------------------
%% test Function Exports
%% ------------------------------------------------------------------
-export([do_execute_command/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%-spec periodic_publish() -> OldServer :: ok.
execute_command(ExecuteCommand, Hostkey, Opts) ->
    Response = dog_file_transfer:execute_command(ExecuteCommand, Hostkey, Opts),
    Response.

delete_file(FilePath, Hostkey, Opts) ->
    Response = dog_file_transfer:delete_file(FilePath, Hostkey, Opts),
    Response.

fetch_file(FilePath, Hostkey, Opts) ->
    Response = dog_file_transfer:fetch_file(FilePath, Hostkey, Opts),
    Response.

send_file(LocalFilePath, RemoteFilePath, Hostkey, Opts) ->
    Response = dog_file_transfer:send_file(LocalFilePath, RemoteFilePath, Hostkey, Opts),
    Response.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State} |
%%          {ok, State, Timeout} |
%%          ignore |
%%          {stop, Reason}
%%----------------------------------------------------------------------

-spec init(_) -> {'ok', []}.
init(_Args) ->
    State = [],
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, Reply, State} | (terminate/2 is called)
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, State :: ips_state()) -> {reply, ok, any()}.
handle_call({execute_command, ExecuteCommand, Hostkey, Opts}, _From, State) ->
    Response = do_execute_command(State, ExecuteCommand, Hostkey, Opts),
    {reply, Response, State};
handle_call({delete_file, Path, Id, Opts}, _From, State) ->
    Response = do_delete_file(State, Path, Id, Opts),
    {reply, Response, State};
handle_call({fetch_file,FilePath, Hostkey, Opts}, _From, State) ->
    Response = do_fetch_file(State, FilePath, Hostkey, Opts),
    {reply, Response, State};
handle_call({send_file, LocalFilePath, RemoteFilePath, Hostkey, Opts}, _From, State ) ->
    Response = do_file_transfer(State, LocalFilePath, RemoteFilePath, Hostkey, Opts),
    {reply, Response, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
-spec handle_cast(_, _) -> {'noreply', _}.
handle_cast(Msg, State) ->
    ?LOG_ERROR(#{"message" => "unknown_message", "msg" => Msg, "state" => State}),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
% TODO: be more specific about Info in spec
-spec handle_info(_, _) -> {'noreply', _}.
handle_info(Info, State) ->
    ?LOG_ERROR(#{"message" => "unknown_message", "info" => Info, "state" => State}),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
-spec terminate(_, ips_state()) -> {close}.
terminate(Reason, State) ->
    ?LOG_INFO(#{"message" => "terminate", "reason" => Reason, "state" => State}),
    {close}.

-spec code_change(_, State :: ips_state(), _) -> {ok, State :: ips_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%-spec do_periodic_publish(State :: list() , ExecuteCommand :: iolist(), Hostkey :: iolist(), Opts :: list() ) ->
%    [{ok | error , iolist() }, state()].
do_execute_command(State, ExecuteCommand, Hostkey, Opts) ->
    Response = dog_file_transfer:execute_command(ExecuteCommand, Hostkey, Opts),
    [Response, State].

do_delete_file(State, Path, Id, Opts) ->
    Response =  dog_file_transfer:delete_file(Path, Id, Opts),
    [Response, State].

do_fetch_file(State, Path, Id, Opts) ->
    Response = dog_file_transfer:fetch_file(Path, Id, Opts),
    [Response, State].

do_file_transfer(State, LocalFilePath, RemoteFilePath, Hostkey, Opts) ->
    Response = dog_file_transfer:send_file(LocalFilePath, RemoteFilePath, Hostkey, Opts),
    [Response, State].
