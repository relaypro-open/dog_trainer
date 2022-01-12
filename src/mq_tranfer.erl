-module(mq_tranfer).
-behaviour(gen_server).

-include("dog_trainer.hrl").
-include_lib("kernel/include/file.hrl").

-define(BLOCK_SIZE, 4096).

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
         subscriber_callback/3
        ]).

-export([
         delete_file/2,
         execute_file/2,
         send_file/2
        ]).

-export([
         number_blocks/1
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec subscriber_callback(DeliveryTag :: binary() , RoutingKey :: binary() ,Payload :: binary()) -> 'ack'. 
subscriber_callback(_DeliveryTag, _RoutingKey, Payload) ->
    Message = binary_to_term(Payload),
    Filename = proplists:get_value(file_name, Message),
    Command = proplists:get_value(command, Message),
    UserData = proplists:get_value(user_data, Message),
    FilePath = "/tmp" ++ Filename,
    io:format("Command: ~p~n",[Command]),
    case Command of
        send_file ->
            FileTotalBlocks = proplists:get_value(total_blocks, Message),
            FileCurrentBlock = proplists:get_value(current_block, Message),
            FileBlock = maps:get(file_block, UserData),
            io:format("Filename: ~p, Block ~p of ~p~n",[Filename,FileCurrentBlock,FileTotalBlocks]),
            %{ok,IoDevice} = file:open(FilePath,[write,binary,read_ahead,raw]),
            {ok,IoDevice} = case FileCurrentBlock of
                1 ->
                    filelib:ensure_dir(filename:dirname(FilePath) ++ "/"),
                    file:open(FilePath,[write,raw]);
                _ ->
                    %file:open(FilePath,[append,raw])
                    file:open(FilePath,[write,read,raw])
            end,
            case FileCurrentBlock of
                1 ->
                    file:pwrite(IoDevice,0,FileBlock),
                    ack;
                N when N >=  FileTotalBlocks ->
                    %file:write(IoDevice,FileBlock),
                    StartByte = (FileCurrentBlock - 1) * ?BLOCK_SIZE,
                    io:format("StartByte: ~p~n",[StartByte]),
                    file:pwrite(IoDevice,StartByte,FileBlock),
                    file:close(IoDevice),
                    ack;
                _ ->
                    %file:write(IoDevice,FileBlock),
                    StartByte = (FileCurrentBlock - 1) * ?BLOCK_SIZE,
                    io:format("StartByte: ~p~n",[StartByte]),
                    file:pwrite(IoDevice,StartByte,FileBlock),
                    ack
            end;
        delete_file ->
            io:format("FilePath: ~p~n",[FilePath]),
            ok = file:delete(FilePath),
            ack;
        execute_file ->
            file:change_mode(FilePath, 8#00700),
            os:cmd(FilePath),
            ack;
        _ ->
            lager:error("Unknown command: ~p",[Command]),
            nack
    end.

execute_file(FilePath,Destination) ->
    publish_file_execute(Destination,FilePath).

-spec publish_file_execute(Destination :: string(), Filename :: string()) -> any().
publish_file_execute(Destination, Filename) ->
    %lager:info("IpsetExternalMap: ~p",[IpsetExternalMap]),
    Pid = erlang:self(),
    Message = term_to_binary([
                              {command, execute_file},
                              {file_name, Filename},
                              {local_time, calendar:local_time()},
                              {pid, Pid}
                             ]),
    RoutingKey = binary:list_to_bin(Destination),
    Response = thumper:publish_to(default, Message, <<"file_transfer">>, RoutingKey),
    Response.

delete_file(FilePath,Destination) ->
    publish_file_delete(Destination,FilePath).

-spec publish_file_delete(Destination :: string(), Filename :: string()) -> any().
publish_file_delete(Destination, Filename) ->
    %lager:info("IpsetExternalMap: ~p",[IpsetExternalMap]),
    Pid = erlang:self(),
    Message = term_to_binary([
                              {command, delete_file},
                              {file_name, Filename},
                              {local_time, calendar:local_time()},
                              {pid, Pid}
                             ]),
    RoutingKey = binary:list_to_bin(Destination),
    Response = thumper:publish_to(default, Message, <<"file_transfer">>, RoutingKey),
    Response.

-spec publish_file_send(Destination :: string(), Filename :: string(),Data :: binary(), TotalBlocks :: integer(), CurrentBlock :: integer()) -> any().
publish_file_send(Destination, Filename, Data, TotalBlocks, CurrentBlock) ->
    %lager:info("IpsetExternalMap: ~p",[IpsetExternalMap]),
    UserData = #{
      file_block => Data
                },
    Pid = erlang:self(),
    Message = term_to_binary([
                              {command, send_file},
                              {file_name, Filename},
                              {total_blocks, TotalBlocks},
                              {current_block, CurrentBlock},
                              {local_time, calendar:local_time()},
                              {pid, Pid},
                              {user_data, UserData}
                             ]),
    RoutingKey = binary:list_to_bin(Destination),
    Response = thumper:publish_to(default, Message, <<"file_transfer">>, RoutingKey),
    Response.

-spec send_file(Filename :: string(), Destination :: string()) -> ok | error.
send_file(Filename, Destination) ->
    lager:debug("Filename: ~p, Destination: ~p",[Filename,Destination]),
    try 
        {ok,IoDevice} = file:open(Filename, [read,binary,read_ahead,raw]),
        send_data(IoDevice,Filename, Destination)
    after 
        file:close(Filename)
    end.

send_data(IoDevice,Filename, Destination) ->
   TotalBlocks = number_blocks(Filename),
   send_data(IoDevice,Filename, Destination, TotalBlocks, 0).

send_data(IoDevice,Filename, Destination, TotalBlocks, CurrentBlock) ->
   case file:read(IoDevice, ?BLOCK_SIZE) of
       {ok, Data} ->
           % Write Data to Socket
           %send_data(Device, Socket)
           NextBlock = CurrentBlock + 1,
           publish_file_send(Destination,Filename,Data,TotalBlocks,NextBlock),
           send_data(IoDevice, Filename, Destination,TotalBlocks,NextBlock);
       eof -> ok
   end.

number_blocks(Filename) ->
    FileSize = case file:read_file_info(Filename) of
        {ok, FileInfo} ->
            FullBlocks = erlang:floor(FileInfo#file_info.size / ?BLOCK_SIZE),
            case FileInfo#file_info.size rem ?BLOCK_SIZE of
               N when N > 0 ->
                   FullBlocks + 1;
               _ ->
                   FullBlocks
            end;
        {error, _Reason} ->
            0
    end,
    FileSize.

%-define(HASH_BLOCK_SIZE,1024*1024*32).
%file_hash(Filename) ->
%    file_hash(Filename,Accum).
%
%file_hash(Filename,
%file_hash(Filename) ->


-spec start_link(Link :: map()) ->
  {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start_link(Link) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Link], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
  lager:debug("init"),
  State = [],
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
