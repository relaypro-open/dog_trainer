-module(dog_file_transfer).

-include_lib("kernel/include/file.hrl").

-define(BLOCK_SIZE, 4096).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         delete_file/2,
         execute_command/2,
         execute_command/3,
         send_data/3,
         send_file/3
        ]).

-export([
         number_blocks/1
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
hostkey_to_routing_key(Hostkey) ->
        erlang:iolist_to_binary(["*.*.*.",Hostkey]).

execute_command(ExecuteCommand,Hostkey) ->
  execute_command(ExecuteCommand,Hostkey, []).

execute_command(ExecuteCommand,Hostkey,Opts) ->
  publish_execute_command(Hostkey, ExecuteCommand, Opts).

-spec publish_execute_command(Hostkey :: string(), ExecuteCommand :: string(), Opts :: list() ) -> {ok|error, iolist()}.
publish_execute_command(Hostkey, ExecuteCommand, Opts) ->
    Pid = erlang:self(),
    Message = term_to_binary([
                              {command, execute_command},
                              {execute_command, ExecuteCommand },
                              {local_time, calendar:local_time()},
                              {pid, Pid}
                             ] ++ Opts),
    RoutingKey = hostkey_to_routing_key(Hostkey),
    Response = case turtle:rpc_sync(
           file_transfer_publisher, 
           <<"file_transfer">>, 
           RoutingKey, 
           <<"text/json">>,
           Message) of
              {error, Reason} -> 
                    lager:error("Reason: ~p",[Reason]),
                    {error,Reason};
              {ok, _NTime, _CType, Payload} ->
                    case hd(jsx:decode(Payload)) of
                      {<<"error">>,StdErr} ->
                        lager:error("StdErr: ~p",[StdErr]),
                        {error, StdErr};
                      {<<"ok">>,StdOut} ->  
                        lager:debug("StdOut: ~p",[StdOut]),
                        {error, StdOut}
                    end
    end,
    Response.

delete_file(FilePath,Hostkey) ->
    publish_file_delete(Hostkey,FilePath).

-spec publish_file_delete(Hostkey :: string(), Filename :: string()) -> any().
publish_file_delete(Hostkey, Filename) ->
    Pid = erlang:self(),
    Message = term_to_binary([
                              {command, delete_file},
                              {file_name, Filename},
                              {local_time, calendar:local_time()},
                              {pid, Pid}
                             ]),
    RoutingKey = hostkey_to_routing_key(Hostkey),
    case turtle:rpc_sync(
           file_transfer_publisher, 
           <<"file_transfer">>, 
           RoutingKey, 
           <<"text/json">>,
           Message) of
              {error, Reason} -> 
                    lager:error("Reason: ~p",[Reason]),
                    Reason;
              {ok, _NTime, _CType, Response} ->
                    lager:info("Response: ~p",[Response]), 
                    Response
    end.

-spec publish_file_send(Hostkey :: string(), RemoteFilePath :: string(),Data :: binary(), TotalBlocks :: integer(), CurrentBlock :: integer()) -> any().
publish_file_send(Hostkey, RemoteFilePath, Data, TotalBlocks, CurrentBlock) ->
    UserData = #{
      file_block => Data
                },
    Pid = erlang:self(),
    Message = term_to_binary([
                              {command, send_file},
                              {file_name, RemoteFilePath},
                              {total_blocks, TotalBlocks},
                              {current_block, CurrentBlock},
                              {local_time, calendar:local_time()},
                              {pid, Pid},
                              {user_data, UserData}
                             ]),
    RoutingKey = hostkey_to_routing_key(Hostkey),
    lager:debug("RoutingKey: ~p",[RoutingKey]),
    Response = turtle:publish(file_transfer_publisher,
        <<"file_transfer">>,
        RoutingKey,
        <<"text/json">>,
        Message,
        #{ delivery_mode => persistent }),
    Response.

-spec send_file(LocalFilePath :: string(), RemoteFilePath :: string(), Hostkey :: string()) -> ok | error.
send_file(LocalFilePath, RemoteFilePath, Hostkey) ->
    lager:debug("LocalFilePath: ~p, Hostkey: ~p",[LocalFilePath,Hostkey]),
    try 
        {ok,IoDevice} = file:open(LocalFilePath, [read,binary,read_ahead,raw]),
        send_data(IoDevice,RemoteFilePath, Hostkey)
    after 
        file:close(LocalFilePath)
    end.

send_data(IoDevice,RemoteFilePath, Hostkey) ->
   TotalBlocks = number_blocks(RemoteFilePath),
   send_data(IoDevice,RemoteFilePath, Hostkey, TotalBlocks, 0).

send_data(IoDevice,RemoteFilePath, Hostkey, TotalBlocks, CurrentBlock) ->
   case file:read(IoDevice, ?BLOCK_SIZE) of
       {ok, Data} ->
           lager:debug("RemoteFilePath: ~p, CurrentBlock: ~p",[RemoteFilePath,CurrentBlock]),
           % Write Data to Socket
           NextBlock = CurrentBlock + 1,
           publish_file_send(Hostkey,RemoteFilePath,Data,TotalBlocks,NextBlock),
           send_data(IoDevice, RemoteFilePath, Hostkey,TotalBlocks,NextBlock);
       eof -> ok
   end.

number_blocks(RemoteFilePath) ->
    FileSize = case file:read_file_info(RemoteFilePath) of
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
