-module(dog_file_transfer).
-include("dog_trainer.hrl").

-include_lib("kernel/include/file.hrl").

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
         fetch_file/2,
         send_data/4,
         send_file/3
        ]).

-export([
         number_blocks/2
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
    ExecuteCommandBase64 = base64:encode(ExecuteCommand),
    Pid = erlang:self(),
    Message = term_to_binary([
                              {command, execute_command},
                              {execute_command, ExecuteCommandBase64 },
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
                    lager:debug("Payload: ~p",[Payload]),
                    case hd(jsx:decode(Payload)) of
                    %case Payload of
                      {<<"error">>,StdErr} ->
                        lager:error("StdErr: ~p",[StdErr]),
                        {error, StdErr};
                      {<<"ok">>,StdOut} ->  
                        lager:debug("StdOut: ~p",[StdOut]),
                        {ok, StdOut}
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
              {ok, _NTime, _CType, Payload} ->
          case hd(jsx:decode(Payload)) of
              {<<"error">>,Error} ->
                  {error, Error};
              <<"ok">> ->
                  ok
          end
    end.

-spec publish_file_send(Hostkey :: string(), RemoteFilePath :: string(),Data :: binary(), TotalBlocks :: integer(), CurrentBlock :: integer(), MaxBlockSizeBytes :: integer()) -> any().
publish_file_send(Hostkey, RemoteFilePath, Data, TotalBlocks, CurrentBlock, MaxBlockSizeBytes) ->
    UserData = #{
      file_block => Data
                },
    Pid = erlang:self(),
    Message = term_to_binary([
                              {command, send_file},
                              {file_name, RemoteFilePath},
                              {total_blocks, TotalBlocks},
                              {current_block, CurrentBlock},
                              {max_block_size_bytes, MaxBlockSizeBytes},
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
    MaxBlockSizeBytes = application:get_env(dog_trainer,max_block_size_bytes,134217728),
    lager:debug("LocalFilePath: ~p, Hostkey: ~p",[LocalFilePath,Hostkey]),
    try 
        {ok,IoDevice} = file:open(LocalFilePath, [read,binary,read_ahead,raw]),
        send_data(IoDevice,RemoteFilePath, Hostkey, MaxBlockSizeBytes)
    after 
        file:close(LocalFilePath)
    end.

send_data(IoDevice,RemoteFilePath, Hostkey, MaxBlockSizeBytes) ->
   TotalBlocks = number_blocks(RemoteFilePath, MaxBlockSizeBytes),
   send_data(IoDevice,RemoteFilePath, Hostkey, TotalBlocks, MaxBlockSizeBytes, 0).

send_data(IoDevice,RemoteFilePath, Hostkey, TotalBlocks, MaxBlockSizeBytes, CurrentBlock) ->
   case file:read(IoDevice, MaxBlockSizeBytes) of
       {ok, Data} ->
       lager:debug("RemoteFilePath: ~p, MaxBlockSizeBytes: ~p, CurrentBlock: ~p",[RemoteFilePath,MaxBlockSizeBytes,CurrentBlock]),
           % Write Data to Socket
           NextBlock = CurrentBlock + 1,
           publish_file_send(Hostkey,RemoteFilePath,Data,TotalBlocks,NextBlock, MaxBlockSizeBytes),
           send_data(IoDevice, RemoteFilePath, Hostkey, TotalBlocks, MaxBlockSizeBytes, NextBlock);
       eof -> ok
   end.

number_blocks(RemoteFilePath, MaxBlockSizeBytes) ->
    FileSize = case file:read_file_info(RemoteFilePath) of
        {ok, FileInfo} ->
            FullBlocks = erlang:floor(FileInfo#file_info.size / MaxBlockSizeBytes),
            case FileInfo#file_info.size rem MaxBlockSizeBytes of
               N when N > 0 ->
                   FullBlocks + 1;
               _ ->
                   FullBlocks
            end;
        {error, _Reason} ->
            0
    end,
    FileSize.

fetch_file(FilePath,Hostkey) ->
    publish_file_fetch(Hostkey,FilePath).

%Requires capability CAP_DAC_READ_SEARCH to read all files 
-spec publish_file_fetch(Hostkey :: string(), Filename :: string()) -> any().
publish_file_fetch(Hostkey, Filename) ->
    Pid = erlang:self(),
    Message = term_to_binary([
                              {command, fetch_file},
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
           Message,
           #{timeout => 20000 }) of
              {error, Reason} -> 
                    lager:error("Reason: ~p",[Reason]),
                    {error, Reason};
              {ok, _NTime, CType, Response} ->
                    lager:debug("CType: ~p",[CType]),
                    case CType of
                        <<"application/octet-stream">> -> 
                            LocalFilePath = ?FILE_LOCATION_BASE  ++ dog_common:to_list(Hostkey) ++ "/fetch/" ++ dog_common:to_list(Filename),
                            filelib:ensure_dir(filename:dirname(LocalFilePath) ++ "/"),                                                     
                            ok = file:write_file( LocalFilePath, Response, [raw, write, binary]),                                               
                            lager:info("Response size in bytes: ~p",[erlang:size(Response)]), 
                            Response;
                        <<"text/json">> ->
                            case hd(jsx:decode(Response)) of
                              {<<"error">>,StdErr} ->
                                lager:error("StdErr: ~p",[StdErr]),
                                {error, StdErr};
                              {<<"ok">>,StdOut} ->  
                                lager:debug("StdOut: ~p",[StdOut]),
                                {ok, StdOut}
                            end
                    end
    end.

%-define(HASH_BLOCK_SIZE,1024*1024*32).
%file_hash(Filename) ->
%    file_hash(Filename,Accum).
%
%file_hash(Filename,
%file_hash(Filename) ->
