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
         execute_file/2,
         send_file/2
        ]).

-export([
         number_blocks/1
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
hostkey_to_routing_key(Hostkey) ->
        erlang:iolist_to_binary(["*.*.*.",Hostkey]).

execute_file(FilePath,Hostkey) ->
    publish_file_execute(Hostkey,FilePath).

-spec publish_file_execute(Hostkey :: string(), Filename :: string()) -> any().
publish_file_execute(Hostkey, Filename) ->
    %lager:info("IpsetExternalMap: ~p",[IpsetExternalMap]),
    Pid = erlang:self(),
    Message = term_to_binary([
                              {command, execute_file},
                              {file_name, Filename},
                              {local_time, calendar:local_time()},
                              {pid, Pid}
                             ]),
    RoutingKey = hostkey_to_routing_key(Hostkey),
    %Response = turtle:publish(file_transfer_publisher,
    %    <<"file_transfer">>,
    %    RoutingKey,
    %    <<"text/json">>,
    %    Message,
    %    #{ delivery_mode => persistent }),
    case turtle:rpc_sync(
           file_transfer_publisher, 
           <<"file_transfer">>, 
           RoutingKey, 
           <<"text/json">>,
           Message) of
              {error, Reason} -> 
                    %ReasonDecode = jsx:decode(Reason),
                    lager:error("Reason: ~p",[Reason]),
                    Reason;
              {ok, _NTime, _CType, Response} ->
                    ResponseDecode = jsx:decode(Response),
                    lager:info("Response: ~p",[ResponseDecode]), 
                    ResponseDecode
    end.

delete_file(FilePath,Hostkey) ->
    publish_file_delete(Hostkey,FilePath).

-spec publish_file_delete(Hostkey :: string(), Filename :: string()) -> any().
publish_file_delete(Hostkey, Filename) ->
    %lager:info("IpsetExternalMap: ~p",[IpsetExternalMap]),
    Pid = erlang:self(),
    Message = term_to_binary([
                              {command, delete_file},
                              {file_name, Filename},
                              {local_time, calendar:local_time()},
                              {pid, Pid}
                             ]),
    RoutingKey = hostkey_to_routing_key(Hostkey),
    %Response = turtle:publish(file_transfer_publisher,
    %    <<"file_transfer">>,
    %    RoutingKey,
    %    <<"text/json">>,
    %    Message,
    %    #{ delivery_mode => persistent }),
    %Response.
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

-spec publish_file_send(Hostkey :: string(), Filename :: string(),Data :: binary(), TotalBlocks :: integer(), CurrentBlock :: integer()) -> any().
publish_file_send(Hostkey, Filename, Data, TotalBlocks, CurrentBlock) ->
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
    RoutingKey = hostkey_to_routing_key(Hostkey),
    lager:debug("RoutingKey: ~p",[RoutingKey]),
    Response = turtle:publish(file_transfer_publisher,
        <<"file_transfer">>,
        RoutingKey,
        <<"text/json">>,
        Message,
        #{ delivery_mode => persistent }),
    Response.
    %case turtle:rpc_sync(
    %       file_transfer_publisher, 
    %       <<"file_transfer">>, 
    %       RoutingKey, 
    %       <<"text/json">>,
    %       Message) of
    %          {error, Reason} -> 
    %                lager:error("Reason: ~p",[Reason]),
    %                Reason;
    %          {ok, _NTime, _CType, Response} ->
    %                lager:info("Response: ~p",[Response]), 
    %                Response
    %end.

-spec send_file(Filename :: string(), Hostkey :: string()) -> ok | error.
send_file(Filename, Hostkey) ->
    lager:debug("Filename: ~p, Hostkey: ~p",[Filename,Hostkey]),
    try 
        {ok,IoDevice} = file:open(Filename, [read,binary,read_ahead,raw]),
        send_data(IoDevice,Filename, Hostkey)
    after 
        file:close(Filename)
    end.
    %{ok,IoDevice} = file:open(Filename, [read,binary,read_ahead,raw]),
    %Response = send_data(IoDevice,Filename, Hostkey),
    %lager:debug("Response: ~p",[Response]),
    %file:close(Filename).

send_data(IoDevice,Filename, Hostkey) ->
   TotalBlocks = number_blocks(Filename),
   send_data(IoDevice,Filename, Hostkey, TotalBlocks, 0).

send_data(IoDevice,Filename, Hostkey, TotalBlocks, CurrentBlock) ->
   case file:read(IoDevice, ?BLOCK_SIZE) of
       {ok, Data} ->
           lager:debug("Filename: ~p, CurrentBlock: ~p",[Filename,CurrentBlock]),
           % Write Data to Socket
           %send_data(Device, Socket)
           NextBlock = CurrentBlock + 1,
           publish_file_send(Hostkey,Filename,Data,TotalBlocks,NextBlock),
           send_data(IoDevice, Filename, Hostkey,TotalBlocks,NextBlock);
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
