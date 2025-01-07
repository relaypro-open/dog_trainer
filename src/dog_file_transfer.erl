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
    delete_file/3,
    execute_command/2,
    execute_command/3,
    fetch_file/3,
    send_data/5,
    send_file/4
]).

-export([
    number_blocks/2
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
hostkey_to_routing_key(Hostkey) ->
    erlang:iolist_to_binary(["*.*.*.", Hostkey]).

execute_command(ExecuteCommand, Hostkey) ->
    execute_command(ExecuteCommand, Hostkey, []).

execute_command(ExecuteCommand, Hostkey, Opts) ->
    imetrics:add_m(file_transfer, execute_command),
    publish_execute_command(Hostkey, ExecuteCommand, Opts).

-spec publish_execute_command(Hostkey :: string(), ExecuteCommand :: string(), Opts :: list()) ->
    {ok | error, iolist()}.
publish_execute_command(Hostkey, ExecuteCommand, Opts) ->
    ExecuteCommandBase64 = base64:encode(ExecuteCommand),
    Pid = erlang:self(),
    Message = term_to_binary(
        [
            {command, execute_command},
            {execute_command, ExecuteCommandBase64},
            {local_time, calendar:local_time()},
            {pid, Pid}
        ] ++ Opts
    ),
    RoutingKey = hostkey_to_routing_key(Hostkey),
    CommandExecutionTimeout = application:get_env(dog_trainer, command_execution_timeout_ms, 60000),
    Response =
        case
            turtle:rpc_sync(
                file_transfer_publisher,
                <<"file_transfer">>,
                RoutingKey,
                <<"text/json">>,
                Message,
                #{timeout => CommandExecutionTimeout}
            )
        of
            {error, Reason} ->
                ?LOGT_ERROR("Reason: ~p, RoutingKey: ~p",[{reason,Reason}, {routing_key,RoutingKey}]),
                {error, Reason};
            {ok, _NTime, _CType, Payload} ->
                ?LOGT_DEBUG("Payload: ~p, RoutingKey: ~p",[{payload,Payload},
                                                           {routing_key,RoutingKey}]),
                case decode_payload(Payload) of
                    {<<"error">>, StdErr} ->
                        ?LOGT_ERROR("StdErr: ~p, RoutingKey: ~p",[{stderr,StdErr},{routing_key,RoutingKey}]),
                        {error, StdErr};
                    {<<"ok">>, StdOut} ->
                        ?LOGT_INFO("StdOut: ~p, RoutingKey: ~p",[{stdout,StdOut},{routing_key,RoutingKey}]),
                        {ok, string:trim(StdOut, trailing, "\n")}
                end
        end,
    Response.

decode_payload(Payload) ->
    {Response, Message} = hd(jsx:decode(Payload)),
    {Response, Message}.

delete_file(FilePath, Hostkey, Opts) ->
    imetrics:add_m(file_transfer, delete_file),
    publish_file_delete(Hostkey, FilePath, Opts).

-spec publish_file_delete(Hostkey :: string(), Filename :: string(), Opts :: list()) -> any().
publish_file_delete(Hostkey, Filename, Opts) ->
    FileDeleteTimeout = application:get_env(dog_trainer, file_delete_timeout_ms, 60000),
    Pid = erlang:self(),
    Message = term_to_binary(
        [
            {command, delete_file},
            {file_name, Filename},
            {local_time, calendar:local_time()},
            {pid, Pid}
        ] ++ Opts
    ),
    RoutingKey = hostkey_to_routing_key(Hostkey),
    case
        turtle:rpc_sync(
            file_transfer_publisher,
            <<"file_transfer">>,
            RoutingKey,
            <<"text/json">>,
            Message,
            #{timeout => FileDeleteTimeout}
        )
    of
        {error, Reason} ->
            ?LOGT_ERROR("Reason: ~p, RoutingKey: ~p",[{reason,Reason}, {routing_key,RoutingKey}]),
            Reason;
        {ok, _NTime, _CType, Payload} ->
            case hd(jsx:decode(Payload)) of
                {<<"error">>, Error} ->
                    {error, Error};
                <<"ok">> ->
                    ok
            end
    end.

-spec publish_file_send(
    Hostkey :: string(),
    RemoteFilePath :: string(),
    Data :: binary(),
    TotalBlocks :: integer(),
    CurrentBlock :: integer(),
    MaxBlockSizeBytes :: integer(),
    Opts :: list()
) -> any().
publish_file_send(
    Hostkey, RemoteFilePath, Data, TotalBlocks, CurrentBlock, MaxBlockSizeBytes, Opts
) ->
    UserData = #{
        file_block => Data
    },
    Pid = erlang:self(),
    Message = term_to_binary(
        [
            {command, send_file},
            {file_name, RemoteFilePath},
            {total_blocks, TotalBlocks},
            {current_block, CurrentBlock},
            {max_block_size_bytes, MaxBlockSizeBytes},
            {local_time, calendar:local_time()},
            {pid, Pid},
            {user_data, UserData}
        ] ++ Opts
    ),
    RoutingKey = hostkey_to_routing_key(Hostkey),
    ?LOGT_ERROR("RoutingKey: ~p",[{routing_key,RoutingKey}]),
    Response = turtle:publish(
        file_transfer_publisher,
        <<"file_transfer">>,
        RoutingKey,
        <<"text/json">>,
        Message,
        #{delivery_mode => persistent}
    ),
    Response.

-spec send_file(
    LocalFilePath :: string(), RemoteFilePath :: string(), Hostkey :: string(), Opts :: list()
) -> ok | error.
send_file(LocalFilePath, RemoteFilePath, Hostkey, Opts) ->
    imetrics:add_m(file_transfer, send_file),
    MaxBlockSizeBytes = application:get_env(dog_trainer, max_block_size_bytes, 134217728),
    ?LOGT_DEBUG("LocalFilePath: ~p, HostKey: ~p",[{localfilepath,LocalFilePath},{hostkey,Hostkey}]),
    {ok, IoDevice} = file:open(LocalFilePath, [read, binary, read_ahead, raw]),
    send_data(IoDevice, RemoteFilePath, Hostkey, MaxBlockSizeBytes, Opts),
    ok = file:close(IoDevice).

send_data(IoDevice, RemoteFilePath, Hostkey, MaxBlockSizeBytes, Opts) ->
    TotalBlocks = number_blocks(RemoteFilePath, MaxBlockSizeBytes),
    send_data(IoDevice, RemoteFilePath, Hostkey, TotalBlocks, MaxBlockSizeBytes, 0, Opts).

send_data(IoDevice, RemoteFilePath, Hostkey, TotalBlocks, MaxBlockSizeBytes, CurrentBlock, Opts) ->
    case file:read(IoDevice, MaxBlockSizeBytes) of
        {ok, Data} ->
            ?LOGT_DEBUG("RemoteFilePath: ~p, MacBlockSizeFiles: ~p, CurrentBlock: ~p, TotalBlocks: ~p",[{remotefilepath,RemoteFilePath}, {maxblocksizebytes,MaxBlockSizeBytes}, {currentblock,CurrentBlock}, {total_blocks,TotalBlocks}]),
            % Write Data to Socket
            NextBlock = CurrentBlock + 1,
            publish_file_send(
                Hostkey, RemoteFilePath, Data, TotalBlocks, NextBlock, MaxBlockSizeBytes, Opts
            ),
            send_data(
                IoDevice, RemoteFilePath, Hostkey, TotalBlocks, MaxBlockSizeBytes, NextBlock, Opts
            );
        eof ->
            ok
    end.

number_blocks(RemoteFilePath, MaxBlockSizeBytes) ->
    FileSize =
        case file:read_file_info(RemoteFilePath) of
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

fetch_file(FilePath, Hostkey, Opts) ->
    imetrics:add_m(file_transfer, fetch_file),
    publish_file_fetch(Hostkey, FilePath, Opts).

%Requires capability CAP_DAC_READ_SEARCH to read all files
-spec publish_file_fetch(Hostkey :: string(), Filename :: string(), Opts :: list()) -> any().
publish_file_fetch(Hostkey, Filename, Opts) ->
    Pid = erlang:self(),
    Message = term_to_binary(
        [
            {command, fetch_file},
            {file_name, Filename},
            {local_time, calendar:local_time()},
            {pid, Pid}
        ] ++ Opts
    ),
    RoutingKey = hostkey_to_routing_key(Hostkey),
    FileTransferTimeout = application:get_env(dog_trainer, file_transfer_timeout_ms, 60000),
    case
        turtle:rpc_sync(
            file_transfer_publisher,
            <<"file_transfer">>,
            RoutingKey,
            <<"text/json">>,
            Message,
            #{timeout => FileTransferTimeout}
        )
    of
        {error, Reason} ->
            ?LOGT_ERROR("Reason: ~p",[{reason,Reason}]),
            {error, Reason};
        {ok, _NTime, CType, Response} ->
            ?LOGT_ERROR("CType: ~p",[{ctype,CType}]),
            case CType of
                <<"application/octet-stream">> ->
                    LocalFilePath =
                        ?FILE_LOCATION_BASE ++ dog_common:to_list(Hostkey) ++ "/fetch/" ++
                            dog_common:to_list(Filename),
                    filelib:ensure_dir(filename:dirname(LocalFilePath) ++ "/"),
                    ok = file:write_file(LocalFilePath, Response, [raw, write, binary]),
                    ?LOGT_INFO("Response size in bytes: ~p", [erlang:size(Response)]),
                    Response;
                <<"text/json">> ->
                    case hd(jsx:decode(Response)) of
                        {<<"error">>, StdErr} ->
                            ?LOGT_ERROR("StdErr: ~p, RoutingKey: ~p",[{stderr,StdErr},{routing_key,RoutingKey}]),
                            {error, StdErr};
                        {<<"ok">>, StdOut} ->
                            ?LOGT_INFO("StdOut: ~p, RoutingKey: ~p",[{stdout,StdOut},{routing_key,RoutingKey}]),
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
