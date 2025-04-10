-module(dog_file_transfer_api_v2).

-include("dog_trainer.hrl").

-export([
    init/2,
    terminate/3
]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([from_post_multipart/2]).
-export([from_post_json/2]).
-export([to_file/2]).
-export([to_json/2]).
-export([to_text/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

from_post_json(Req, State) ->
    ?LOG_DEBUG("Req: ~p", [Req]),
    Hostkey = cowboy_req:binding(id, Req),
    ApiUserName = cowboy_req:header(<<"x-consumer-username">>, Req),
    ConsumerCustomId = cowboy_req:header(<<"x-consumer-custom-id">>, Req),
    ConsumerId = cowboy_req:header(<<"x-consumer-id">>, Req),
    CredentialIdentifier = cowboy_req:header(<<"x-credential-identifier">>, Req),
    ?LOG_DEBUG("ApiUserName: ~p", [ApiUserName]),
    ?LOG_DEBUG("ConsumerCustomId: ~p", [ConsumerCustomId]),
    ?LOG_DEBUG("ConsumerId: ~p", [ConsumerId]),
    ?LOG_DEBUG("CredentialIdentifier: ~p", [CredentialIdentifier]),
    case dog_host:get_by_hostkey(Hostkey) of
        {error, notfound} ->
            Req@2 = cowboy_req:reply(
                200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(
                  #{Hostkey =>
                    #{retcode => 1, stdout => "" , stderr => <<"Hostkey not found">>
                            }}),
                Req
            ),
            {stop, Req@2, State};
        _ ->
            Body = cowboy_req:read_body(Req),
            ?LOG_DEBUG("Body: ~p", [Body]),
            {ok, Content, _} = Body,
            ?LOG_DEBUG("Content: ~p", [Content]),
            Message = jsx:decode(Content, [return_maps]),
            Response = handle_command(Hostkey, Message, ApiUserName),
            case Response of
                {error, StdErr} ->
                    Req@2 = cowboy_req:reply(
                        200,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(#{Hostkey => #{retcode => 1, stdout => <<"">>, stderr => #{error
                                                                                              =>
                                                                                              StdErr}}}),
                        Req
                    ),
                    {stop, Req@2, State};
                {ok, []} ->
                    Req@2 = cowboy_req:reply(
                        200,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(#{Hostkey => #{retcode => 0, stdout => <<"">>, stderr => #{}}}),
                        Req
                    ),
                    {stop, Req@2, State};
                {ok, StdOut} ->
                    Req@2 = cowboy_req:reply(
                        200,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(#{
                            Hostkey => #{
                                retcode => 0, stdout => string:trim(StdOut), stderr => #{}
                            }
                        }),
                        Req
                    ),
                    {stop, Req@2, State}
            end
    end.

-spec handle_command(Hostkey :: binary(), Message :: binary(), ApiUserName :: binary() ) -> {ok | error, iolist()}.
handle_command(Hostkey, Message, ApiUserName) ->
    ?LOG_DEBUG("Message: ~p", [Message]),
    Command = maps:get(<<"command">>, Message),
    UseShell = erlang:binary_to_atom(maps:get(<<"use_shell">>, Message, <<"false">>)),
    NewOpts =
        case (maps:is_key(<<"user">>, Message)) of
            true ->
                User = dog_common:to_list(maps:get(<<"user">>, Message)),
                [{use_shell, UseShell}, {api_user, ApiUserName}, {user, User}];
            false ->
                [{use_shell, UseShell}, {api_user, ApiUserName}]
        end,
    ?LOG_DEBUG("NewOpts: ~p", [NewOpts]),
    Result = dog_file_transfer:execute_command(Command, Hostkey, NewOpts),
    Result.

terminate(_Reason, _Req, _State) ->
    ok.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            {false, Req, State};
        <<"DELETE">> ->
            {true, Req, State};
        <<"GET">> ->
            Id = cowboy_req:binding(id, Req),
            ApiUserName = cowboy_req:header(<<"x-consumer-username">>, Req),
            ?LOG_DEBUG("ApiUserName: ~p", [ApiUserName]),
            Path =
                case cowboy_req:match_qs([{path, [], plain}], Req) of
                    #{path := Value} ->
                        Value;
                    _ ->
                        undefined
                end,
            ?LOG_DEBUG("ID: ~p, Path:~p", [Id, Path]),
            Opts = [{api_user, ApiUserName}],
            case dog_file_transfer:fetch_file(Path, Id, Opts) of
                {error, Error} ->
                    Req@2 = cowboy_req:reply(
                        200,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(#{error => Error}),
                        Req
                    ),
                    {stop, Req@2, State};
                Result ->
                    Req@2 = cowboy_req:reply(
                        200,
                        #{<<"content-type">> => <<"application/octet-stream">>},
                        Result,
                        Req
                    ),
                    {stop, Req@2, State}
            end
    end.

from_post_multipart(Req, State) ->
    ApiUserName = cowboy_req:header(<<"x-consumer-username">>, Req),
    Opts = [{api_user, ApiUserName}],
    Hostkey = cowboy_req:binding(id, Req),
    ?LOG_DEBUG("Hostkey= ~p~n", [Hostkey]),
    ?LOG_DEBUG("Req= ~p~n", [Req]),
    case dog_host:get_by_hostkey(Hostkey) of
        {error, notfound} ->
            Req@2 = cowboy_req:reply(
                403,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(<<"Hostkey not found">>),
                Req
            ),
            {stop, Req@2, State};
        _ ->
            {Result, Req@2} = acc_multipart(Hostkey, Req, [], Opts),
            ?LOG_DEBUG("Result= ~p~n", [Result]),
            ?LOG_DEBUG("Req@2= ~p~n", [Req@2]),
            ParsedResult = jsx:encode(
                lists:map(
                    fun(X) ->
                        element(1, X)
                    end,
                    Result
                )
            ),
            ?LOG_DEBUG("ParsedResult= ~p~n", [ParsedResult]),
            Req@3 = cowboy_req:reply(
                200,
                #{<<"content-type">> => <<"application/json">>},
                ParsedResult,
                Req@2
            ),
            ?LOG_DEBUG("Req@3: ~p", [Req@3]),
            {stop, Req@3, State}
    end.

acc_multipart(Hostkey, Req, Acc, Opts) ->
    case cowboy_req:read_part(Req) of
        {ok, Headers, Req2} ->
            [Req4, Body] =
                case cow_multipart:form_data(Headers) of
                    {data, _FieldName} ->
                        {ok, MyBody, Req3} = cowboy_req:read_part_body(Req2),
                        [Req3, MyBody];
                    {file, _FieldName, RemoteFilePath, CType} ->
                        ?LOG_DEBUG("stream_file filename=~p content_type=~p~n", [
                            RemoteFilePath, CType
                        ]),
                        UUID = entropy_string:session_id(),
                        FileLocationBase = erlang:list_to_binary(?FILE_LOCATION_BASE),
                        HostFilePath =
                        <<FileLocationBase/binary,Hostkey/binary,<<"/">>/binary,UUID/binary>>,
                        ?LOG_DEBUG("HostFilePath: ~p", [HostFilePath]),
                        LocalFilePath = <<HostFilePath/binary,<<"/send">>/binary,RemoteFilePath/binary>>,
                        ?LOG_DEBUG("LocalFilePath: ~p", [LocalFilePath]),
                        ok = filelib:ensure_dir(LocalFilePath),
                        {ok, IoDevice} = file:open(LocalFilePath, [raw, write, binary, sync]),
                        Req5 = stream_file(Req2, IoDevice),
                        ok = file:sync(IoDevice),
                        ok = file:close(IoDevice),
                        Response = dog_file_transfer:send_file(
                            HostFilePath, LocalFilePath, RemoteFilePath, Hostkey, Opts
                        ),
                        ?LOG_DEBUG(#{response => Response}),
                        [Req5, RemoteFilePath]
                end,
            acc_multipart(Hostkey, Req4, [{Headers, Body} | Acc], Opts);
        {done, Req2} ->
            {lists:reverse(Acc), Req2}
    end.

stream_file(Req, IoDevice) ->
    case cowboy_req:read_part_body(Req) of
        {ok, Body, Req2} ->
            ?LOG_DEBUG("part_body ok~n", []),
            file:write(IoDevice, Body),
            Req2;
        {more, Body, Req2} ->
            ?LOG_DEBUG("part_body more~n", []),
            file:write(IoDevice, Body),
            stream_file(Req2, IoDevice)
    end.

content_types_provided(Req, State) ->
    {
        [
            {<<"application/json">>, to_json},
            {<<"multipart/form-data">>, to_json},
            {<<"text/plain">>, to_text},
            {<<"application/octet-stream">>, to_file}
        ],
        Req,
        State
    }.

content_types_accepted(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            {
                [
                    {<<"multipart/form-data">>, from_post_multipart},
                    {<<"application/json">>, from_post_json}
                ],
                Req,
                State
            }
    end.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"DELETE">>, <<"GET">>], Req, State}.

to_text(Req, State) ->
    {State, Req, State}.

to_file(Req, State) ->
    {State, Req, State}.

to_json(Req, State) ->
    ?LOG_DEBUG("State: ~p~n", [State]),
    Json = jsx:encode(State),
    {Json, Req, State}.

delete_resource(Req@0, State) ->
    ApiUserName = cowboy_req:header(<<"x-consumer-username">>, Req@0),
    Id = cowboy_req:binding(id, Req@0),
    Path =
        case cowboy_req:match_qs([{path, [], plain}], Req@0) of
            #{path := Value} ->
                Value;
            _ ->
                undefined
        end,
    ?LOG_DEBUG("ID: ~p, Path:~p", [Id, Path]),
    Opts = [{api_user, ApiUserName}],
    {Result, Req@1} =
        case dog_file_transfer:delete_file(Path, Id, Opts) of
            ok ->
                {true, Req@0};
            timeout ->
                ErrorReq = cowboy_req:set_resp_body(jsx:encode(agent_timeout), Req@0),
                {false, ErrorReq};
            {error, Error} ->
                ErrorReq = cowboy_req:set_resp_body(jsx:encode(Error), Req@0),
                {false, ErrorReq}
        end,
    {Result, Req@1, State}.
