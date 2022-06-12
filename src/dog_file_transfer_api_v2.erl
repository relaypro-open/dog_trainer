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
    lager:debug("Req: ~p", [Req]),
    Hostkey = cowboy_req:binding(id, Req),
    case dog_host:get_by_hostkey(Hostkey) of
        {error,notfound} ->
            Req@2 = cowboy_req:reply(404, 
                                     #{<<"content-type">> => <<"application/json">>},
                                     jsx:encode(<<"Hostkey not found">>),
                                     Req),
            {stop, Req@2, State};
        _ ->
            Body = cowboy_req:read_body(Req),
            lager:debug("Body: ~p", [Body]),
            {ok, Content, _} = Body,
            lager:debug("Content: ~p", [Content]),
            Message = jsx:decode(Content,[return_maps]),
            Response = handle_command(Hostkey,Message),
            case Response of
                {error,StdErr} ->
                    Req@2 = cowboy_req:reply(400, 
                                             #{<<"content-type">> => <<"application/json">>},
                                             jsx:encode(#{Hostkey => #{retcode => 1, stdout => <<"">>, stderr => StdErr}}),
                                             Req),
                    {stop, Req@2, State};
                {ok,[]} ->
                    Req@2 = cowboy_req:reply(200,
                                             #{<<"content-type">> => <<"application/json">>},
                                             jsx:encode(#{Hostkey => #{retcode => 0, stdout => <<"">>, stderr => <<"">>}}),
                                             Req),
                    {stop, Req@2, State};
                {ok,StdOut} ->
                    Req@2 = cowboy_req:reply(200,
                                             #{<<"content-type">> => <<"application/json">>},
                                             jsx:encode(#{Hostkey => #{retcode => 0, stdout => string:trim(StdOut), stderr => <<"">>}}),
                                             Req),
                    {stop, Req@2, State}
            end
    end.

handle_command(Hostkey,Message) ->
  lager:debug("Message: ~p",[Message]),
  Command = maps:get(<<"command">>,Message),
  UseShell = erlang:binary_to_atom(maps:get(<<"use_shell">>,Message,<<"false">>)),
  User = dog_common:to_list(maps:get(<<"user">>,Message,"dog")),
  NewOpts = [{use_shell, UseShell},{user, User}],
  lager:debug("NewOpts: ~p",[NewOpts]),
  CommandBase64 = base64:encode(Command),
  dog_file_transfer:execute_command(CommandBase64,Hostkey,NewOpts).

terminate(_Reason, _Req, _State) ->
  ok.

resource_exists(Req, State) ->
  case cowboy_req:method(Req) of
    <<"POST">> -> 
      {false,Req,State};
    <<"DELETE">> -> 
      {true,Req,State};
    <<"GET">> ->
      Id = cowboy_req:binding(id, Req),
      Path = case cowboy_req:match_qs([{path, [], plain}], Req) of
          #{path := Value} ->
                 Value;
          _ -> undefined
      end,
      lager:debug("ID: ~p, Path:~p",[Id,Path]),
      case dog_file_transfer:fetch_file(Path,Id) of
          timeout ->
              Req@2 = cowboy_req:reply(500, 
                                       #{<<"content-type">> => <<"application/json">>},
                                       jsx:encode(#{error => timeout}),
                                       Req),
              {stop, Req@2, State};
          {error, Error} -> 
              Req@2 = cowboy_req:reply(400, 
                                       #{<<"content-type">> => <<"application/json">>},
                                       jsx:encode(#{error => Error}),
                                       Req),
              {stop, Req@2, State};
          Result ->
              Req@2 = cowboy_req:reply(200, 
                                       #{<<"content-type">> => <<"application/octet-stream">> },
                                       Result,
                                       Req),
              {stop, Req@2, State}
      end
  end.

from_post_multipart(Req, State) ->
  Hostkey = cowboy_req:binding(id, Req),
  lager:debug( "Hostkey= ~p~n", [Hostkey] ),
  lager:debug( "Req= ~p~n", [Req] ),
  case dog_host:get_by_hostkey(Hostkey) of
    {error,notfound} ->
      Req@2 = cowboy_req:reply(404, 
                       #{<<"content-type">> => <<"application/json">>},
                       jsx:encode(<<"Hostkey not found">>),
                       Req),
      {stop, Req@2, State};
    _ ->
      {Result, Req@2} = acc_multipart(Hostkey, Req, []),
      lager:debug( "Result= ~p~n", [Result] ),
      lager:debug( "Req@2= ~p~n", [Req@2] ),
      ParsedResult = jsx:encode(lists:map(fun(X) ->
                                        element(1,X)
                                 end,Result)),
      lager:debug( "ParsedResult= ~p~n", [ParsedResult] ),
      Req@3 = cowboy_req:reply(200, 
                       #{<<"content-type">> => <<"application/json">>},
                       ParsedResult,
                       Req@2),
      {stop, Req@3, State}
  end.

acc_multipart(Hostkey, Req, Acc) ->
  case cowboy_req:read_part(Req) of
    {ok, Headers, Req2} ->
            [Req4, Body] = case cow_multipart:form_data(Headers) of
                       {data, _FieldName} ->
                         {ok, MyBody, Req3} = cowboy_req:part_body(Req2),
                         [Req3, MyBody];
                       {file, _FieldName, RemoteFilePath, CType} ->
                         lager:debug("stream_file filename=~p content_type=~p~n", [RemoteFilePath, CType]),
                         LocalFilePath = ?FILE_LOCATION_BASE  ++ dog_common:to_list(Hostkey) ++ "/send/" ++ dog_common:to_list(RemoteFilePath),
                         filelib:ensure_dir(filename:dirname(LocalFilePath) ++ "/"),
                         {ok, IoDevice} = file:open( LocalFilePath, [raw, write, binary]),
                         Req5=stream_file(Req2, IoDevice),
                         file:close(IoDevice),
                         dog_file_transfer:send_file(LocalFilePath, RemoteFilePath,Hostkey),
                         [Req5, RemoteFilePath]
                     end,
      acc_multipart(Hostkey, Req4, [{Headers, Body}|Acc]);
    {done, Req2} ->
      {lists:reverse(Acc), Req2}
  end.

stream_file(Req, IoDevice) ->
  case cowboy_req:read_part_body(Req) of
    {ok, Body, Req2} ->
      lager:debug("part_body ok~n", []),
      file:write(IoDevice, Body),
      Req2;
    {more, Body, Req2} ->
      lager:debug("part_body more~n", []),
      file:write(IoDevice, Body),
      stream_file(Req2, IoDevice)
  end.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, to_json},
		{<<"multipart/form-data">>, to_json},
		{<<"text/plain">>, to_text},
        {<<"application/octet-stream">>, to_file}
	], Req, State}.

content_types_accepted(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            {[ 
              {<<"multipart/form-data">>, from_post_multipart},
              {<<"application/json">>, from_post_json}
             ], Req, State}
    end.

allowed_methods(Req, State) ->
        {[<<"POST">>, <<"DELETE">>, <<"GET">>], Req, State}.

to_text(Req, State) ->
  {State,Req,State}.

to_file(Req, State) ->
  {State,Req,State}.

to_json(Req, State) ->
  lager:debug("State: ~p~n",[State]),
    %Id = cowboy_req:binding(id, Req),
    %Sub = cowboy_req:binding(sub, Req),
    %Object = maps:get(<<"object">>,State),
    Json = jsx:encode(State),
    {Json, Req, State}.

delete_resource(Req@0, State) ->
  Id = cowboy_req:binding(id, Req@0),
  Path = case cowboy_req:match_qs([{path, [], plain}], Req@0) of
      #{path := Value} ->
             Value;
      _ -> 
          undefined
  end,
  lager:debug("ID: ~p, Path:~p",[Id,Path]),
  {Result,Req@1} = case dog_file_transfer:delete_file(Path,Id) of
                     ok -> 
                       {true,Req@0};
                     timeout ->
                       ErrorReq = cowboy_req:set_resp_body(jsx:encode(agent_timeout),Req@0),
                       {false,ErrorReq};
                     {error, Error} -> 
                       ErrorReq = cowboy_req:set_resp_body(jsx:encode(Error),Req@0),
                       {false,ErrorReq}
                   end,
  {Result, Req@1, State}.
