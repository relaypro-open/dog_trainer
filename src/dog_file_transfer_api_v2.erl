-module(dog_file_transfer_api_v2).
%-behaviour(cowboy_rest).

-include("dog_trainer.hrl").


-export([
         init/2, 
         %handle/2, 
         terminate/3
        ]).
%-export([allowed_methods/2]).
%-export([content_types_accepted/2]).
%-export([content_types_provided/2]).
%-export([from_post_multipart/2]).
%-export([to_json/2]).

%API
%-export([
%         create/1,
%         delete/1
%        ]).

init(Req, Opts) ->
	%{cowboy_rest, Req, Opts}.
  from_post_multipart(Req, Opts).
  %{ok, Req, Opts}.


%AcceptCallback(Req, State) -> {Result, Req, State}

from_post_multipart(Req, State) ->
  Hostkey = cowboy_req:binding(id, Req),
  lager:debug( "Hostkey= ~p~n", [Hostkey] ),
  lager:debug( "Req= ~p~n", [Req] ),
  case dog_host:get_by_hostkey(Hostkey) of
    {error,notfound} ->
      cowboy_req:reply(404, 
                       #{<<"content-type">> => <<"application/json">>},
                       jsx:encode(<<"Hostkey not found">>),
                       Req),
      {ok, Req, State};
    _ ->
      {Result, Req2} = acc_multipart(Hostkey, Req, []),
      lager:debug( "Result= ~p~n", [Result] ),
      {ok, Req2, State}
  end.

terminate(_Reason, _Req, _State) ->
  ok.

acc_multipart(Hostkey, Req, Acc) ->
  case cowboy_req:read_part(Req) of
    {ok, Headers, Req2} ->
            [Req4, Body] = case cow_multipart:form_data(Headers) of
                       {data, _FieldName} ->
                         {ok, MyBody, Req3} = cowboy_req:part_body(Req2),
                         [Req3, MyBody];
                       {file, _FieldName, Filename, CType} ->
                         lager:debug("stream_file filename=~p content_type=~p~n", [Filename, CType]),
                         LocalFilePath = ?FILE_LOCATION_BASE  ++ dog_common:to_list(Hostkey) ++ "/send/" ++ dog_common:to_list(Filename),
                         filelib:ensure_dir(filename:dirname(LocalFilePath) ++ "/"),
                         {ok, IoDevice} = file:open( LocalFilePath, [raw, write, binary]),
                         Req5=stream_file(Req2, IoDevice),
                         file:close(IoDevice),
                         dog_file_transfer:send_file(LocalFilePath, Filename,Hostkey),
                         [Req5, Filename]
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

%content_types_provided(Req, State) ->
%	{[
%		{<<"application/json">>, to_json}
%	], Req, State}.
%
%content_types_accepted(Req, State) ->
%    case cowboy_req:method(Req) of
%            
%        <<"POST">> ->
%            {[ 
%              {<<"multipart/form-data">>, from_post_multipart}
%             ], Req, State}
%    end.
%
%allowed_methods(Req, State) ->
%        {[<<"POST">>, <<"DELETE">>], Req, State}.
%
%to_json(Req, State) ->
%  lager:debug("State: ~p~n",[State]),
%    %Id = cowboy_req:binding(id, Req),
%    %Sub = cowboy_req:binding(sub, Req),
%    %Object = maps:get(<<"object">>,State),
%    Json = jsx:encode(State),
%    {Json, Req, State}.

%from_post_multipart(Req@0, State) ->
%    Path = cowboy_req:path(Req@0),
%    %_Handler = get_handler_module(Path),
%    %_HandlerPath = get_handler_path(Path),
%    lager:debug("State: ~p~n",[State]),
%    lager:debug("Path: ~p~n",[Path]).

%-spec create(Group :: map()) -> {ok | error, Key :: iolist() | name_exists }.
%create(ZoneMap@0) ->
%  pass.
%{ok, ZoneMap@1} = dog_zone:cleanup(ZoneMap@0),
%Name = maps:get(<<"name">>, ZoneMap@1),
%{ok, ExistingZones} = get_all(),
%ExistingNames = [maps:get(<<"name">>,Zone) || Zone <- ExistingZones],
%case lists:member(Name, ExistingNames) of
%    false ->
%        {ok, R} = dog_rethink:run(
%                                  fun(X) -> 
%                                          reql:db(X, dog),
%                                          reql:table(X, ?TYPE_TABLE),
%                                          reql:insert(X, ZoneMap@1,#{return_changes => always})
%                                  end),
%        NewVal = maps:get(<<"new_val">>,hd(maps:get(<<"changes">>,R))),
%        {ok, NewVal};
%    true ->
%        {error, name_exists}
%end.

%-spec delete(ZoneId :: binary()) -> ok | {error, Error :: map()}.
%delete(Id) ->
%  pass.
%case dog_zone:in_active_profile(Id) of
%      {false,[]} -> 
%          {ok, R} = dog_rethink:run(
%                                    fun(X) -> 
%                                            reql:db(X, dog),
%                                            reql:table(X, ?TYPE_TABLE),
%                                            reql:get(X, Id),
%                                            reql:delete(X)
%                                    end),
%          lager:debug("delete R: ~p~n",[R]),
%          Deleted = maps:get(<<"deleted">>, R),
%          case Deleted of
%              1 -> ok;
%              _ -> {error,#{<<"error">> => <<"error">>}}
%          end;
%      {true,Profiles} ->
%          lager:info("zone ~p not deleted, in profiles: ~p~n",[Id,Profiles]),
%          {error,#{ <<"errors">> => #{<<"in active profile">> => Profiles}}}
%   end.
