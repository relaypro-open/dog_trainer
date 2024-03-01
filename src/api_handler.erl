-module(api_handler).

-include("dog_trainer.hrl").

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([to_json/2]).
-export([to_text/2]).
-export([from_post_json/2]).
-export([from_put_json/2]).
-export([delete_resource/2]).
-export([resource_exists/2]).
-export([extract_module_from_path/3]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

extract_module_from_path(Path, Start, Length) ->
    Path@0 = erlang:binary_to_list(Path),
    Path@1 = lists:flatten(
        lists:join("/", lists:sublist(string:split(Path@0, "/", all), Start, Length))
    ),
    Path@2 = erlang:list_to_binary(Path@1),
    Path@2.

get_handler_path(Path) ->
    Item = extract_module_from_path(Path, 3, 1),
    erlang:binary_to_list(Item).

get_handler_module(Path) ->
    handler_lookup(extract_module_from_path(Path, 3, 1)).

handler_lookup(<<"external">>) -> dog_external;
handler_lookup(<<"group">>) -> dog_group;
handler_lookup(<<"host">>) -> dog_host;
handler_lookup(<<"link">>) -> dog_link;
handler_lookup(<<"profile">>) -> dog_profile;
handler_lookup(<<"service">>) -> dog_service;
handler_lookup(<<"zone">>) -> dog_zone;
handler_lookup(<<"ruleset">>) -> dog_ruleset;
handler_lookup(<<"fact">>) -> dog_fact.

content_types_provided(Req, State) ->
    {
        [
            {<<"application/json">>, to_json},
            {<<"text/plain">>, to_text}
        ],
        Req,
        State
    }.

content_types_accepted(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            {[{<<"application/json">>, from_post_json}], Req, State};
        <<"PUT">> ->
            {[{<<"application/json">>, from_put_json}], Req, State}
    end.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

resource_exists(Req@0, State@0) ->
    case cowboy_req:method(Req@0) of
        <<"POST">> ->
            Path = cowboy_req:path(Req@0),
            Handler = get_handler_module(Path),
            Body = cowboy_req:read_body(Req@0),
            ?LOG_INFO("~p", [Body]),
            {ok, NewContent, Req@1} = Body,
            Map = jsx:decode(NewContent, [return_maps]),
            ObjectName = maps:get(<<"name">>, Map),
            Object = Handler:get_by_name(ObjectName),
            Result =
                case Object of
                    {error, notfound} ->
                        false;
                    {validation_error, _Error} ->
                        false;
                    {ok, _} ->
                        true
                end,
            State@1 = maps:put(<<"object">>, Object, State@0),
            State@2 = maps:put(<<"content">>, NewContent, State@1),
            {Result, Req@1, State@2};
        <<"PUT">> ->
            Path = cowboy_req:path(Req@0),
            Handler = get_handler_module(Path),
            Body = cowboy_req:read_body(Req@0),
            ?LOG_DEBUG("~p", [Body]),
            {ok, NewContent, Req@1} = Body,
            Map = jsx:decode(NewContent, [return_maps]),
            ObjectName = maps:get(<<"name">>, Map),
            Object = Handler:get_by_name(ObjectName),
            Result =
                case Object of
                    {error, notfound} ->
                        false;
                    {validation_error, _Error} ->
                        false;
                    {ok, _} ->
                        true
                end,
            State@1 = maps:put(<<"object">>, Object, State@0),
            State@2 = maps:put(<<"content">>, NewContent, State@1),
            {Result, Req@1, State@2};
        <<"GET">> ->
            Path = cowboy_req:path(Req@0),
            Handler = get_handler_module(Path),
            Id = cowboy_req:binding(id, Req@0),
            case Id of
                undefined ->
                    QsVals = cowboy_req:parse_qs(Req@0),
                    NameTuple = lists:keyfind(<<"name">>, 1, QsVals),
                    HostkeyTuple = lists:keyfind(<<"hostkey">>, 1, QsVals),
                    case {NameTuple, HostkeyTuple} of
                        {{<<"name">>, Name}, false} ->
                            case Handler:get_by_name(Name) of
                                {ok, Object} ->
                                    State@1 = maps:put(<<"object">>, Object, State@0),
                                    {true, Req@0, State@1};
                                {error, _Error} ->
                                    {false, Req@0, State@0}
                            end;
                        {false, {<<"hostkey">>, Hostkey}} ->
                            case Handler:get_by_hostkey(Hostkey) of
                                {ok, Object2} ->
                                    State@2 = maps:put(<<"object">>, Object2, State@0),
                                    {true, Req@0, State@2};
                                {error, _Error2} ->
                                    {false, Req@0, State@0}
                            end;
                        {false, false} ->
                            {false, Req@0, State@0}
                    end;
                I ->
                    case Handler:get_by_id(I) of
                        {ok, Object} ->
                            State@1 = maps:put(<<"object">>, Object, State@0),
                            {true, Req@0, State@1};
                        {error, _Error} ->
                            {false, Req@0, State@0}
                    end
            end;
        <<"DELETE">> ->
            Path = cowboy_req:path(Req@0),
            Handler = get_handler_module(Path),
            Id = cowboy_req:binding(id, Req@0),
            case Handler:get_by_id(Id) of
                {ok, Object} ->
                    State@1 = maps:put(<<"object">>, Object, State@0),
                    {true, Req@0, State@1};
                {error, Error} ->
                    {false, Error, Req@0, State@0}
            end;
        _ ->
            {true, Req@0, State@0}
    end.

from_post_json(Req@0, State) ->
    Path = cowboy_req:path(Req@0),
    Handler = get_handler_module(Path),
    HandlerPath = get_handler_path(Path),
    Content = maps:get(<<"content">>, State),
    Map = jsx:decode(Content, [return_maps]),
    Object = maps:get(<<"object">>, State),
    case Object of
        {error, notfound} ->
            case Handler:create(Map) of
                {validation_error, Error} ->
                    cowboy_req:reply(
                        400,
                        #{<<"content-type">> => <<"application/json">>},
                        Error,
                        Req@0
                    );
                {ok, Id} ->
                    SuccessMap = #{<<"id">> => Id, <<"result">> => <<"created">>},
                    Req@1 = cowboy_req:set_resp_body([jsx:encode(SuccessMap)], Req@0),
                    Uri = io_lib:format("~s/~s/~s", [?ROOT, HandlerPath, erlang:binary_to_list(Id)]),
                    ?LOG_INFO("Uri: ~p~n", [Uri]),
                    {{true, list_to_binary(Uri)}, Req@1, State}
            end;
        {ok, Result} ->
            Id = maps:get(<<"id">>, Result),
            SuccessMap = #{<<"id">> => Id, <<"result">> => <<"exists">>},
            Req@1 = cowboy_req:set_resp_body([jsx:encode(SuccessMap)], Req@0),
            Uri = io_lib:format("~s/~s/~s", [?ROOT, HandlerPath, erlang:binary_to_list(Id)]),
            %303 redirect
            {{true, list_to_binary(Uri)}, Req@1, State};
        {validation_error, Error} ->
            cowboy_req:reply(
                400,
                #{<<"content-type">> => <<"application/json">>},
                Error,
                Req@0
            )
    end.

from_put_json(Req@0, State) ->
    Path = cowboy_req:path(Req@0),
    Handler = get_handler_module(Path),
    HandlerPath = get_handler_path(Path),
    Content = maps:get(<<"content">>, State),
    UpdateMap = jsx:decode(Content, [return_maps]),
    Id = cowboy_req:binding(id, Req@0),
    SuccessMap = #{<<"id">> => Id},
    Req@1 = cowboy_req:set_resp_body([jsx:encode(SuccessMap)], Req@0),
    InPlace = cowboy_req:match_qs([{inplace, [], plain}], Req@1),
    ?LOG_INFO("InPlace: ~p", [InPlace]),
    Response =
        case InPlace of
            #{inplace := <<"True">>} ->
                Handler:update(Id, UpdateMap, true);
            _ ->
                Handler:update(Id, UpdateMap)
        end,
    case Response of
        {false, Error} when is_atom(Error) ->
            ?LOG_INFO("{false, ~p}", [Error]),
            Req@2 = cowboy_req:set_resp_body([atom_to_list(Error)], Req@1),
            {false, Req@2, State};
        {false, ObjectId} ->
            ?LOG_INFO("{false, ~p}", [ObjectId]),
            SuccessMap2 = #{<<"id">> => ObjectId},
            Uri = io_lib:format("~s/~s/~s", [?ROOT, HandlerPath, binary_to_list(ObjectId)]),
            ?LOG_INFO("ObjectId: ~p", [ObjectId]),
            Req@2 = cowboy_req:reply(
                303,
                #{
                    <<"content-type">> => <<"application/json">>,
                    <<"location">> => Uri
                },
                binary_to_list(jsx:encode(SuccessMap2)),
                Req@1
            ),
            {stop, Req@2, State};
        {true, ObjectId} ->
            ?LOG_INFO("{true, ~p}", [ObjectId]),
            SuccessMap2 = #{<<"id">> => ObjectId},
            Uri = io_lib:format("~s/~s/~s", [?ROOT, HandlerPath, binary_to_list(ObjectId)]),
            ?LOG_INFO("ObjectId: ~p", [ObjectId]),
            Req@2 = cowboy_req:reply(
                303,
                #{
                    <<"content-type">> => <<"application/json">>,
                    <<"location">> => Uri
                },
                binary_to_list(jsx:encode(SuccessMap2)),
                Req@1
            ),
            {stop, Req@2, State};
        {validation_error, Error} ->
            cowboy_req:reply(
                400,
                #{<<"content-type">> => <<"application/json">>},
                Error,
                Req@1
            )
    end.

to_json(Req, State) ->
    Id@0 = cowboy_req:binding(id, Req),
    Sub = cowboy_req:binding(sub, Req),
    Object = maps:get(<<"object">>, State),
    Path = cowboy_req:path(Req),
    HandlerPath = get_handler_path(Path),
    Handler = get_handler_module(Path),
    Json =
        case HandlerPath of
            "group" ->
                case Id@0 of
                    undefined ->
                        jsx:encode(Object);
                    Id ->
                        case Sub of
                            undefined ->
                                jsx:encode(Object);
                            <<"ips">> ->
                                {ok, ObjectIps} = dog_group:get_all_ips_by_id(Id),
                                jsx:encode(ObjectIps);
                            <<"ipv4s">> ->
                                {ok, ObjectIps} = dog_group:get_all_ipv4s_by_id(Id),
                                jsx:encode(ObjectIps);
                            <<"ipv6s">> ->
                                {ok, ObjectIps} = dog_group:get_all_ipv6s_by_id(Id),
                                jsx:encode(ObjectIps);
                            <<"internal_ips">> ->
                                {ok, ObjectIps} = dog_group:get_internal_ips_by_id(Id),
                                jsx:encode(ObjectIps);
                            <<"internal_ipv4s">> ->
                                {ok, ObjectIps} = dog_group:get_internal_ipv4s_by_id(Id),
                                jsx:encode(ObjectIps);
                            <<"internal_ipv6s">> ->
                                {ok, ObjectIps} = dog_group:get_internal_ipv6s_by_id(Id),
                                jsx:encode(ObjectIps);
                            <<"external_ips">> ->
                                {ok, ObjectIps} = dog_group:get_external_ips_by_id(Id),
                                jsx:encode(ObjectIps);
                            <<"external_ipv4s">> ->
                                {ok, ObjectIps} = dog_group:get_external_ipv4s_by_id(Id),
                                jsx:encode(ObjectIps);
                            <<"external_ipv6s">> ->
                                {ok, ObjectIps} = dog_group:get_external_ipv6s_by_id(Id),
                                jsx:encode(ObjectIps);
                            <<"hosts">> ->
                                {ok, ObjectHosts} = dog_group:get_hosts_by_id(Id),
                                jsx:encode(ObjectHosts);
                            <<"ec2_security_group_ids">> ->
                                ObjectHosts = dog_group:get_internal_ec2_security_group_ids_by_id(Id),
                                jsx:encode(ObjectHosts);
                            <<"hcl">> ->
                                Handler:to_hcl_by_id(Id)
                        end
                end;
            _ ->
                case Id@0 of
                    undefined ->
                        jsx:encode(Object);
                    Id ->
                        case Sub of
                            undefined ->
                                jsx:encode(Object);
                            <<"hcl">> ->
                                Handler:to_hcl_by_id(Id)
                        end
                end
        end,
    {Json, Req, State}.

to_text(Req, State) ->
    Id = cowboy_req:binding(id, Req),
    Sub = cowboy_req:binding(sub, Req),
    Path = cowboy_req:path(Req),
    HandlerPath = get_handler_path(Path),
    Text =
        case Id of
            undefined ->
                error;
            Id ->
                case HandlerPath of
                    "profile" ->
                        case Sub of
                            undefined ->
                                case cowboy_req:match_qs([{diff, [], plain}], Req) of
                                    #{diff := plain} ->
                                        case dog_profile:get_by_id(Id) of
                                            {ok, Profile} ->
                                                {ok, T} = dog_profile:to_text(Profile),
                                                T;
                                            {error, Error} ->
                                                Error
                                        end;
                                    #{diff := DiffId} ->
                                        ProfileResult1 = dog_profile:get_by_id(Id),
                                        ProfileResult2 = dog_profile:get_by_id(DiffId),
                                        _ =
                                            case {ProfileResult1, ProfileResult2} of
                                                {{ok, Profile1}, {ok, Profile2}} ->
                                                    {ok, T1} = dog_profile:to_text(Profile1),
                                                    {ok, T2} = dog_profile:to_text(Profile2),
                                                    {ok, Diff} = diff:diff_text(T1, T2),
                                                    Diff;
                                                {{error, Error1}, {error, Error2}} ->
                                                    dog_common:to_list(Error1) ++
                                                        dog_common:to_list(Error2);
                                                {{error, Error1}, {ok, _Profile2}} ->
                                                    Error1;
                                                {{ok, _Profile1}, {error, Error2}} ->
                                                    Error2
                                            end
                                end,
                                case cowboy_req:match_qs([{git_diff, [], plain}], Req) of
                                    #{git_diff := plain} ->
                                        ?LOG_DEBUG("here ~n", []),
                                        case dog_profile:get_by_id(Id) of
                                            {ok, Profile_} ->
                                                {ok, T_} = dog_profile:to_text(Profile_),
                                                T_;
                                            {error, Error_} ->
                                                Error_
                                        end;
                                    #{git_diff := DiffId_} ->
                                        ?LOG_DEBUG("here WHAT~n", []),
                                        ProfileResult1_ = dog_profile:get_by_id(Id),
                                        ProfileResult2_ = dog_profile:get_by_id(DiffId_),
                                        case {ProfileResult1_, ProfileResult2_} of
                                            {{ok, Profile1_}, {ok, Profile2_}} ->
                                                {ok, T1_} = dog_profile:to_text(Profile1_),
                                                {ok, T2_} = dog_profile:to_text(Profile2_),
                                                {ok, Diff_} = diff:diff_git(T1_, T2_),
                                                Diff_;
                                            {{error, Error1_}, {error, Error2_}} ->
                                                dog_common:to_list(Error1_) ++
                                                    dog_common:to_list(Error2_);
                                            {{error, Error1_}, {ok, _Profile2_}} ->
                                                Error1_;
                                            {{ok, _Profile1_}, {error, Error2_}} ->
                                                Error2_
                                        end
                                end
                        end;
                    "group" ->
                        case Sub of
                            undefined ->
                                error;
                            _ ->
                                case cowboy_req:match_qs([{git_diff, [], plain}], Req) of
                                    #{git_diff := plain} ->
                                        case Sub of
                                            <<"iptablesv4">> ->
                                                {_, Iptables} = dog_profile:generate_ipv4_iptables_ruleset_by_group_id(
                                                    Id
                                                ),
                                                Iptables;
                                            <<"iptablesv6">> ->
                                                {_, Iptables} = dog_profile:generate_ipv4_iptables_ruleset_by_group_id(
                                                    Id
                                                ),
                                                Iptables;
                                            <<"ipsetsv4">> ->
                                                {Ipsets, _} = dog_profile:generate_ipv4_iptables_ruleset_by_group_id(
                                                    Id
                                                ),
                                                Ipsets;
                                            <<"ipsetsv6">> ->
                                                {Ipsets, _} = dog_profile:generate_ipv6_iptables_ruleset_by_group_id(
                                                    Id
                                                ),
                                                Ipsets
                                        end;
                                    #{git_diff := DiffId_} ->
                                        ProfileResult1_ = dog_profile:get_by_id(Id),
                                        ProfileResult2_ = dog_profile:get_by_id(DiffId_),
                                        case {ProfileResult1_, ProfileResult2_} of
                                            {{ok, Profile1_}, {ok, Profile2_}} ->
                                                {ok, T1_} = dog_profile:to_text(Profile1_),
                                                {ok, T2_} = dog_profile:to_text(Profile2_),
                                                {ok, Diff_} = diff:diff_git(T1_, T2_),
                                                Diff_;
                                            {{error, Error1_}, {error, Error2_}} ->
                                                dog_common:to_list(Error1_) ++
                                                    dog_common:to_list(Error2_);
                                            {{error, Error1_}, {ok, _Profile2_}} ->
                                                Error1_;
                                            {{ok, _Profile1_}, {error, Error2_}} ->
                                                Error2_
                                        end
                                end
                        end;
                    _ ->
                        error
                end
        end,
    {Text, Req, State}.

delete_resource(Req@0, State) ->
    Path = cowboy_req:path(Req@0),
    Handler = get_handler_module(Path),
    Id = cowboy_req:binding(id, Req@0),
    {Result, Req@1} =
        case Handler:delete(Id) of
            ok ->
                {true, Req@0};
            {error, Error} ->
                ErrorReq = cowboy_req:set_resp_body(jsx:encode(Error), Req@0),

                {false, ErrorReq}
        end,
    {Result, Req@1, State}.
