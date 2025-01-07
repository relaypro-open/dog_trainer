-module(logt).

-export([
    proplist_format/2,
    proplist_lib_format/2,
    proplist_keys/1,
    proplist_log_to_map/2,
    proplist_values/1
]).

%is_proplist(List) ->
%    is_list(List) andalso
%        lists:all(fun({_, _}) -> true;
%                     (_)      -> false
%                  end,
%                  List).

proplist_keys(List) ->
    lists:map(fun({Key, _Value}) -> Key end, List).

proplist_values(List) ->
    lists:map(fun({_Key, Value}) -> Value end, List).
proplist_log_to_map(Format, Terms) ->
    Keys = lists:map(fun(Key) -> io_lib:format("$~s", [Key]) end, proplist_keys(Terms)),
    Values = proplist_values(Terms),
    KV = maps:from_list(Terms),
    ReportKV = #{
        unstructured_log =>
            unicode:characters_to_binary(io_lib:format(Format, Values)),
        template_log => unicode:characters_to_binary(
            io_lib:format(
                Format,
                Keys
            )
        )
    },
    maps:merge(ReportKV, KV).

proplist_lib_format(Format, Terms) ->
    Values = lists:map(fun(Value) -> io_lib:format("~p", [Value]) end, proplist_values(Terms)),
    io_lib:format(Format, Values).

proplist_format(Format, Terms) ->
    Values = lists:map(fun(Value) -> io_lib:format("~p", [Value]) end, proplist_values(Terms)),
    io:format(Format, Values).
