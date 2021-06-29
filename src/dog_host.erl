-module(dog_host).

-include("dog_trainer.hrl").

-define(VALIDATION_TYPE, <<"host">>).
-define(TYPE_TABLE, host).

%API
-export([
         create/1,
         delete/1,
         get_by_hostkey/1,
         get_by_id/1, 
         get_by_name/1,
         get_all/0,
         get_all_active/0,
         get_schema/0,
         lmm/2,
         rkmm/3,
         update/2,
         update_by_hostkey/2,
         update_by_name/2
        ]).

-export([
         check_hashes/1,
         hash_age_check/0,
         hash_age_check/1,
         get_active_by_id/1,
         get_id_by_name/1,
         get_id_by_hostkey/1,
         get_all_ips/0,
         get_all_active_interfaces/0,
         hash_check/0,
         hash_logic/5,
         init/0, 
         keepalive_age_check/0,
         keepalive_age_check/1,
         keepalive_check/0,
         keepalive_check/1,
         retirement_check/0,
         retirement_check/1,
         send_hash_alert/2,
         send_hash_recover/2,
         send_keepalive_alert/1,
         send_keepalive_recover/1,
         send_retirement_alert/1,
         set_active_by_id/1,
         set_active_by_name/1,
         set_hosts_active/1,
         set_hosts_inactive/1,
         set_hosts_retired/1,
         set_inactive_by_id/1,
         set_inactive_by_name/1,
         set_retired_by_name/1,
         set_retired_by_id/1,
         update_active/2,
         update_hash_alert_sent/2,
         update_keepalive_alert_sent/2,
         all_hash_alerted/0,
         all_keepalive_alerted/0
        ]).

-export([
         group_hashes/0,
         host_hashes/0
        ]).

-spec keepalive_check() ->{ok, Unalive :: list()}.
keepalive_check() ->
    Now =  erlang:system_time(second),
    {ok, KeepAliveAlertSeconds} = application:get_env(dog_trainer,keepalive_alert_seconds),
    TimeCutoff = Now - KeepAliveAlertSeconds,
    lager:debug("Now: ~p",[calendar:system_time_to_rfc3339(Now)]),
    lager:debug("TimeCutoff: ~p",[calendar:system_time_to_rfc3339(TimeCutoff)]),
    keepalive_check(TimeCutoff).

-spec keepalive_check(TimeCutoff :: number()) -> {ok,list()}.
keepalive_check(TimeCutoff) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
	fun(X) -> 
		reql:db(X, dog), 
		reql:table(X, ?TYPE_TABLE),
        %reql:filter(X,#{<<"active">> => <<"active">>}),
        reql:pluck(X, [<<"id">>,<<"name">>,<<"keepalive_timestamp">>])
	end),
    {ok, ResultTime} = rethink_cursor:all(R),
    lager:debug("ResultTime: ~p",[ResultTime]),
    R1 = lists:flatten(ResultTime),
    Ids = [maps:get(<<"id">>,X) || X <- R1],
    Names = [maps:get(<<"name">>,X) || X <- R1],
    Timestamps = [ maps:get(<<"keepalive_timestamp">>,X) || X <- R1],
    ZippedList = lists:zip3(Ids,Names,Timestamps),
    OldAgents = [#{<<"id">> => Id,<<"name">> => Name,<<"keepalive_timestamp">> => TimeStamp} || {Id,Name,TimeStamp} <- ZippedList, TimeStamp < TimeCutoff],
    lager:info("OldAgents: ~p",[OldAgents]),
    {ok, OldAgents}.

-spec retirement_check() ->{ok, Unalive :: list()}.
retirement_check() ->
    Now =  erlang:system_time(second),
    KeepAliveAlertSeconds = application:get_env(dog_trainer,retirement_alert_seconds,86400),
    TimeCutoff = Now - KeepAliveAlertSeconds,
    lager:debug("Now: ~p",[calendar:system_time_to_rfc3339(Now)]),
    lager:debug("TimeCutoff: ~p",[calendar:system_time_to_rfc3339(TimeCutoff)]),
    retirement_check(TimeCutoff).

-spec retirement_check(TimeCutoff :: number()) -> {ok,list()}.
retirement_check(TimeCutoff) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
	fun(X) -> 
		reql:db(X, dog), 
		reql:table(X, ?TYPE_TABLE),
        reql:filter(X,#{<<"active">> => <<"inactive">>}),
        reql:pluck(X, [<<"id">>,<<"name">>,<<"keepalive_timestamp">>])
	end),
    {ok, ResultTime} = rethink_cursor:all(R),
    lager:debug("ResultTime: ~p",[ResultTime]),
    R1 = lists:flatten(ResultTime),
    Ids = [maps:get(<<"id">>,X) || X <- R1],
    Names = [maps:get(<<"name">>,X) || X <- R1],
    Timestamps = [ maps:get(<<"keepalive_timestamp">>,X) || X <- R1],
    ZippedList = lists:zip3(Ids,Names,Timestamps),
    OldAgents = [#{<<"id">> => Id,<<"name">> => Name,<<"keepalive_timestamp">> => TimeStamp} || {Id,Name,TimeStamp} <- ZippedList, TimeStamp < TimeCutoff],
    lager:info("OldAgents: ~p",[OldAgents]),
    {ok, OldAgents}.

-spec group_hashes() -> {ok, map()}.
group_hashes() ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
	fun(X) -> 
		reql:db(X, dog), 
		reql:table(X,group),
%        reql:has_fields(X,[<<"hash4_ipsets">>,<<"hash6_ipsets">>,<<"hash4_iptables">>,<<"hash6_iptables">>]),
        reql:pluck(X,[<<"name">>,<<"hash4_ipsets">>, <<"hash6_ipsets">>, <<"hash4_iptables">>, <<"hash6_iptables">>]) 
    end),
    {ok, GroupResult} = rethink_cursor:all(R),
    R1 = lists:flatten(GroupResult),
    Groups = [ A || A <- R1],
    GroupsList = lists:map(fun(G) -> 
                      [{maps:get(<<"name">>,G),maps:remove(<<"name">>,G)}] end, Groups),
    GroupsMap = maps:from_list(lists:flatten(GroupsList)),
    {ok, GroupsMap}.

-spec host_hashes() -> list().
host_hashes() ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
	fun(X) -> 
		reql:db(X, dog), 
		reql:table(X,host),
        reql:has_fields(X,[<<"group">>,<<"hash4_ipsets">>,<<"hash6_ipsets">>,<<"hash4_iptables">>,<<"hash6_iptables">>,<<"ipset_hash">>]),
        reql:filter(X,#{<<"active">> => <<"active">>}),
        reql:filter(X,fun(Y) -> reql:bracket(Y, <<"group">>), reql:ne(Y, <<"">>) end),
        reql:pluck(X,[<<"name">>,<<"group">>,<<"hash4_ipsets">>,<<"hash6_ipsets">>,<<"hash4_iptables">>,<<"hash6_iptables">>,<<"ipset_hash">>])
    end),
    {ok, HostResult} = rethink_cursor:all(R),
    R1 = lists:flatten(HostResult),
    Hosts = [ A || A <- R1],
    Hosts.

-spec update_hash_pass(SuccesHosts :: list()) -> ok.
update_hash_pass(SuccessHosts) ->
    lager:info("SuccessHosts: ~p",[SuccessHosts]),
    lists:foreach(fun(HostName) ->
        %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
        %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
        {ok, R} = dog_rethink:run(
              fun(X) ->
                      reql:db(X, dog),
                      reql:table(X, ?TYPE_TABLE),
                      reql:get_all(X, HostName, #{index => <<"name">>}),
                      reql:update(X, #{<<"hash_timestamp">> => dog_time:timestamp()} )
                  end),
        lager:debug("update R: ~p~n", [R]),
        Replaced = maps:get(<<"replaced">>, R),
        Unchanged = maps:get(<<"unchanged">>, R),
        case {Replaced,Unchanged} of
            {1,0} -> {true,HostName};
            {0,1} -> {false,HostName};
            _ -> {false, no_updated}
        end
    end, SuccessHosts),
    ok.

lmm(ListOfMaps, Key) ->
    list_of_maps_to_map(ListOfMaps, Key, #{}).

list_of_maps_to_map([],_Key,MapAcc) ->
    MapAcc;
list_of_maps_to_map(ListOfMaps,Key,MapAcc) ->
    KeyValue = maps:get(Key,hd(ListOfMaps)),
    NewMap = maps:remove(Key,hd(ListOfMaps)),
    MapAcc@1 = maps:put(KeyValue,NewMap,MapAcc),
    list_of_maps_to_map(tl(ListOfMaps),Key,MapAcc@1).

%> MM = #{drew => #{test => rest, a => b}, bob => #{test => zest, a=> c}}.
%> rkmm(MM,a,name).                                           
% #{b => #{name => drew,test => rest},
%   c => #{name => bob,test => zest}}
rkmm(MapOfMaps,NewKey,OldKeysNewKey) ->
    Iterator = maps:iterator(MapOfMaps),
    Next = maps:next(Iterator),
    rekey_map_of_maps(Next,NewKey,OldKeysNewKey,#{}).

rekey_map_of_maps(none,_NewKey,_OldKeysNewKey,MapAcc) ->
    MapAcc;
rekey_map_of_maps(Iterator,NewKey,OldKeysNewKey,MapAcc) ->
    {OldKey,OldValue,ThisIterator} = Iterator,
    io:format("Iterator: ~p~n",[Iterator]),
    NewKeyValue = maps:get(NewKey,OldValue),
    NewMap@0 = maps:remove(NewKey,OldValue),
    NewMap@1 = maps:put(OldKeysNewKey,OldKey,NewMap@0),
    MapAcc@1 = maps:put(NewKeyValue,NewMap@1,MapAcc),
    NewIterator = maps:next(ThisIterator),
    rekey_map_of_maps(NewIterator,NewKey,OldKeysNewKey,MapAcc@1).
    

-spec hash_check() -> {ok, map(), map()}.
hash_check() ->
    {ok, GroupsMap} = group_hashes(),
    Hosts = host_hashes(),
    TrainerIpsetHash = dog_ipset:read_hash(),
    HostChecks = lists:map(fun(Host) ->
                Group = maps:get(<<"group">>,Host),
                Hostname = maps:get(<<"name">>,Host),
                HostHash4Ipsets = maps:get(<<"hash4_ipsets">>,Host),
                HostHash6Ipsets = maps:get(<<"hash6_ipsets">>,Host),
                HostHash4Iptables = maps:get(<<"hash4_iptables">>,Host),
                HostHash6Iptables = maps:get(<<"hash6_iptables">>,Host),
                HostIpsetHash = maps:get(<<"ipset_hash">>,Host),
                GroupHash4Ipsets =  maps:get(<<"hash4_ipsets">>,maps:get(Group,GroupsMap)),
                GroupHash6Ipsets =  maps:get(<<"hash6_ipsets">>,maps:get(Group,GroupsMap)),
                GroupHash4Iptables =  maps:get(<<"hash4_iptables">>,maps:get(Group,GroupsMap)),
                GroupHash6Iptables =  maps:get(<<"hash6_iptables">>,maps:get(Group,GroupsMap)),
                Match4IpsetsCheck = (HostHash4Ipsets == GroupHash4Ipsets),
                Match6IpsetsCheck = (HostHash6Ipsets == GroupHash6Ipsets),
                Match4IptablesCheck = (HostHash4Iptables == GroupHash4Iptables),
                Match6IptablesCheck = (HostHash6Iptables == GroupHash6Iptables),
                MatchIpsetHashCheck = (HostIpsetHash == TrainerIpsetHash),
                #{<<"name">> => Hostname, 
                 <<"hash4_ipsets">> => Match4IpsetsCheck,
                 <<"hash6_ipsets">> => Match6IpsetsCheck, 
                 <<"hash4_iptables">> => Match4IptablesCheck,
                 <<"hash6_iptables">> => Match6IptablesCheck,
                 <<"ipset_hash">> => MatchIpsetHashCheck
                 }
              end, Hosts),
    {SuccessChecksList,FailedChecksList} = check_hashes(HostChecks),
    SuccessHosts = [maps:get(<<"name">>,Check) || Check <- SuccessChecksList],
    _FailedHosts = [maps:get(<<"name">>,Check) || Check <- FailedChecksList],
    SuccessChecks = lmm(SuccessChecksList,<<"name">>),
    FailedChecks = lmm(FailedChecksList,<<"name">>),
    send_hash_metrics(FailedChecks),
    lager:debug("SuccessChecks: ~p",[SuccessChecks]),
    lager:debug("FailedChecks: ~p",[FailedChecks]),
    update_hash_pass(SuccessHosts),
    {ok, SuccessChecks, FailedChecks}.

send_hash_metrics(FailedChecks) ->
    MetricNames = [<<"hash4_iptables">>,<<"hash6_iptables">>,<<"hash4_ipsets">>,<<"hash6_ipsets">>,<<"ipset_hash">>],
    Metrics = lists:map(fun(MetricName) ->
        MetricNumber = length([HostName || {HostName,Map} <- maps:to_list(FailedChecks), maps:get(MetricName,Map) == false]),
        {MetricName, MetricNumber} end, MetricNames),    
    imetrics:set_gauge(<<"hash_failures">>,Metrics),
    ok.

-spec hash_age_check() -> {ok, Unalive :: list()}.
hash_age_check() ->
    Now =  erlang:system_time(second),
    KeepAliveAlertSeconds = application:get_env(dog_trainer,hashcheck_alert_seconds,30),
    TimeCutoff = Now - KeepAliveAlertSeconds,
    lager:debug("Now: ~p",[calendar:system_time_to_rfc3339(Now)]),
    lager:debug("TimeCutoff: ~p",[calendar:system_time_to_rfc3339(TimeCutoff)]),
    hash_age_check(TimeCutoff).

-spec hash_age_check(TimeCutoff :: number()) -> {ok, list()}.
hash_age_check(TimeCutoff) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:filter(X,#{<<"active">> => <<"active">>}),
            reql:filter(X,fun(Y) -> reql:bracket(Y, <<"hash_timestamp">>), reql:lt(Y,<<"keepalive_timestamp">>) end),
            reql:pluck(X,[<<"id">>,<<"name">>,<<"hash_timestamp">>,<<"keepalive_timestamp">>])
        end),
    {ok, ResultTime} = rethink_cursor:all(R),
    lager:info("ResultTime: ~p",[ResultTime]),
    R1 = lists:flatten(ResultTime),
    Ids = [maps:get(<<"id">>,X) || X <- R1],
    Names = [maps:get(<<"name">>,X) || X <- R1],
    Timestamps = [ maps:get(<<"hash_timestamp">>,X) || X <- R1],
    ZippedList = lists:zip3(Ids,Names,Timestamps),
    OldAgents = [#{<<"id">> => Id,<<"name">> => Name,<<"hash_timestamp">> => TimeStamp} || {Id,Name,TimeStamp} <- ZippedList, TimeStamp < TimeCutoff],
    lager:info("OldAgents: ~p",[OldAgents]),
    {ok, OldAgents}.

-spec keepalive_age_check() -> {ok, Unalive :: list()}.
keepalive_age_check() ->
    Now =  erlang:system_time(second),
    KeepAliveAlertSeconds = application:get_env(dog_trainer,keepalive_alert_seconds,1800),
    TimeCutoff = Now - KeepAliveAlertSeconds,
    lager:debug("Now: ~p",[calendar:system_time_to_rfc3339(Now)]),
    lager:debug("TimeCutoff: ~p",[calendar:system_time_to_rfc3339(TimeCutoff)]),
    keepalive_age_check(TimeCutoff).

-spec keepalive_age_check(TimeCutoff :: number()) -> {ok, list()}.
keepalive_age_check(TimeCutoff) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:filter(X,#{<<"active">> => <<"active">>}),
            reql:pluck(X,[<<"id">>,<<"name">>,<<"keepalive_timestamp">>])
        end),
    {ok, ResultTime} = rethink_cursor:all(R),
    lager:info("ResultTime: ~p",[ResultTime]),
    R1 = lists:flatten(ResultTime),
    Ids = [maps:get(<<"id">>,X) || X <- R1],
    Names = [maps:get(<<"name">>,X) || X <- R1],
    Timestamps = [ maps:get(<<"keepalive_timestamp">>,X) || X <- R1],
    ZippedList = lists:zip3(Ids,Names,Timestamps),
    OldAgents = [#{<<"id">> => Id,<<"name">> => Name,<<"keepalive_timestamp">> => TimeStamp} || {Id,Name,TimeStamp} <- ZippedList, TimeStamp < TimeCutoff],
    lager:info("OldAgents: ~p",[OldAgents]),
    {ok, OldAgents}.

-spec all_keepalive_alerted() -> [binary()].
all_keepalive_alerted() ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:filter(X,#{<<"keepalive_alert_sent">> => true}),
            reql:get_field(X,<<"name">>)
        end),
    {ok, ResultTime} = rethink_cursor:all(R),
    lager:info("ResultTime: ~p",[ResultTime]),
    R1 = lists:flatten(ResultTime),
    R1.

-spec all_hash_alerted() -> [binary()].
all_hash_alerted() ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:filter(X,#{<<"hash_alert_sent">> => true}),
            reql:get_field(X,<<"name">>)
        end),
    {ok, ResultTime} = rethink_cursor:all(R),
    lager:info("ResultTime: ~p",[ResultTime]),
    R1 = lists:flatten(ResultTime),
    R1.

-spec check_hashes(HostChecks :: list()) -> {list(), list()}.
check_hashes(HostChecks) ->
    lager:info("HostChecks: ~p",[HostChecks]),
    lists:partition(fun(Host) ->
                HashCheck4Ipsets = maps:get(<<"hash4_ipsets">>,Host),
                HashCheck6Ipsets = maps:get(<<"hash6_ipsets">>,Host),
                HashCheck4Iptables = maps:get(<<"hash4_iptables">>,Host),
                HashCheck6Iptables = maps:get(<<"hash6_iptables">>,Host),
                HashCheckIpset = maps:get(<<"ipset_hash">>,Host),
                %TODO: enable when ipv6 ruleset generation fixed
                hash_logic(HashCheck4Ipsets,HashCheck6Ipsets,HashCheck4Iptables,HashCheck6Iptables,HashCheckIpset) end, HostChecks).

-spec hash_logic(HashCheck4Ipsets :: boolean(), HashCheck6Ipsets :: boolean(), HashCheck4Iptables :: boolean(), HashCheck6Iptables :: boolean(), HashCheckIpset :: boolean() ) -> boolean().
hash_logic(HashCheck4Ipsets, HashCheck6Ipsets, HashCheck4Iptables, HashCheck6Iptables,HashCheckIpset) ->
    CheckV6Hashes = application:get_env(dog_trainer,check_v6_hashes,true),
    case CheckV6Hashes of
        true ->
            HashCheck4Ipsets and HashCheck6Ipsets and HashCheck4Iptables and HashCheck6Iptables and HashCheckIpset;
        false ->
            HashCheck4Ipsets and HashCheck4Iptables and HashCheckIpset
    end.

-spec send_retirement_alert(Hosts :: list()) -> ok.
send_retirement_alert(Hosts) ->
    lager:info("Retirement alert sent: ~p",[Hosts]),
    HostStrings = [ binary:bin_to_list(H) || H <- Hosts],
    {ok, SmtpRelay} = application:get_env(dog_trainer,smtp_relay),
    {ok, SmtpUsername} = application:get_env(dog_trainer,smtp_username),
    {ok, SmtpPassword} = application:get_env(dog_trainer,smtp_password),
    Subject = io_lib:format("Dog Agent Retired: ~p",[HostStrings]),
    {ok,From} = application:get_env(dog_trainer,smtp_from),
    {ok,Addresses} = application:get_env(dog_trainer,smtp_to),
    To = string:join(Addresses,","),
    KeepAliveAlertSeconds = application:get_env(dog_trainer,retirement_alert_seconds,86400),
    Body = io_lib:format("Hosts that haven't communicated in last ~p seconds: ~p", [KeepAliveAlertSeconds,HostStrings]),
    Email = io_lib:format("Subject: ~s\r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s",[Subject,From,To,Body]),
    gen_smtp_client:send({From, Addresses, Email},
                       [{relay, SmtpRelay}, {username, SmtpUsername}, {password, SmtpPassword},
                        {tls,always}]),
    imetrics:add_m(alert,"retirement"),
    ok.

-spec send_keepalive_alert(Hosts :: list()) -> ok.
send_keepalive_alert(Hosts) ->
    lager:info("Keepalive alert sent: ~p",[Hosts]),
    HostStrings = [ binary:bin_to_list(H) || H <- Hosts],
    {ok, SmtpRelay} = application:get_env(dog_trainer,smtp_relay),
    {ok, SmtpUsername} = application:get_env(dog_trainer,smtp_username),
    {ok, SmtpPassword} = application:get_env(dog_trainer,smtp_password),
    Subject = io_lib:format("Dog Not Checked In: ~p",[HostStrings]),
    {ok,From} = application:get_env(dog_trainer,smtp_from),
    {ok,Addresses} = application:get_env(dog_trainer,smtp_to),
    To = string:join(Addresses,","),
    {ok, KeepAliveAlertSeconds} = application:get_env(dog_trainer,keepalive_alert_seconds),
    Body = io_lib:format("Hosts that haven't communicated in last ~p seconds: ~p", [KeepAliveAlertSeconds,HostStrings]),
    Email = io_lib:format("Subject: ~s\r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s",[Subject,From,To,Body]),
    gen_smtp_client:send({From, Addresses, Email},
                       [{relay, SmtpRelay}, {username, SmtpUsername}, {password, SmtpPassword},
                        {tls,always}]),
    imetrics:add_m(alert,"keepalive_fail"),
    ok.

-spec send_keepalive_recover(Hosts :: list()) -> ok.
send_keepalive_recover(Hosts) ->
    lager:info("Keepalive recover alert sent: ~p",[Hosts]),
    HostStrings = [ binary:bin_to_list(H) || H <- Hosts],
    {ok, SmtpRelay} = application:get_env(dog_trainer,smtp_relay),
    {ok, SmtpUsername} = application:get_env(dog_trainer,smtp_username),
    {ok, SmtpPassword} = application:get_env(dog_trainer,smtp_password),
    Subject = io_lib:format("Dog Agents Reconnected: ~p",[HostStrings]),
    {ok,From} = application:get_env(dog_trainer,smtp_from),
    {ok,Addresses} = application:get_env(dog_trainer,smtp_to),
    To = string:join(Addresses,","),
    {ok, KeepAliveAlertSeconds} = application:get_env(dog_trainer,keepalive_alert_seconds),
    Body = io_lib:format("Hosts reconnected in last ~p seconds: ~p", [KeepAliveAlertSeconds,HostStrings]),
    Email = io_lib:format("Subject: ~s\r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s",[Subject,From,To,Body]),
    gen_smtp_client:send({From, Addresses, Email},
                       [{relay, SmtpRelay}, {username, SmtpUsername}, {password, SmtpPassword},
                        {tls,always}]),
    imetrics:add_m(alert,"keepalive_recover"),
    ok.

-spec send_hash_alert(Hosts :: list(), FailedChecks :: map()) -> ok.
send_hash_alert(Hosts,FailedChecks) ->
    HostStrings = [ binary:bin_to_list(H) || H <- Hosts],
    lager:info("Hash alert sent: ~p",[HostStrings]),
    {ok, SmtpRelay} = application:get_env(dog_trainer,smtp_relay),
    {ok, SmtpUsername} = application:get_env(dog_trainer,smtp_username),
    {ok, SmtpPassword} = application:get_env(dog_trainer,smtp_password),
    Subject = io_lib:format("Dog Locally Modified On: ~p",[HostStrings]),
    {ok,From} = application:get_env(dog_trainer,smtp_from),
    {ok,Addresses} = application:get_env(dog_trainer,smtp_to),
    To = string:join(Addresses,","),
    Body = io_lib:format("Host's iptables and/or ipsets modified outside of dog: ~p\r\n", [FailedChecks]),
    Email = io_lib:format("Subject: ~s\r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s",[Subject,From,To,Body]),
    gen_smtp_client:send({From, Addresses, Email},
                       [{relay, SmtpRelay}, {username, SmtpUsername}, {password, SmtpPassword},
                        {tls,always}]),
    imetrics:add_m(alert,"hash_fail"),
    ok.

-spec send_hash_recover(Hosts :: list(), FailedChecks :: map()) -> ok.
send_hash_recover(Hosts,_FailedChecks) ->
    HostStrings = [ binary:bin_to_list(H) || H <- Hosts],
    lager:info("Hash recover alert sent: ~p",[HostStrings]),
    {ok, SmtpRelay} = application:get_env(dog_trainer,smtp_relay),
    {ok, SmtpUsername} = application:get_env(dog_trainer,smtp_username),
    {ok, SmtpPassword} = application:get_env(dog_trainer,smtp_password),
    Subject = io_lib:format("Dog Back In Control On: ~p",[HostStrings]),
    {ok,From} = application:get_env(dog_trainer,smtp_from),
    {ok,Addresses} = application:get_env(dog_trainer,smtp_to),
    To = string:join(Addresses,","),
    Body = io_lib:format("Host's iptables and/or ipsets back in sync with dog: ~p\r\n", [Hosts]),
    Email = io_lib:format("Subject: ~s\r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s",[Subject,From,To,Body]),
    gen_smtp_client:send({From, Addresses, Email},
                       [{relay, SmtpRelay}, {username, SmtpUsername}, {password, SmtpPassword},
                        {tls,always}]),
    imetrics:add_m(alert,"hash_recover"),
    ok.

-spec init() -> any(). 
init() ->
  pass.

-spec get_document_by_id(binary()) -> {ok, map()} | {error, atom()}.
get_document_by_id(Id) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
                       fun(X) -> 
                               reql:db(X, dog), 
                               reql:table(X, ?TYPE_TABLE),
                               reql:get(X, Id)
                        end),
    case R of
        null -> {error, notfound};
        _ -> {ok, R}
    end.

-spec get_by_name(binary()) -> {ok, map()} | {error, notfound}.
get_by_name(Name) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
                               fun(X) -> 
                                       reql:db(X, dog), 
                                       reql:table(X, ?TYPE_TABLE),
                                       reql:get_all(X, Name, #{index => <<"name">>})
                               end),
    {ok, R3} = rethink_cursor:all(R),
    Result = lists:flatten(R3),
    case Result of
        [] -> {error, notfound};
        _ -> {ok, hd(Result)}
    end.

-spec get_by_hostkey(binary()) -> {ok, map()} | {error, notfound}.
get_by_hostkey(Name) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
                               fun(X) -> 
                                       reql:db(X, dog), 
                                       reql:table(X, ?TYPE_TABLE),
                                       reql:get_all(X, Name, #{index => <<"hostkey">>})
                               end),
    {ok, R3} = rethink_cursor:all(R),
    Result = lists:flatten(R3),
    case Result of
        [] -> {error, notfound};
        _ -> {ok, hd(Result)}
    end.

-spec get_by_id(binary()) -> {ok, map()} | {error, atom()}.
get_by_id(HostId) ->
    case get_document_by_id(HostId) of
        {ok, Host} -> {ok, Host};
        {error, Error} -> {error, Error}
    end.

-spec get_all() -> {ok, list()}.
get_all() ->
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    %{ok, R} = dog_rethink:run(
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, ?TYPE_TABLE),
                                      reql:pluck(X, [<<"name">>,<<"id">>,<<"group">>,<<"active">>,<<"hostkey">>])
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Hosts = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    {ok, Hosts}.

-spec get_all_active() -> {ok, list()}.
get_all_active() ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, ?TYPE_TABLE),
                                      reql:filter(X,#{<<"active">> => <<"active">>}),
                                      reql:pluck(X, [<<"name">>,<<"id">>,<<"group">>])
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Hosts = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    {ok, Hosts}.

-spec create(Group :: map()) -> {ok | error, Key :: iolist() | name_exists }.
create(HostMap@0) ->
    Name = maps:get(<<"name">>, HostMap@0),
    Hostkey = maps:get(<<"hostkey">>, HostMap@0, notfound),
    case Hostkey of
      notfound ->
                   {error, no_hostkey};
      _ -> 
          {ok, ExistingHosts} = get_all(),
          ExistingHostkeys = [maps:get(<<"hostkey">>,Host) || Host <- ExistingHosts],
          DefaultValuesHostMap = #{
                          <<"hash_timestamp">> => <<"">>,
                          <<"keepalive_timestamp">> => <<"">>,
                          <<"hash_alert_sent">> => <<"">>,
                          <<"keepalive_alert_sent">> =>  <<"">>,
                          <<"location">> => <<"*">>,
                          <<"environment">> => <<"*">>,
                          <<"hostkey">> => <<"">> },
          MergedHostMap = maps:merge(DefaultValuesHostMap, HostMap@0),

          case lists:member(Name, ExistingHostkeys) of
              false ->
                  %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
                  %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
                  {ok, R} = dog_rethink:run(
                                            fun(X) -> 
                                                    reql:db(X, dog),
                                                    reql:table(X, ?TYPE_TABLE),
                                                    reql:insert(X, MergedHostMap)
                                            end),
                  Key = hd(maps:get(<<"generated_keys">>,R)),
                  {ok, Key};
              true ->
                  {error, name_exists}
          end
    end.

-spec update(Id :: binary(), UpdateMap :: map()) -> {ok, iolist()} | {false, iolist()} | {false, no_updated} | {validation_error, iolist()} .
update(Id, UpdateMap) ->
    case get_by_id(Id) of
        {ok, OldService} ->
            NewService = maps:merge(OldService,UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE,NewService) of
                ok ->
                    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
                    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
                    {ok, R} = dog_rethink:run(
                          fun(X) -> 
                                  reql:db(X, dog),
                                  reql:table(X, ?TYPE_TABLE),
                                  reql:get(X, Id),
                                  reql:update(X,UpdateMap)
                              end),
                    lager:debug("update R: ~p~n", [R]),
                    Replaced = maps:get(<<"replaced">>, R),
                    Unchanged = maps:get(<<"unchanged">>, R),
                    case {Replaced,Unchanged} of
                        {1,0} -> {true,Id};
                        {0,1} -> {false,Id};
                        _ -> {false, no_updated}
                    end;
                {error, Error} ->
                    Response = dog_parse:validation_error(Error),
                    {validation_error, Response}
            end;
        {error, Error} ->
            {false, Error}
    end.

-spec update_by_hostkey(HostKey :: binary(), UpdateMap :: map()) -> no_return().
update_by_hostkey(HostKey, UpdateMap) ->
    case get_id_by_hostkey(HostKey) of
        {ok, Id} -> 
            update(Id, UpdateMap);
        {error, Reason} -> 
            {error, Reason}
    end.

-spec update_by_name(HostName :: binary(), UpdateMap :: map()) -> no_return().
update_by_name(HostName, UpdateMap) ->
    case get_id_by_name(HostName) of
        {ok, Id} -> 
            update(Id, UpdateMap);
        {error, Reason} -> 
            {error, Reason}
    end.

-spec delete(Id :: binary()) -> (ok | error).
delete(Id) ->
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog),
                                      reql:table(X, ?TYPE_TABLE),
                                      reql:get(X, Id),
                                      reql:delete(X)
                              end),
    lager:debug("delete R: ~p~n",[R]),
    Deleted = maps:get(<<"deleted">>, R),
    case Deleted of
        1 -> ok;
        _ -> error
    end.

-spec update_keepalive_alert_sent(Name :: binary(), State :: boolean() ) -> { true, iolist() } | {false, no_updated}.
update_keepalive_alert_sent(Name, State) ->
    lager:info("Setting keepalive_alert_sent: ~p, ~p",[Name,State]),
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
          fun(X) -> 
                  reql:db(X, dog),
                  reql:table(X, ?TYPE_TABLE),
                  reql:get_all(X, Name, #{index => <<"name">>}),
                  reql:update(X,#{<<"keepalive_alert_sent">> => State})
          end),
    Replaced = maps:get(<<"replaced">>, R),
    Unchanged = maps:get(<<"unchanged">>, R),
    case {Replaced,Unchanged} of
        {1,0} -> {true,Name};
        {0,1} -> {false,Name};
        _ -> {false, no_updated}
    end.

-spec update_hash_alert_sent(Name :: binary(), State :: boolean() ) -> { true, iolist() } | {false, no_updated}.
update_hash_alert_sent(Name, State) ->
    lager:info("Setting hash_alert_sent: ~p, ~p",[Name,State]),
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
          fun(X) -> 
                  reql:db(X, dog),
                  reql:table(X, ?TYPE_TABLE),
                  reql:get_all(X, Name, #{index => <<"name">>}),
                  reql:update(X,#{<<"hash_alert_sent">> => State})
          end),
    Replaced = maps:get(<<"replaced">>, R),
    Unchanged = maps:get(<<"unchanged">>, R),
    case {Replaced,Unchanged} of
        {1,0} -> {true,Name};
        {0,1} -> {false,Name};
        _ -> {false, no_updated}
    end.

-spec update_active(Id :: binary(), ActiveState :: binary() ) -> { true, binary() } | {false, no_updated}.
update_active(Id, ActiveState) ->
    lager:info("Setting agent active state: ~p, ~p",[Id,ActiveState]),
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
          fun(X) -> 
                  reql:db(X, dog),
                  reql:table(X, ?TYPE_TABLE),
                  reql:get(X, Id),
                  reql:update(X,#{<<"active">> => ActiveState})
          end),
    Replaced = maps:get(<<"replaced">>, R),
    Unchanged = maps:get(<<"unchanged">>, R),
    case {Replaced,Unchanged} of
        {1,0} -> {true,Id};
        {0,1} -> {false,Id};
        _ -> {false, no_updated}
    end.

-spec get_id_by_hostkey(Hostkey :: binary()) -> {ok, binary()} | {error, atom()}.
get_id_by_hostkey(Hostkey) ->
    case get_by_hostkey(Hostkey) of
        {ok,Host} ->
            Id = maps:get(<<"id">>,Host),
            {ok, Id};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_id_by_name(Name :: binary()) -> {ok, binary()} | {error, atom()}.
get_id_by_name(Name) ->
    case get_by_name(Name) of
        {ok,Host} ->
            Id = maps:get(<<"id">>,Host),
            {ok, Id};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_all_active_interfaces() -> {ok, list()}.
get_all_active_interfaces() ->
    %r.db("dog").table("host").filter({"active":"active"}).getField("interfaces")
    %{ok, RethinkTimeout} = application:get_env(dog_trainer,rethink_timeout_ms),
    %{ok, Connection} = gen_rethink_session:get_connection(dog_session),
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:filter(X,#{<<"active">> => <<"active">>}),
            reql:get_field(X,<<"interfaces">>)
        end),
    {ok, Result} = rethink_cursor:all(R),
    Interfaces = lists:flatten(Result),
    lager:info("Interfaces: ~p",[Interfaces]),
    %lager:debug("Interfaces: ~p",[Interfaces]),
    Interfaces@1 = dog_group:merge(Interfaces),
    %lager:debug("Interfaces@1: ~p",[Interfaces@1]),
    case Interfaces@1 of
        [] -> {ok, []};
        _ -> {ok, Interfaces@1}
    end.

-spec set_inactive_by_name(Name :: binary()) -> { true, iolist() } | {false, no_updated}.
set_inactive_by_name(Name) ->
    {ok, Id} = get_id_by_name(Name),
    update_active(Id,<<"inactive">>).

-spec set_active_by_name(Name :: binary()) -> { true, iolist() } | {false, no_updated}.
set_active_by_name(Name) ->
    {ok, Id} = get_id_by_name(Name),
    update_active(Id,<<"active">>).

-spec set_retired_by_name(Name :: binary()) -> { true, iolist() } | {false, no_updated}.
set_retired_by_name(Name) ->
    {ok, Id} = get_id_by_name(Name),
    update_active(Id,<<"retired">>).

-spec set_inactive_by_id(Id :: binary()) -> { true, iolist() } | {false, no_updated}.
set_inactive_by_id(Id) ->
    update_active(Id,<<"inactive">>).

-spec set_active_by_id(Id :: binary()) -> { true, iolist() } | {false, no_updated}.
set_active_by_id(Id) ->
    update_active(Id,<<"active">>).

-spec set_retired_by_id(Id :: binary()) -> { true, iolist() } | {false, no_updated}.
set_retired_by_id(Id) ->
    update_active(Id,<<"retired">>).

-spec get_active_by_id(Id :: binary()) -> {'error','notfound'} | {'ok',_}.
get_active_by_id(Id) ->
    case get_document_by_id(Id) of
        {ok, Host} -> 
            {ok, maps:get(<<"active">>,Host)};
        {error, Error} -> 
            {error, Error}
    end.

-spec set_hosts_active( Ids :: list() ) -> ok.
set_hosts_active(Ids) ->
    lager:info("set_hosts_active: ~p",[Ids]),
    lists:foreach(fun(Id) -> set_active_by_id(Id) end, Ids),
    ok.

-spec set_hosts_inactive( Ids :: list() ) -> ok.
set_hosts_inactive(Ids) ->
    lager:info("set_hosts_inactive: ~p",[Ids]),
    lists:foreach(fun(Id) -> set_inactive_by_id(Id) end, Ids),
    ok.

-spec set_hosts_retired( Ids :: list() ) -> ok.
set_hosts_retired(Ids) ->
    lager:info("set_hosts_retired: ~p",[Ids]),
    lists:foreach(fun(Id) -> set_retired_by_id(Id) end, Ids),
    ok.

-spec get_schema() -> binary().
get_schema() ->
  dog_json_schema:get_file(<<"host">>).

-spec get_all_ips() -> {ok, list()}. 
get_all_ips() ->
    {ok, R} = dog_rethink:run(
                              fun(X) -> 
                                      reql:db(X, dog), 
                                      reql:table(X, ?TYPE_TABLE),
                                      reql:filter(X,#{<<"active">> => <<"active">>}),
                                      reql:get_field(X, <<"interfaces">>)
                              end),
    {ok, Result} = rethink_cursor:all(R),
    Interfaces = case lists:flatten(Result) of
                [] -> [];
                Else -> Else
            end,
    Ips = lists:map(fun(Interface) -> element(2,dog_ips:addresses_from_interfaces(jsx:decode(Interface))) end, Interfaces),
    {ok, lists:flatten(Ips)}.
