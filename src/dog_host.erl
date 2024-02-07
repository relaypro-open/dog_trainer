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
    update/2,
    update_by_hostkey/2,
    get_id_by_hostkey/1
]).

-export([
    get_all_active_interfaces/0,
    get_hostkeys_by_ips/0,
    get_names_by_ips/0,
    hash_check/1,
    init/0,
    interfaces_to_ips/1,
    keepalive_age_check/0, keepalive_age_check/1,
    keepalive_check/0, keepalive_check/1,
    retirement_check/0, retirement_check/1,
    state_event/3
]).

-spec keepalive_check() -> {ok, Unalive :: list()}.
keepalive_check() ->
    Now = erlang:system_time(second),
    {ok, KeepAliveAlertSeconds} = application:get_env(dog_trainer, keepalive_alert_seconds),
    TimeCutoff = Now - KeepAliveAlertSeconds,
    ?LOG_DEBUG("Now: ~p", [calendar:system_time_to_rfc3339(Now)]),
    ?LOG_DEBUG("TimeCutoff: ~p", [calendar:system_time_to_rfc3339(TimeCutoff)]),
    keepalive_check(TimeCutoff).

-spec keepalive_check(TimeCutoff :: number()) -> {ok, list()}.
keepalive_check(TimeCutoff) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:pluck(X, [<<"id">>, <<"name">>, <<"keepalive_timestamp">>])
        end
    ),
    {ok, ResultTime} = rethink_cursor:all(R),
    ?LOG_DEBUG("ResultTime: ~p", [ResultTime]),
    R1 = lists:flatten(ResultTime),
    Ids = [maps:get(<<"id">>, X) || X <- R1],
    Names = [maps:get(<<"name">>, X) || X <- R1],
    Timestamps = [maps:get(<<"keepalive_timestamp">>, X) || X <- R1],
    ZippedList = lists:zip3(Ids, Names, Timestamps),
    OldAgents = [
        #{<<"id">> => Id, <<"name">> => Name, <<"keepalive_timestamp">> => TimeStamp}
     || {Id, Name, TimeStamp} <- ZippedList, TimeStamp < TimeCutoff
    ],
    ?LOG_INFO("OldAgents: ~p", [OldAgents]),
    {ok, OldAgents}.

-spec retirement_check() -> {ok, Unalive :: list()}.
retirement_check() ->
    Now = erlang:system_time(second),
    KeepAliveAlertSeconds = application:get_env(dog_trainer, retirement_alert_seconds, 86400),
    TimeCutoff = Now - KeepAliveAlertSeconds,
    ?LOG_DEBUG("Now: ~p", [calendar:system_time_to_rfc3339(Now)]),
    ?LOG_DEBUG("TimeCutoff: ~p", [calendar:system_time_to_rfc3339(TimeCutoff)]),
    retirement_check(TimeCutoff).

-spec retirement_check(TimeCutoff :: number()) -> {ok, list()}.
retirement_check(TimeCutoff) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:filter(X, #{<<"active">> => <<"inactive">>}),
            reql:pluck(X, [<<"id">>, <<"name">>, <<"keepalive_timestamp">>])
        end
    ),
    {ok, ResultTime} = rethink_cursor:all(R),
    ?LOG_DEBUG("ResultTime: ~p", [ResultTime]),
    R1 = lists:flatten(ResultTime),
    Ids = [maps:get(<<"id">>, X) || X <- R1],
    Names = [maps:get(<<"name">>, X) || X <- R1],
    Timestamps = [maps:get(<<"keepalive_timestamp">>, X) || X <- R1],
    ZippedList = lists:zip3(Ids, Names, Timestamps),
    OldAgents = [
        #{<<"id">> => Id, <<"name">> => Name, <<"keepalive_timestamp">> => TimeStamp}
     || {Id, Name, TimeStamp} <- ZippedList, TimeStamp < TimeCutoff
    ],
    ?LOG_INFO("OldAgents: ~p", [OldAgents]),
    {ok, OldAgents}.

-spec hash_fail_count_check(HostId :: binary(), HashCheck :: (true | false), HashStatus :: map()) ->
    {true | false, map()}.
hash_fail_count_check(HostId, HashCheck, HashStatus) ->
    case HashCheck of
        true ->
            hash_fail_count_update(HostId, 0),
            {pass, HashStatus};
        false ->
            hash_fail_count_increment(HostId),
            case
                hash_fail_count(HostId) >= application:get_env(dog_trainer, max_hash_fail_count, 2)
            of
                true ->
                    {fail, HashStatus};
                false ->
                    {pass, HashStatus}
            end
    end.

-spec hash_fail_count_increment(HostId :: binary()) -> number().
hash_fail_count_increment(HostId) ->
    Count = hash_fail_count(HostId),
    hash_fail_count_update(HostId, Count + 1).

-spec hash_fail_count(HostId :: binary()) -> number().
hash_fail_count(HostId) ->
    {ok, HostHashFailCount} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, HostId),
            reql:get_field(X, <<"hash_fail_count">>)
        end
    ),
    ?LOG_DEBUG("HostHashFailCount: ~p", [HostHashFailCount]),
    HostHashFailCount.

-spec hash_fail_count_update(HostId :: binary(), Count :: number()) ->
    {true, binary()} | {false, atom()}.
hash_fail_count_update(HostId, Count) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, HostId),
            reql:update(X, #{<<"hash_fail_count">> => Count})
        end
    ),
    ?LOG_DEBUG("update R: ~p~n", [R]),
    Replaced = maps:get(<<"replaced">>, R),
    Unchanged = maps:get(<<"unchanged">>, R),
    case {Replaced, Unchanged} of
        {1, 0} -> {true, HostId};
        {0, 1} -> {false, HostId};
        _ -> {false, no_updated}
    end.

-spec hash_check(HostId :: binary()) -> {pass, map()} | {fail, map()}.
hash_check(Host) ->
    HostId = maps:get(<<"id">>, Host),
    HostName = maps:get(<<"name">>, Host),
    GroupName = maps:get(<<"group">>, Host),
    {ok, Group} = dog_group:get_by_name(GroupName),
    %Iptables
    HostHash4Ipsets = maps:get(<<"hash4_ipsets">>, Host),
    HostHash6Ipsets = maps:get(<<"hash6_ipsets">>, Host),
    HostHash4Iptables = maps:get(<<"hash4_iptables">>, Host),
    HostHash6Iptables = maps:get(<<"hash6_iptables">>, Host),
    GroupHash4Ipsets = maps:get(<<"hash4_ipsets">>, Group),
    GroupHash6Ipsets = maps:get(<<"hash6_ipsets">>, Group),
    GroupHash4Iptables = maps:get(<<"hash4_iptables">>, Group),
    GroupHash6Iptables = maps:get(<<"hash6_iptables">>, Group),
    Match4IpsetsCheck = (HostHash4Ipsets == GroupHash4Ipsets),
    Match6IpsetsCheck = (HostHash6Ipsets == GroupHash6Ipsets),
    Match4IptablesCheck = (HostHash4Iptables == GroupHash4Iptables),
    Match6IptablesCheck = (HostHash6Iptables == GroupHash6Iptables),
    IptablesHashCheck = iptables_hash_logic(
        Match4IpsetsCheck, Match6IpsetsCheck, Match4IptablesCheck, Match6IptablesCheck
    ),
    %Ipset
    HostIpsetHash = maps:get(<<"ipset_hash">>, Host),
    IpsetHashCheck = dog_ipset:hash_check(HostIpsetHash),

    IptablesHashAgeCheck = iptables_hash_age_check(HostId),
    Now = dog_time:timestamp(),
    IpsetHashAgeCheck = ipset_hash_age_check(HostId),
    HashStatus = #{
        <<"name">> => HostName,
        <<"id">> => HostId,
        <<"hash4_ipsets">> => Match4IpsetsCheck,
        <<"hash6_ipsets">> => Match6IpsetsCheck,
        <<"hash4_iptables">> => Match4IptablesCheck,
        <<"hash6_iptables">> => Match6IptablesCheck,
        <<"ipset_hash">> => IpsetHashCheck,
        <<"ipset_hash_age_check">> => IpsetHashAgeCheck,
        <<"iptables_hash_age_check">> => IptablesHashCheck
    },
    ?LOG_DEBUG("HashStatus: ~p", [HashStatus]),
    ?LOG_DEBUG("IptablesHashCheck,IpsetHashCheck: ~p, ~p", [IptablesHashCheck, IpsetHashCheck]),
    ?LOG_DEBUG("IptablesHashAgeCheck: ~s,  IpsetHashAgeCheck: ~s", [
        IptablesHashAgeCheck, IpsetHashAgeCheck
    ]),
    case {IptablesHashCheck, IpsetHashCheck} of
        {true, true} ->
            iptables_hash_age_update(HostId, Now),
            ipset_hash_age_update(HostId, Now),
            hash_fail_count_update(HostId, 0),
            {pass, HashStatus};
        {false, true} ->
            ipset_hash_age_update(HostId, Now),
            hash_fail_count_check(HostId, IptablesHashAgeCheck, HashStatus);
        {true, false} ->
            iptables_hash_age_update(HostId, Now),
            hash_fail_count_check(HostId, IpsetHashAgeCheck, HashStatus);
        {false, false} ->
            case {IptablesHashAgeCheck, IpsetHashAgeCheck} of
                {true, true} ->
                    {pass, HashStatus};
                {_, _} ->
                    {fail, HashStatus}
            end
    end.

-spec ipset_hash_age_check(HostId :: binary()) -> boolean().
ipset_hash_age_check(HostId) ->
    Now = erlang:system_time(second),
    KeepAliveAlertSeconds = application:get_env(dog_trainer, hashcheck_alert_seconds, 30),
    TimeCutoff = Now - KeepAliveAlertSeconds,
    ?LOG_DEBUG("Now: ~p", [calendar:system_time_to_rfc3339(Now)]),
    ?LOG_DEBUG("TimeCutoff: ~p", [calendar:system_time_to_rfc3339(TimeCutoff)]),
    ipset_hash_age_check(HostId, TimeCutoff).

-spec ipset_hash_age_check(HostId :: binary(), TimeCutoff :: number()) -> boolean().
ipset_hash_age_check(HostId, TimeCutoff) ->
    {ok, IpsetHashTimestamp} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, HostId),
            reql:get_field(X, <<"ipset_hash_timestamp">>)
        end
    ),
    ?LOG_DEBUG("IpsetHashTimestamp, TimeCutoff: ~p, ~p", [IpsetHashTimestamp, TimeCutoff]),
    case IpsetHashTimestamp of
        <<>> ->
            Now = erlang:system_time(second),
            ipset_hash_age_update(HostId, Now),
            false;
        _ ->
            IpsetHashTimestamp >= TimeCutoff
    end.

-spec iptables_hash_age_check(HostId :: binary()) -> boolean().
iptables_hash_age_check(HostId) ->
    Now = erlang:system_time(second),
    KeepAliveAlertSeconds = application:get_env(dog_trainer, hashcheck_alert_seconds, 30),
    TimeCutoff = Now - KeepAliveAlertSeconds,
    ?LOG_DEBUG("Now: ~p", [calendar:system_time_to_rfc3339(Now)]),
    ?LOG_DEBUG("TimeCutoff: ~p", [calendar:system_time_to_rfc3339(TimeCutoff)]),
    iptables_hash_age_check(HostId, TimeCutoff).

-spec iptables_hash_age_check(HostId :: binary(), TimeCutoff :: number()) -> boolean().
iptables_hash_age_check(HostId, TimeCutoff) ->
    {ok, IptablesHashTimestamp} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, HostId),
            reql:get_field(X, <<"iptables_hash_timestamp">>)
        end
    ),
    ?LOG_DEBUG("IptablesHashTimestamp: ~p", [IptablesHashTimestamp]),
    case IptablesHashTimestamp of
        <<>> ->
            Now = erlang:system_time(second),
            iptables_hash_age_update(HostId, Now),
            false;
        _ ->
            IptablesHashTimestamp < TimeCutoff
    end.

-spec iptables_hash_age_update(HostId :: binary(), Timestamp :: number()) ->
    {true, binary()} | {false, atom()}.
iptables_hash_age_update(HostId, Timestamp) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, HostId),
            reql:update(X, #{<<"iptables_hash_timestamp">> => Timestamp})
        end
    ),
    ?LOG_DEBUG("update R: ~p~n", [R]),
    Replaced = maps:get(<<"replaced">>, R),
    Unchanged = maps:get(<<"unchanged">>, R),
    case {Replaced, Unchanged} of
        {1, 0} -> {true, HostId};
        {0, 1} -> {false, HostId};
        _ -> {false, no_updated}
    end.

-spec ipset_hash_age_update(HostId :: binary(), Timestamp :: number()) ->
    {true, binary()} | {false, atom()}.
ipset_hash_age_update(HostId, Timestamp) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, HostId),
            reql:update(X, #{<<"ipset_hash_timestamp">> => Timestamp})
        end
    ),
    ?LOG_DEBUG("update R: ~p~n", [R]),
    Replaced = maps:get(<<"replaced">>, R),
    Unchanged = maps:get(<<"unchanged">>, R),
    case {Replaced, Unchanged} of
        {1, 0} -> {true, HostId};
        {0, 1} -> {false, HostId};
        _ -> {false, no_updated}
    end.

-spec keepalive_age_check() -> {ok, Unalive :: list()}.
keepalive_age_check() ->
    Now = erlang:system_time(second),
    KeepAliveAlertSeconds = application:get_env(dog_trainer, keepalive_alert_seconds, 1800),
    TimeCutoff = Now - KeepAliveAlertSeconds,
    ?LOG_DEBUG("Now: ~p", [calendar:system_time_to_rfc3339(Now)]),
    ?LOG_DEBUG("TimeCutoff: ~p", [calendar:system_time_to_rfc3339(TimeCutoff)]),
    keepalive_age_check(TimeCutoff).

-spec keepalive_age_check(TimeCutoff :: number()) -> {ok, list()}.
keepalive_age_check(TimeCutoff) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:filter(X, #{<<"active">> => <<"active">>}),
            reql:pluck(X, [<<"id">>, <<"name">>, <<"keepalive_timestamp">>])
        end
    ),
    {ok, ResultTime} = rethink_cursor:all(R),
    ?LOG_INFO("ResultTime: ~p", [ResultTime]),
    R1 = lists:flatten(ResultTime),
    Ids = [maps:get(<<"id">>, X) || X <- R1],
    Names = [maps:get(<<"name">>, X) || X <- R1],
    Timestamps = [maps:get(<<"keepalive_timestamp">>, X) || X <- R1],
    ZippedList = lists:zip3(Ids, Names, Timestamps),
    OldAgents = [
        #{<<"id">> => Id, <<"name">> => Name, <<"keepalive_timestamp">> => TimeStamp}
     || {Id, Name, TimeStamp} <- ZippedList, TimeStamp < TimeCutoff
    ],
    ?LOG_INFO("OldAgents: ~p", [OldAgents]),
    {ok, OldAgents}.

-spec iptables_hash_logic(
    HashCheck4Ipsets :: boolean(),
    HashCheck6Ipsets :: boolean(),
    HashCheck4Iptables :: boolean(),
    HashCheck6Iptables :: boolean()
) -> boolean().
iptables_hash_logic(HashCheck4Ipsets, HashCheck6Ipsets, HashCheck4Iptables, HashCheck6Iptables) ->
    CheckV6Hashes = application:get_env(dog_trainer, check_v6_hashes, true),
    case CheckV6Hashes of
        true ->
            HashCheck4Ipsets and HashCheck6Ipsets and HashCheck4Iptables and HashCheck6Iptables;
        false ->
            HashCheck4Ipsets and HashCheck4Iptables
    end.

-spec send_retirement_alert(Host :: binary()) -> ok.
send_retirement_alert(Host) ->
    ?LOG_INFO("Host: ~p", [Host]),
    HostName = binary:bin_to_list(maps:get(<<"name">>, Host)),
    HostKey = binary:bin_to_list(maps:get(<<"hostkey">>, Host)),
    ?LOG_INFO("Retirement alert sent: ~p, ~p", [HostName, HostKey]),
    {ok, SmtpRelay} = application:get_env(dog_trainer, smtp_relay),
    {ok, SmtpUsername} = application:get_env(dog_trainer, smtp_username),
    {ok, SmtpPassword} = application:get_env(dog_trainer, smtp_password),
    Subject = io_lib:format("Dog Agent Retired: ~p", [HostName]),
    {ok, From} = application:get_env(dog_trainer, smtp_from),
    {ok, Addresses} = application:get_env(dog_trainer, smtp_to),
    To = string:join(Addresses, ","),
    KeepAliveAlertSeconds = application:get_env(dog_trainer, retirement_alert_seconds, 86400),
    Body = io_lib:format(
        "Hosts that haven't communicated in last ~p seconds: ~nHostName: ~p~nHostKey: ~p~n", [
            KeepAliveAlertSeconds, HostName, HostKey
        ]
    ),
    Email = io_lib:format("Subject: ~s\r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s", [Subject, From, To, Body]),
    gen_smtp_client:send(
        {From, Addresses, Email},
        [
            {relay, SmtpRelay},
            {username, SmtpUsername},
            {password, SmtpPassword},
            {tls, always}
        ]
    ),
    imetrics:add_m(alert, "retirement"),
    ok.

-spec send_keepalive_alert(Host :: binary()) -> ok.
send_keepalive_alert(Host) ->
    ?LOG_INFO("Host: ~p", [Host]),
    HostName = binary:bin_to_list(maps:get(<<"name">>, Host)),
    HostKey = binary:bin_to_list(maps:get(<<"hostkey">>, Host)),
    ?LOG_INFO("Keepalive disconnect alert sent: ~p, ~p", [HostName, HostKey]),
    {ok, SmtpRelay} = application:get_env(dog_trainer, smtp_relay),
    {ok, SmtpUsername} = application:get_env(dog_trainer, smtp_username),
    {ok, SmtpPassword} = application:get_env(dog_trainer, smtp_password),
    Subject = io_lib:format("Dog Agents Disconnected: ~p", [HostName]),
    {ok, From} = application:get_env(dog_trainer, smtp_from),
    {ok, Addresses} = application:get_env(dog_trainer, smtp_to),
    To = string:join(Addresses, ","),
    {ok, KeepAliveAlertSeconds} = application:get_env(dog_trainer, keepalive_alert_seconds),
    Body = io_lib:format(
        "Hosts that haven't communicated in last ~p seconds: ~nHostName: ~p~nHostKey: ~p~n", [
            KeepAliveAlertSeconds, HostName, HostKey
        ]
    ),
    Email = io_lib:format("Subject: ~s\r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s", [Subject, From, To, Body]),
    gen_smtp_client:send(
        {From, Addresses, Email},
        [
            {relay, SmtpRelay},
            {username, SmtpUsername},
            {password, SmtpPassword},
            {tls, always}
        ]
    ),
    imetrics:add_m(alert, "keepalive_fail"),
    ok.

-spec send_keepalive_recover(Host :: map()) -> ok.
send_keepalive_recover(Host) ->
    ?LOG_INFO("Host: ~p", [Host]),
    HostName = binary:bin_to_list(maps:get(<<"name">>, Host)),
    HostKey = binary:bin_to_list(maps:get(<<"hostkey">>, Host)),
    ?LOG_INFO("Keepalive recover alert sent: ~p, ~p", [HostName, HostKey]),
    {ok, SmtpRelay} = application:get_env(dog_trainer, smtp_relay),
    {ok, SmtpUsername} = application:get_env(dog_trainer, smtp_username),
    {ok, SmtpPassword} = application:get_env(dog_trainer, smtp_password),
    Subject = io_lib:format("Dog Agents Reconnected: ~p", [HostName]),
    {ok, From} = application:get_env(dog_trainer, smtp_from),
    {ok, Addresses} = application:get_env(dog_trainer, smtp_to),
    To = string:join(Addresses, ","),
    {ok, KeepAliveAlertSeconds} = application:get_env(dog_trainer, keepalive_alert_seconds),
    Body = io_lib:format("Host reconnected in last ~p seconds: ~nHostName: ~s~nHostKey: ~s~n", [
        KeepAliveAlertSeconds, HostName, HostKey
    ]),
    Email = io_lib:format("Subject: ~s\r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s", [Subject, From, To, Body]),
    gen_smtp_client:send(
        {From, Addresses, Email},
        [
            {relay, SmtpRelay},
            {username, SmtpUsername},
            {password, SmtpPassword},
            {tls, always}
        ]
    ),
    imetrics:add_m(alert, "keepalive_recover"),
    ok.

-spec send_hash_alert(Host :: binary(), HashStatus :: map()) -> ok.
send_hash_alert(Host, HashStatus) ->
    HashAlertEnabled = application:get_env(dog_trainer, hash_alert_enabled, true),
    case HashAlertEnabled of
        true ->
            ?LOG_INFO("Host: ~p", [Host]),
            HostName = binary:bin_to_list(maps:get(<<"name">>, Host)),
            HostKey = binary:bin_to_list(maps:get(<<"hostkey">>, Host)),
            GroupName = maps:get(<<"group">>, Host),
            {ok, IpsetHashes} = dog_ipset:latest_hash(),
            {ok, Group} = dog_group:get_by_name(GroupName),
            ?LOG_INFO("Hash alert sent: ~p", [Host]),
            {ok, SmtpRelay} = application:get_env(dog_trainer, smtp_relay),
            {ok, SmtpUsername} = application:get_env(dog_trainer, smtp_username),
            {ok, SmtpPassword} = application:get_env(dog_trainer, smtp_password),
            Subject = io_lib:format("Dog Locally Modified On: ~s", [HostName]),
            {ok, From} = application:get_env(dog_trainer, smtp_from),
            {ok, Addresses} = application:get_env(dog_trainer, smtp_to),
            To = string:join(Addresses, ","),
            Body = io_lib:format(
                "Host's iptables and/or ipsets modified outside of dog~nHostName: ~s~nHostKey: ~s~nHost: ~p~nGroup: ~p~nIpsetHashes: ~p~nHashStatus: ~p~n",
                [HostName, HostKey, Host, Group, IpsetHashes, HashStatus]
            ),
            Email = io_lib:format("Subject: ~s\r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s", [
                Subject, From, To, Body
            ]),
            gen_smtp_client:send(
                {From, Addresses, Email},
                [
                    {relay, SmtpRelay},
                    {username, SmtpUsername},
                    {password, SmtpPassword},
                    {tls, always}
                ]
            ),
            imetrics:add_m(alert, "hash_fail"),
            ok;
        false ->
            ok
    end.

-spec send_hash_recover(Host :: binary(), HashStatus :: map()) -> ok.
send_hash_recover(Host, HashStatus) ->
    HashAlertEnabled = application:get_env(dog_trainer, hash_alert_enabled, true),
    case HashAlertEnabled of
        true ->
            ?LOG_INFO("Host: ~p", [Host]),
            HostName = binary:bin_to_list(maps:get(<<"name">>, Host)),
            HostKey = binary:bin_to_list(maps:get(<<"hostkey">>, Host)),
            GroupName = maps:get(<<"group">>, Host),
            {ok, IpsetHashes} = dog_ipset:latest_hash(),
            {ok, Group} = dog_group:get_by_name(GroupName),
            ?LOG_INFO("Hash alert sent: ~p", [Host]),
            {ok, SmtpRelay} = application:get_env(dog_trainer, smtp_relay),
            {ok, SmtpUsername} = application:get_env(dog_trainer, smtp_username),
            {ok, SmtpPassword} = application:get_env(dog_trainer, smtp_password),
            Subject = io_lib:format("Dog Back In Control On: ~s", [HostName]),
            {ok, From} = application:get_env(dog_trainer, smtp_from),
            {ok, Addresses} = application:get_env(dog_trainer, smtp_to),
            To = string:join(Addresses, ","),
            Body = io_lib:format(
                "Host's iptables and/or ipsets back in sync with dog: ~nHostName: ~s~nHostKey: ~s~nHost: ~p~nGroup: ~p~nIpsetHashes: ~p~nHashStatus: ~p~n",
                [HostName, HostKey, Host, Group, IpsetHashes, HashStatus]
            ),
            Email = io_lib:format("Subject: ~s\r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s", [
                Subject, From, To, Body
            ]),
            gen_smtp_client:send(
                {From, Addresses, Email},
                [
                    {relay, SmtpRelay},
                    {username, SmtpUsername},
                    {password, SmtpPassword},
                    {tls, always}
                ]
            ),
            imetrics:add_m(alert, "hash_recover"),
            ok;
        false ->
            ok
    end.

-spec init() -> any().
init() ->
    pass.

-spec get_document_by_id(binary()) -> {ok, map()} | {error, atom()}.
get_document_by_id(Id) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, Id)
        end
    ),
    case R of
        null -> {error, notfound};
        _ -> {ok, R}
    end.

-spec get_by_name(binary()) -> {ok, map()} | {error, notfound}.
get_by_name(Name) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get_all(X, Name, #{index => <<"name">>}),
            reql:filter(X, #{<<"active">> => <<"active">>})
        end
    ),
    {ok, R3} = rethink_cursor:all(R),
    Result = lists:flatten(R3),
    case Result of
        [] -> {error, notfound};
        _ -> {ok, hd(Result)}
    end.

-spec get_by_hostkey(binary()) -> {ok, map()} | {error, notfound}.
get_by_hostkey(Name) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get_all(X, Name, #{index => <<"hostkey">>})
        end
    ),
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
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:pluck(X, [<<"name">>, <<"id">>, <<"group">>, <<"active">>, <<"hostkey">>])
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Hosts =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Hosts}.

-spec get_all_active() -> {ok, list()}.
get_all_active() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:filter(X, #{<<"active">> => <<"active">>}),
            reql:pluck(X, [<<"name">>, <<"id">>, <<"group">>])
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Hosts =
        case lists:flatten(Result) of
            [] -> [];
            Else -> Else
        end,
    {ok, Hosts}.

get_names_by_ips() ->
    get_by_ips(<<"name">>).

get_hostkeys_by_ips() ->
    get_by_ips(<<"hostkey">>).

-spec get_by_ips(Key :: iolist()) -> map().
get_by_ips(Key) ->
    {ok, R} =
        dog_rethink:run(fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:filter(X, #{<<"active">> => <<"active">>}),
            reql:pluck(X, [Key, <<"interfaces">>])
        end),
    {ok, Result} = rethink_cursor:all(R),
    Hosts =
        case lists:flatten(Result) of
            [] ->
                [];
            Else ->
                Else
        end,
    IpsHosts =
        lists:map(
            fun(Host) ->
                Interfaces = maps:get(<<"interfaces">>, Host),
                Ips = interfaces_to_ips(Interfaces),
                maps:remove(<<"interfaces">>, maps:put(<<"ips">>, Ips, Host))
            end,
            Hosts
        ),
    IpHost =
        lists:map(
            fun(Host) ->
                Ips = maps:get(<<"ips">>, Host),
                Name = maps:get(Key, Host),
                lists:map(fun(Ip) -> {Ip, Name} end, Ips)
            end,
            IpsHosts
        ),
    maps:from_list(
        lists:flatten(IpHost)
    ).

-spec interfaces_to_ips(InterfacesString :: iolist()) -> Ips :: list().
interfaces_to_ips(InterfacesString) ->
    Interfaces = jsx:decode(InterfacesString),
    lists:delete(<<"127.0.0.1">>, lists:flatten([element(2, I) || I <- Interfaces])).

-spec create(Group :: map()) -> {ok | error, Key :: iolist() | name_exists}.
create(HostMap@0) ->
    Hostkey = maps:get(<<"hostkey">>, HostMap@0, notfound),
    case Hostkey of
        notfound ->
            {error, no_hostkey};
        _ ->
            {ok, ExistingHosts} = get_all(),
            ExistingHostkeys = [maps:get(<<"hostkey">>, Host) || Host <- ExistingHosts],
            DefaultValuesHostMap = #{
                <<"active">> => <<"new">>,
                <<"environment">> => <<"*">>,
                <<"hash_alert_sent">> => <<"">>,
                <<"hash_fail_count">> => 0,
                <<"hostkey">> => <<"">>,
                <<"ipset_hash_timestamp">> => <<"">>,
                <<"iptables_hash_timestamp">> => <<"">>,
                <<"keepalive_alert_sent">> => <<"">>,
                <<"keepalive_timestamp">> => <<"">>,
                <<"location">> => <<"*">>
            },
            MergedHostMap = maps:merge(DefaultValuesHostMap, HostMap@0),

            case lists:member(Hostkey, ExistingHostkeys) of
                false ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:insert(X, MergedHostMap)
                        end
                    ),
                    Key = hd(maps:get(<<"generated_keys">>, R)),
                    {ok, Key};
                true ->
                    {error, name_exists}
            end
    end.

-spec update(Id :: binary(), UpdateMap :: map()) ->
    {ok, iolist()} | {false, iolist()} | {false, no_updated} | {validation_error, iolist()}.
update(Id, UpdateMap) ->
    case get_by_id(Id) of
        {ok, OldHost} ->
            NewHost = maps:merge(OldHost, UpdateMap),
            case dog_json_schema:validate(?VALIDATION_TYPE, NewHost) of
                ok ->
                    {ok, R} = dog_rethink:run(
                        fun(X) ->
                            reql:db(X, dog),
                            reql:table(X, ?TYPE_TABLE),
                            reql:get(X, Id),
                            reql:replace(X, NewHost, #{return_changes => always})
                        end
                    ),
                    ?LOG_DEBUG("update R: ~p~n", [R]),
                    Replaced = maps:get(<<"replaced">>, R),
                    Unchanged = maps:get(<<"unchanged">>, R),
                    case {Replaced, Unchanged} of
                        {1, 0} -> {true, Id};
                        {0, 1} -> {false, Id};
                        _ -> {false, no_updated}
                    end;
                {error, Error} ->
                    {validation_error, Error}
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

-spec delete(Id :: binary()) -> (ok | error).
delete(Id) ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, Id),
            reql:delete(X)
        end
    ),
    ?LOG_DEBUG("delete R: ~p~n", [R]),
    Deleted = maps:get(<<"deleted">>, R),
    case Deleted of
        1 -> ok;
        _ -> error
    end.

-spec get_state_from_host(Host :: map()) -> {'error', 'notfound'} | {'ok', _}.
get_state_from_host(Host) ->
    Active = maps:get(<<"active">>, Host, <<"new">>),
    Hashpass = maps:get(<<"hashpass">>, Host, true),
    State =
        case {Active, Hashpass} of
            {<<"retired">>, _} ->
                <<"retired">>;
            {<<"new">>, true} ->
                <<"new">>;
            {<<"active">>, true} ->
                <<"active">>;
            {<<"active">>, false} ->
                <<"active_hashfail">>;
            {<<"inactive">>, true} ->
                <<"inactive">>;
            {<<"inactive">>, false} ->
                <<"inactive_hashfail">>
        end,
    {ok, State}.

-spec set_state_by_id(Id :: binary(), State :: binary()) ->
    {true, binary()} | {false, binary()} | {false, no_updated}.
set_state_by_id(Id, State) ->
    ?LOG_DEBUG("Setting agent ~p state to: ~p", [Id, State]),
    {Active, Hashpass} =
        case State of
            <<"retired">> ->
                {<<"retired">>, true};
            <<"new">> ->
                {<<"new">>, true};
            <<"active">> ->
                {<<"active">>, true};
            <<"active_hashfail">> ->
                {<<"active">>, false};
            <<"inactive">> ->
                {<<"inactive">>, true};
            <<"inactive_hashfail">> ->
                {<<"inactive">>, false}
        end,
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:get(X, Id),
            reql:update(X, #{<<"active">> => Active, <<"hashpass">> => Hashpass})
        end
    ),
    Replaced = maps:get(<<"replaced">>, R),
    Unchanged = maps:get(<<"unchanged">>, R),
    case {Replaced, Unchanged} of
        {1, 0} -> {true, Id};
        {0, 1} -> {false, Id};
        _ -> {false, no_updated}
    end.
-spec get_id_by_hostkey(Hostkey :: binary()) -> {ok, binary()} | {error, atom()}.
get_id_by_hostkey(Hostkey) ->
    case get_by_hostkey(Hostkey) of
        {ok, Host} ->
            Id = maps:get(<<"id">>, Host),
            {ok, Id};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_all_active_interfaces() -> {ok, list()}.
get_all_active_interfaces() ->
    {ok, R} = dog_rethink:run(
        fun(X) ->
            reql:db(X, dog),
            reql:table(X, ?TYPE_TABLE),
            reql:filter(X, #{<<"active">> => <<"active">>}),
            reql:get_field(X, <<"interfaces">>)
        end
    ),
    {ok, Result} = rethink_cursor:all(R),
    Interfaces = lists:flatten(Result),
    ?LOG_INFO("Interfaces: ~p", [Interfaces]),
    Interfaces@1 = dog_group:merge(Interfaces),
    case Interfaces@1 of
        [] -> {ok, []};
        _ -> {ok, Interfaces@1}
    end.


-spec get_schema() -> binary().
get_schema() ->
    dog_json_schema:get_file(<<"host">>).

-spec state_event(
    Id :: binary(),
    Event :: keepalive | keepalive_timeout | retirement_timeout | fail_hashcheck | pass_hashcheck,
    HashStatus :: map()
) -> NewState :: binary().
state_event(HostMap, Event, HashStatus) ->
    Id = maps:get(<<"id">>, HostMap),
    {ok, OldState} = get_state_from_host(HostMap),
    NewState = new_state(HostMap, OldState, Event, HashStatus),
    ?LOG_DEBUG("Id: ~p, Event: ~p, OldState: ~p, NewState: ~p", [Id, Event, OldState, NewState]),
    case OldState == NewState of
        false ->
            set_state_by_id(Id, NewState);
        true ->
            pass
    end,
    NewState.

-spec new_state(
    HostMap :: map(),
    OldState :: binary(),
    Event :: keepalive | keepalive_timeout | retirement_timeout | fail_hashcheck | pass_hashcheck,
    HashStatus :: map()
) -> NewState :: binary().
new_state(_HostMap, <<"active">>, pass_hashcheck, _HashStatus) ->
    <<"active">>;
new_state(_HostMap, <<"new">>, pass_hashcheck, _HashStatus) ->
    <<"active">>;
new_state(_HostMap, <<"new">>, fail_hashcheck, _HashStatus) ->
    <<"active">>;
new_state(HostMap, <<"active">>, fail_hashcheck, HashStatus) ->
    send_hash_alert(HostMap, HashStatus),
    <<"active_hashfail">>;
new_state(HostMap, <<"active">>, keepalive_timeout, _HashStatus) ->
    send_keepalive_alert(HostMap),
    <<"inactive">>;
new_state(_HostMap, <<"active_hashfail">>, fail_hashcheck, _HashStatus) ->
    <<"active_hashfail">>;
new_state(HostMap, <<"active_hashfail">>, pass_hashcheck, HashStatus) ->
    send_hash_recover(HostMap, HashStatus),
    <<"active">>;
new_state(HostMap, <<"active_hashfail">>, keepalive_timeout, HashStatus) ->
    send_hash_recover(HostMap, HashStatus),
    <<"inactive_hashfail">>;
new_state(_HostMap, <<"inactive">>, keepalive_timeout, _HashStatus) ->
    <<"inactive">>;
new_state(HostMap, <<"inactive">>, pass_hashcheck, _HashStatus) ->
    send_keepalive_recover(HostMap),
    <<"active">>;
new_state(HostMap, <<"inactive">>, fail_hashcheck, HashStatus) ->
    send_hash_alert(HostMap, HashStatus),
    <<"active_hashfail">>;
new_state(HostMap, <<"inactive">>, retirement_timeout, _HashStatus) ->
    send_retirement_alert(HostMap),
    <<"retired">>;
new_state(_HostMap, <<"inactive_hashfail">>, fail_hashcheck, _HashStatus) ->
    <<"active_hashfail">>;
new_state(HostMap, <<"inactive_hashfail">>, pass_hashcheck, HashStatus) ->
    send_hash_recover(HostMap, HashStatus),
    <<"active">>;
new_state(HostMap, <<"inactive_hashfail">>, retirement_timeout, _HashStatus) ->
    send_retirement_alert(HostMap),
    <<"retired">>;
new_state(_HostMap, <<"retired">>, retirement_timeout, _HashStatus) ->
    <<"retired">>;
new_state(HostMap, <<"retired">>, fail_hashcheck, _HashStatus) ->
    imetrics:set_gauge_m(<<"host_keepalive">>, <<"recovery">>, 0),
    send_keepalive_recover(HostMap),
    <<"active">>;
new_state(HostMap, <<"retired">>, pass_hashcheck, _HashStatus) ->
    imetrics:set_gauge_m(<<"host_keepalive">>, <<"recovery">>, 0),
    send_keepalive_recover(HostMap),
    <<"active">>;
new_state(HostMap, State, Event, _HashStatus) ->
    ?LOG_ERROR("Invalid event: ~p, for state : ~p for host: ~p combination", [Event, State, HostMap]),
    State.
