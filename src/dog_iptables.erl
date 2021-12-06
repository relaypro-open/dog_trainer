-module(dog_iptables).

-include("dog_trainer.hrl").

-export([ write_temp_iptables/1, 
          publish_to_queue/6,
          valid_iptables/0, update_group_iptables/2,
          update_all_iptables/0
        ]).

-export([chunk_list/1,
         chunk_list/2]).

-spec update_group_iptables(GroupZoneName :: binary(), GroupType :: binary() ) -> 'ok'.
update_group_iptables(GroupZoneName, GroupType) ->
    lager:info("GroupZoneName: ~p",[GroupZoneName]),
    Groups = case application:get_env(dog_trainer,generate_unset_tables,true) of 
        true -> 
            {ok, GroupsList} = case GroupType of
                <<"role">> -> 
                    dog_group:role_group_effects_groups(GroupZoneName);
                <<"zone">> -> 
                    dog_group:zone_group_effects_groups(GroupZoneName)
            end,
            GroupsList;
        false ->
            {ok, GroupsList} = case GroupType of
                <<"role">> -> 
                   {ok, [GroupZoneName]};
                <<"zone">> -> 
                    case dog_zone:get_by_name(GroupZoneName) of
                        {ok,Zone} ->
                            ZoneId = maps:get(<<"id">>,Zone),
                            dog_group:zone_group_effects_groups(ZoneId);
                        _ ->
                            {ok, []}
                    end
            end,
            GroupsList
    end,
    lager:info("Effected Groups: ~p",[Groups]),
    lager:info("add_to_queue: ~p",[Groups]),
    dog_profile_update_agent:add_to_queue(Groups),
    ok.

-spec update_all_iptables() -> 'ok'.
update_all_iptables() ->
    lager:debug("update_all_iptables:start"),
    {ok, Groups} = dog_group:get_active_groups(),
    GroupNames = [ maps:get(<<"name">>,Group) || Group <- Groups],
    ChunkedGroupNames = chunk_list(GroupNames,2),
    lists:foreach(fun(GroupName) ->
        dog_profile_update_agent:add_to_queue(GroupName),
        timer:sleep(1000)
    end,ChunkedGroupNames),
    lager:debug("update_all_iptables:end"),
    ok.

chunk_list(List) ->
    chunk_list(List,2).

chunk_list([],_) -> [];
chunk_list(List,Len) when Len > length(List) ->
        [List];
chunk_list(List,Len) ->
        {Head,Tail} = lists:split(Len,List),
            [Head | chunk_list(Tail,Len)].

-spec write_temp_iptables(Ruleset :: iolist() ) -> 'ok' | 'validation_error'.
write_temp_iptables(Ruleset) ->
    {ok, TempFile } = write_to_temp_file4(Ruleset),
    update_iptables4(TempFile),
    ok.
    %TODO: temporarily disabled validation because ipsets don't exist on dog_trainer
    %case validate_ruleset4(TempFile) of
    %    ok -> 
    %        update_iptables4(TempFile);
    %    _ -> validation_error 
    %end.

-spec delete_iptables_tempfile(TempFile :: iolist() ) -> 'ok'.
delete_iptables_tempfile(TempFile) ->
    ok = file:delete(TempFile),
    ok.

-spec write_to_temp_file4(Ruleset :: iolist()) -> {'ok',[any(),...]}.
write_to_temp_file4(Ruleset) ->
    RandString = binary_to_list(base16:encode(crypto:strong_rand_bytes(6))), 
    TempFile = ?RUNDIR ++ "/iptables4." ++ RandString ++ ".txt",
    ok = file:write_file(TempFile, Ruleset),
    {ok, TempFile}.

%TODO: temporarily disabled validation because ipsets don't exist on dog_trainer
%-spec validate_ruleset4([any(),...]) -> 'error' | 'ok'.
%validate_ruleset4(TempFile) ->
%    Cmd = "sudo /sbin/iptables-restore --test " ++ TempFile,
%    Result = os:cmd(Cmd),
%    case Result of
%        [] ->
%           lager:info("iptables valid"), 
%            ok;
%        _ -> 
%            error
%    end.

-spec backup_ruleset4() -> 'error' | 'ok'.
backup_ruleset4() ->
    Cmd = "sudo " ++ "/sbin/iptables-save" ++ " > " ++ ?RUNDIR ++ "/iptables.back",
    Result = os:cmd(Cmd),
    case Result of
        [] ->
           lager:info("iptables backed up"), 
            ok;
        _ -> 
            error
    end.

-spec update_iptables4([any(),...]) -> 'ok'.
update_iptables4(TempFile) ->
    ok = backup_ruleset4(),
    ok = delete_iptables_tempfile(TempFile),
    %TODO
    lager:info("Iptables updated."),
    ok.

-spec valid_iptables() -> binary().
valid_iptables() -> 
<<"# Generated iptables by devops
*filter
:INPUT ACCEPT [0:0]
:FORWARD ACCEPT [0:0]
:OUTPUT ACCEPT [33:2995]
:FIREWALL-INPUT - [0:0]
:PROXY - [0:0]
:SSH - [0:0]
-A INPUT -i lo -j ACCEPT
-A INPUT -j FIREWALL-INPUT
-A INPUT -j REJECT --reject-with icmp-port-unreachable
-A FIREWALL-INPUT -m state --state RELATED,ESTABLISHED -j ACCEPT
-A FIREWALL-INPUT -p tcp -m tcp --dport 22 -j SSH
-A FIREWALL-INPUT -j LOG --log-prefix \"Dropped@FIREWALL-INPUT - \" --log-level 7 --log-ip-options
-A FIREWALL-INPUT -j REJECT --reject-with icmp-port-unreachable
-A PROXY -p tcp -m tcp --dport 8005 -j ACCEPT
-A PROXY -j LOG --log-prefix \"Dropped@PROXY - \" --log-level 7 --log-ip-options
-A PROXY -j REJECT --reject-with icmp-port-unreachable
COMMIT
# Generated iptables by devops">>.

-spec publish_to_queue(RoutingKey :: binary(), R4IpsetsRuleset :: list() | boolean(), R6IpsetsRuleset :: list() | boolean(), R4IptablesRuleset :: list() | boolean(), R6IptablesRuleset :: list() | boolean(), Ipsets :: list()) -> any().
publish_to_queue(RoutingKey, R4IpsetsRuleset, R6IpsetsRuleset, R4IptablesRuleset, R6IptablesRuleset, Ipsets) ->
    lager:info("RoutingKey: ~p",[RoutingKey]),
    UserData = #{
      ruleset4_ipset => R4IpsetsRuleset,
      ruleset6_ipset => R6IpsetsRuleset,
      ruleset4_iptables => R4IptablesRuleset,
      ruleset6_iptables => R6IptablesRuleset,
      ipsets => Ipsets
                },
    Count = 1,
    Pid = erlang:self(),
    Message = term_to_binary([{count, Count}, {local_time, calendar:local_time()}, {pid, Pid}, {user_data, UserData}]),
    Response = thumper:publish(Message, ?IptablesExchange, RoutingKey),
    imetrics:add(iptables_publish),
    Response.
