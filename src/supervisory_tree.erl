-module(supervisory_tree).

-include("dog_trainer.hrl"). 


-export([
         tree/0
        ]).


tree() ->
    Lst = lists:map(fun(App) ->
                      %io:format("App: ~p~n",[App]),
                      {AppName, _Description,_Id} = App,
                      %io:format("AppName: ~p~n",[AppName]),
                      AppMasterPid = application_controller:get_master(AppName),
                      case AppMasterPid of
                          undefined ->
                              {AppName,[]};
                          _ ->
                              {AppMasterChildPid,_AppMasterName} = application_master:get_child(AppMasterPid),
                              %io:format("AppMasterPid: ~p~n",[AppMasterChildPid]),
                              get_children(AppName, AppMasterChildPid)
                      end
                     end, application:which_applications()),
    %Lst.
    [{K,V} || {K,V} <- Lst, V =/= [] ].

get_children({Name,Pid,supervisor,_NameList}) ->
    %io:format("supervisor~n"),
    case Pid of
        undefined ->
            {Name, []};
        _ ->
            ChildList = supervisor:which_children(Pid),
            {Name, lists:map(fun(Child) ->
                              get_children(Child)
                      end, ChildList)}
    end;
get_children({Name,Pid,worker,_NameList}) ->
    %io:format("worker~n"),
    %io:format("Pid info: ~p~n",[erlang:process_info(Pid)]),
    {Name,Pid}.

get_children(AppName,Pid) ->
    %io:format("Pid: ~p~n",[Pid]),
    Children = supervisor:which_children(Pid),
    %io:format("Children: ~p~n",[Children]),
    {AppName, lists:map(fun(Child) ->
                      %io:format("Child: ~p~n",[Child]),
                      get_children(Child)
              end, Children)}.
