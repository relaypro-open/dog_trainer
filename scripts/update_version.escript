#!/usr/bin/env escript
%%! -noshell -noinput 

main([FilePath, Version]) ->
    %update_version:main(FilePath, Version).
    io:format("File: ~p~n",[FilePath]),
    {ok,ContentsList} = file:consult(FilePath),
    %io:format("ContentsList: ~p~n",[ContentsList]),
    Contents = hd(ContentsList),
    Map = create_map(Contents),
    %io:format("~p~n",[Map]),
    Dog = maps:get(dog_trainer,Map),
    %io:format("Dog = ~p~n",[Dog]),
    DogMap = maps:from_list(Dog),
    %io:format("DogMap = ~p~n",[DogMap]),
    %OldVersion = element(2,hd(Dog)),
    OldVersion = maps:get(version,DogMap),
    io:format("Old Version: ~p~n",[OldVersion]),
    io:format("New Version: ~p~n",[Version]),
    NewDog = maps:to_list(maps:update(version,Version,DogMap)),
    %io:format("NewDog = ~p~n",[NewDog]),
    NewMap = Map#{dog_trainer := NewDog},
    NewConfig = maps:to_list(NewMap),
    NC = io_lib:format("~p.",[NewConfig]),
    %io:format("NC: ~p~n",[NC]),
    Result = file:write_file(FilePath, NC),
    io:format("Result: ~p~n",[Result]).

create_map(List_Of_Key_Val_Tuples) ->
        create_map(List_Of_Key_Val_Tuples, #{}).

create_map([], Acc) ->
        Acc;
create_map([{Key, Val} | Tail], Acc) ->
        create_map(Tail, Acc#{Key => Val}).
