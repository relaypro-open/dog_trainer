-module(dog_file).

-include("dog_trainer.hrl").

-export([
         read_file/1
        ]).

read_file(FileName) ->
  try
    case file:read_file(FileName) of
              {ok, Ruleset} -> {ok, Ruleset};
            {error,Reason} ->
              ?LOG_ERROR("file error: ~p, ~p",[Reason,FileName]),
              {error,Reason}
    end
  of
    File -> File
  catch ErrorType:ErrorReason:Stacktrace ->
          ?LOG_ERROR("catch ErrorType:ErrorReason:Stacktrace",[ErrorType,ErrorReason,Stacktrace])
  end.
