-module(dog_file).

-include("dog_trainer.hrl").

-export([
    read_file/1
]).

read_file(FileName) ->
    try
        case file:read_file(FileName) of
            {ok, IptablesRuleset} ->
                {ok, IptablesRuleset};
            {error, Reason} ->
                {error, Reason}
        end
    of
        File -> File
    catch
        ErrorType:ErrorReason:Stacktrace ->
            ?LOG_ERROR(#{"message" => "catch", "error_type" => ErrorType, "error_reason" => ErrorReason, "stacktrace" => Stacktrace})
    end.
