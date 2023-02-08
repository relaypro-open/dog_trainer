-module(dog_parse).

-include("dog_trainer.hrl").

-export([validation_error/1]).

-spec validation_error(Error :: [tuple()]) -> binary().
validation_error(Error) ->
    Response =
        case Error of
            [{data_invalid, Schema, ErrorType, Value, Path}] ->
                #{
                    error => data_invalid,
                    schema => Schema,
                    error_type => ErrorType,
                    value => Value,
                    path => Path
                };
            [{schema_invalid, Schema, ErrorType}] ->
                #{error => schema_invalid, schema => Schema, error_type => ErrorType};
            [{database_error, Schema, ErrorType}] ->
                #{error => database_error, schema => Schema, error_type => ErrorType}
        end,
    jsx:encode(Response).
