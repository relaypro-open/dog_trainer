-module(dog_log_filter).

-export([dog_trainer_only/2, non_dog_trainer/2]).

%% Pass only log events with domain [dog_trainer].
-spec dog_trainer_only(logger:log_event(), term()) -> logger:filter_return().
dog_trainer_only(#{meta := #{domain := [dog_trainer | _]}} = Event, _Extra) ->
    Event;
dog_trainer_only(_Event, _Extra) ->
    stop.

%% Pass log events not in the dog_trainer domain (i.e. third-party libraries).
-spec non_dog_trainer(logger:log_event(), term()) -> logger:filter_return().
non_dog_trainer(#{meta := #{domain := [dog_trainer | _]}}, _Extra) ->
    stop;
non_dog_trainer(Event, _Extra) ->
    Event.
