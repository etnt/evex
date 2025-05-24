-module(evex_monitor_event_manager).
-behaviour(gen_event).

-export([start_link/0, stop/0, notify/2]).
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% Event Manager API
start_link() ->
    % Register with the module name locally
    gen_event:start_link({local, ?MODULE}).

stop() ->
    gen_event:stop(?MODULE).

notify(EventId, Data) ->
    gen_event:notify(?MODULE, {EventId, Data}).

% gen_event Callbacks (for the manager itself, though often these are just pass-through)
init(_Args) ->
    % Initial state for the manager itself
    {ok, []}.

handle_event(_Event, State) ->
    % The event manager itself doesn't typically process events;
    % it just dispatches them to handlers.
    % This clause is usually unreachable if only notify is used.
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
