-module(evex_console_logger).
-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

init(_Args) ->
    io:format("~p: Console Logger Initialized~n", [self()]),
    % No specific state needed for this simple logger
    {ok, undefined}.

handle_event({cpu_usage, Value}, _State) when Value > 80 ->
    io:format("~p: ALERT! High CPU Usage: ~p%~n", [self(), Value]),
    {ok, undefined};
handle_event({cpu_usage, Value}, _State) ->
    io:format("~p: CPU Usage: ~p%~n", [self(), Value]),
    {ok, undefined};
handle_event({memory_alert, {Threshold, Current}}, _State) ->
    io:format("~p: MEMORY ALERT! Threshold: ~pMB, Current: ~pMB~n", [
        self(), Threshold, Current
    ]),
    {ok, undefined};
handle_event(Event, _State) ->
    io:format("~p: Received unknown event: ~p~n", [self(), Event]),
    {ok, undefined}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("~p: Console Logger Terminated~n", [self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
