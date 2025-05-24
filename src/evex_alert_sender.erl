-module(evex_alert_sender).
-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% Initial state: (sent_alerts_count, last_cpu_alert_time)
init(_Args) ->
    io:format("~p: Alert Sender Initialized~n", [self()]),
    {ok, {0, nil}}.

handle_event({cpu_usage, Value}, {SentCount, _LastCpuAlert}) when Value > 90 ->
    io:format(
        "~p: [ALERT SENDER] Sending CRITICAL CPU Alert for ~p% usage!~n", [
            self(), Value
        ]
    ),
    {ok, {SentCount + 1, os:timestamp()}};
handle_event({memory_alert, {Threshold, Current}}, {SentCount, LastCpuAlert}) ->
    io:format(
        "~p: [ALERT SENDER] Sending MEMORY Alert! (Threshold: ~p, Current: ~p)~n",
        [self(), Threshold, Current]
    ),
    {ok, {SentCount + 1, LastCpuAlert}};
handle_event(_Event, State) ->
    % Ignore other events
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, {SentCount, _LastCpuAlert}) ->
    io:format("~p: Alert Sender Terminated. Total alerts sent: ~p~n", [
        self(), SentCount
    ]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
