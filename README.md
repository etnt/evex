# Erlang Event Manager Example

A simple example using gen_event for a "system monitoring" scenario.
We'll have an event manager that dispatches system events
(e.g., cpu_usage, memory_alert), and different handlers will react to them.

Let's run the example (NOTE: you can also run them as a Lux test by issue the command: `make test` ):

```erlang
4> evex_monitor_event_manager:start_link().
{ok,<0.82.0>}
5> gen_event:add_handler(evex_monitor_event_manager, evex_console_logger, []).
<0.82.0>: Console Logger Initialized
ok
6> gen_event:add_handler(evex_monitor_event_manager, evex_alert_sender, []).
<0.82.0>: Alert Sender Initialized
ok
```

Let's confirm the handlers are registered (this is usually for debugging/introspection):

```erlang
7> gen_event:which_handlers(evex_monitor_event_manager).
[evex_alert_sender,evex_console_logger]
```

You can see both `evex_alert_sender` and `evex_console_logger` are registered.
The PIDs here are the actual handler processes.

Now, let's send some events:

 ```erlang
8> evex_monitor_event_manager:notify(cpu_usage, 50).
<0.82.0>: CPU Usage: 50%
ok
 ```

Only the console_logger reacts to this level.

```erlang
9> evex_monitor_event_manager:notify(cpu_usage, 85).
<0.82.0>: ALERT! High CPU Usage: 85%
ok
```

Again, only the console_logger reacts, but with an alert message.

```erlang
10> evex_monitor_event_manager:notify(cpu_usage, 95).
<0.82.0>: [ALERT SENDER] Sending CRITICAL CPU Alert for 95% usage!
<0.82.0>: ALERT! High CPU Usage: 95%
ok
```

Now, both console_logger and alert_sender react, each with their specific logic.

 ```erlang
11> evex_monitor_event_manager:notify(memory_alert, {1024, 1500}).
<0.82.0>: [ALERT SENDER] Sending MEMORY Alert! (Threshold: 1024, Current: 1500)
<0.82.0>: MEMORY ALERT! Threshold: 1024MB, Current: 1500MB
ok
 ```

 Both handlers respond to the memory alert.

  ```erlang
12> evex_monitor_event_manager:notify(disk_io, {read, 100}).
<0.82.0>: Received unknown event: {disk_io,{read,100}}
ok
```

The console_logger handles unknown events, while alert_sender simply
ignores them, demonstrating independent logic.

You can also dynamically add/remove handlers:

```erlang
13> gen_event:delete_handler(evex_monitor_event_manager, evex_console_logger, []).
<0.82.0>: Console Logger Terminated
ok
14> evex_monitor_event_manager:notify(cpu_usage, 70).
ok % No console output now, as the handler is gone.
15> evex_monitor_event_manager:notify(cpu_usage, 98).
<0.82.0>: [ALERT SENDER] Sending CRITICAL CPU Alert for 98% usage!
ok % Only alert sender reacts
```

To stop the event manager:

```erlang
16> evex_monitor_event_manager:stop().
<0.82.0>: Alert Sender Terminated. Total alerts sent: 3
ok
```

The terminate functions of the remaining handlers are called.

This example clearly illustrates:

* How a single gen_event manager dispatches events.
* How multiple, independent handlers can be attached.
* How each handler applies its own logic to the same event.
* The dynamic nature of adding/removing handlers.

