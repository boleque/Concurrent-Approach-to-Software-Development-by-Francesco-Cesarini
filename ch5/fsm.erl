-module(fsm).
-export([start/0, idle/0,ringing/1, connected/1, dial/0]).

start() ->
  ok = event_manager:start(billing, [{io_handler, 1}]),
  register(phone, spawn(fsm, idle, [])),
  ok.

idle() ->
  receive
    {Number, incoming} ->
      start_ringing(),
      ringing(Number);
    off_hook ->
      start_tone(),
      dial()
  end.

ringing(Number) ->
  receive
    {Number, other_on_hook} ->
      stop_ringing(),
      idle();
    {Number, off_hook} ->
      stop_ringing(), 
      start_conversation(Number),
      connected(Number)
  end.

connected(Number) ->
  receive
    on_hook ->
      stop_conversation(Number),
      idle()
  end.

dial() ->
  receive
    on_hook ->
      idle();
    {Number, reach_number} ->
      start_conversation(Number),
      connected(Number)
  end.

start_ringing() -> 
    event_manager:send_event(billing, {billing_alarm, 0, start_ringing}).

start_tone() ->
    event_manager:send_event(billing, {billing_alarm, 0, start_tone}).

stop_ringing() -> 
    event_manager:send_event(billing, {billing_alarm, 0, stop_ringing}).

start_conversation(Number) ->
  event_manager:send_event(billing, {billing_alarm, Number, start_conversation}).

stop_conversation(Number) ->
  event_manager:send_event(billing, {billing_alarm, Number, stop_conversation}).



