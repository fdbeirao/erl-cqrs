-module(erlcqrs_logger).
-compile([export_all]).

%% Public API

unexpected_message_on_handle_call(Module, Message, FromPid) ->
  io:format("Unexpected message on [handle_call] for module ~p, coming from Pid ~p: [~p] ~n", [Module, FromPid, Message]),
  ok.

unexpected_message_on_handle_cast(Module, Message) ->
  io:format("Unexpected message on [handle_cast] for module ~p: [~p]~n", [Module, Message]),
  ok.

unexpected_message_on_handle_info(Module, Message) ->
  io:format("Unexpected message on [handle_info] for module ~p: [~p]~n", [Module, Message]),
  ok.
