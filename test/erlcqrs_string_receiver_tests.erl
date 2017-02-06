-module(erlcqrs_string_receiver_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SUT, erlcqrs_string_receiver).
-define(Send(String), ?SUT:push_string(String)).
-define(Test(Fun), { setup, fun startSut/0, fun stopSut/1, Fun }).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
until_100_characters__ok_test_() ->
  { "Until 100 characters there is no problem",
  ?Test(fun until_100_characters__ok/0) }.

after_100_characters__error_test_() ->
  { "Once 100 characters are inputted, the system will not accept more characters",
  ?Test(fun after_100_characters__error/0) }.

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
until_100_characters__ok() ->
  ?assertEqual(ok, ?Send("A string with that contains 41 characters")),
  ?assertEqual(ok, ?Send("Another 21 characters")),
  ?assertEqual(ok, ?Send("This makes it to 100 characters total.")).

after_100_characters__error() ->
  until_100_characters__ok(),
  ?assertEqual({ error, "expecting carriage return" }, ?Send("1")),
  ?assertEqual({ error, "expecting carriage return" }, ?Send("Anything other than 'carriageReturn' will be an error")).

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
startSut() ->
  ?SUT:start_link().

stopSut(_) ->
  ?SUT:stop().