-module(string_receiver).
-behavior(gen_server).
-define(SERVER, ?MODULE).

% supervision callbacks
-export([start_link/0]).

% Public API
-export([push_string/1]).

% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
        
-record(state, { }).

% supervision callbacks
start_link() ->
  InitialInfo = [],
  gen_server:start_link({local, ?SERVER}, ?MODULE, InitialInfo, []).

%% Public API
push_string(String) ->
  gen_server:call(?SERVER, { push_string, String }).

%% gen_server callbacks

init(_InitialInfo) ->
  { ok, #state{} }.

handle_call({ push_string, String }, _From, State) ->
  io:format("Received string [~p]~n", [String]), % debug, remove this line
  {reply, ok, State };

handle_call(Other, From, State) ->
  logger:unexpected_message_on_handle_call(?MODULE, Other, From),
  { reply, ok, State }.

handle_cast(Other, State) ->
  logger:unexpected_message_on_handle_cast(?MODULE, Other),
  { noreply, State }.

handle_info(Other, State) ->
  logger:unexpected_message_on_handle_info(?MODULE, Other),
  { noreply, State }.

terminate(_Reason, _State) ->
  ok.

code_change(_PreviousVersion, State, _Extra) ->
  {ok, State}.
