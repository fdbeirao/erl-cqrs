-module(erlcqrs_string_receiver).
-behavior(gen_server).
-define(SERVER, ?MODULE).

% supervision callbacks
-export([start_link/0]).

% Public API
-export([push_string/1, stop/0]).

% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
        
-record(state, { char_count = 0 }).

% supervision callbacks
start_link() ->
  InitialInfo = [],
  gen_server:start_link({local, ?SERVER}, ?MODULE, InitialInfo, []).

%% Public API
push_string(String) ->
  gen_server:call(?SERVER, { push_string, String }).

stop() ->
  gen_server:stop(?SERVER).

%% gen_server callbacks

init(_InitialInfo) ->
  { ok, #state{} }.

handle_call({ push_string, String }, _From, State) ->
  { Reply, NewState } = handle_push_string(String, State),
  { reply, Reply, NewState };

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

%% private server functions
handle_push_string("carriageReturn", #state{ char_count = CharCount } = State) when CharCount >= 100 ->
  { ok, State#state{ char_count = 0 } };
handle_push_string(_String, #state{ char_count = CharCount } = State) when CharCount >= 100 ->
  { { error, "expecting carriage return" }, State };
handle_push_string(String, #state{ char_count = CharCount } = State) ->
  case validate_one_uppercase_per_word(String) of
    true -> { ok, State#state{ char_count = CharCount + length(String) } };
    _ -> { { error, "each word can have at most one uppercase letter"}, State }
  end.

validate_one_uppercase_per_word(String) ->
  Words = string:tokens(String, " "),
  not lists:any(fun word_contains_more_than_one_uppercase_letter/1, Words).

word_contains_more_than_one_uppercase_letter(String) ->
  uppercase_chars_in_string(String) > 1.

uppercase_chars_in_string(String) ->
  length(lists:filter(fun(Char) -> Char >= "A" andalso Char =< "Z" end, String)).
