-module(homework7).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

-record(homework7_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #homework7_state{}} | {ok, State :: #homework7_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  ets:new(cache, [named_table, public]),
  {ok, #homework7_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #homework7_state{}) ->
  {reply, Reply :: term(), NewState :: #homework7_state{}} |
  {reply, Reply :: term(), NewState :: #homework7_state{}, timeout() | hibernate} |
  {noreply, NewState :: #homework7_state{}} |
  {noreply, NewState :: #homework7_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #homework7_state{}} |
  {stop, Reason :: term(), NewState :: #homework7_state{}}).

handle_call({<<"insert">>, Data}, _From, State = #homework7_state{}) ->
  Key = proplists:get_value(<<"key">>, Data),
  Value = proplists:get_value(<<"value">>, Data),
  TimeStamp = calendar:universal_time(),
  ets:insert(cache, {Key, Value, TimeStamp}),
  Response = jsone:encode(#{<<"result">> => <<"ok">>}),
  {reply, Response, State};
handle_call({<<"lookup">>, Data}, _From, State = #homework7_state{}) ->
  Key = proplists:get_value(<<"key">>, Data),
  Response = jsone:encode(#{<<"result">> => lookup(key, Key)}),
  {reply, Response, State};
handle_call({<<"lookup_by_date">>, Data}, _From, State = #homework7_state{}) ->
  DateFrom = proplists:get_value(<<"date_from">>, Data),
  DateTo = proplists:get_value(<<"date_to">>, Data),
  Value1 =
    case check(DateFrom, DateTo) of
      [] -> undefined;
      Value -> Value
    end,
  Response = jsone:encode(#{<<"result">> => Value1}),
  {reply, Response, State};
handle_call(_, _From, State = #homework7_state{}) ->
  {reply, {<<"result">>, <<"bed request">>}, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #homework7_state{}) ->
  {noreply, NewState :: #homework7_state{}} |
  {noreply, NewState :: #homework7_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #homework7_state{}}).
handle_cast(_Request, State = #homework7_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #homework7_state{}) ->
  {noreply, NewState :: #homework7_state{}} |
  {noreply, NewState :: #homework7_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #homework7_state{}}).
handle_info(_Info, State = #homework7_state{}) ->
  {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check(DateFrom, DateTo) ->
  case ets:first(cache) of
    '$end_of_table' -> ok;
    Key -> check(Key, DateFrom, DateTo, [])
  end.

check('$end_of_table', _, _, Acc) ->
  lists:reverse(Acc);
check(Key, DateFrom, DateTo, Acc) ->
  Acc1 = case lookup(time, Key) of
           {Value, Time} ->
             case check_time(DateFrom, DateTo, Time) of
               true -> [#{<<"key">> => Key, <<"value">> => Value} | Acc];
               false -> Acc
             end;
           _Err -> Acc
         end,
  NewKey = ets:next(cache, Key),
  check(NewKey, DateFrom, DateTo, Acc1).

check_time(DateFrom, DateTo, Time) ->
  DateFrom1 = parse_date(DateFrom),
  DateTo1 = parse_date(DateTo),
  TimeFrom = calendar:datetime_to_gregorian_seconds(DateFrom1),
  TimeTo = calendar:datetime_to_gregorian_seconds(DateTo1),
  TimeRecord = calendar:datetime_to_gregorian_seconds(Time),
  (TimeFrom < TimeRecord) and (TimeRecord < TimeTo).

lookup(key, Key) ->
  case ets:lookup(cache, Key) of
    [{_, Value, _}] -> Value;
    _ -> undefined
  end;
lookup(time, Key) ->
  case ets:lookup(cache, Key) of
    [{Key, Value, Time}] -> {Value, Time};
    _ -> undefined
  end.

parse_date(Rest) ->
  [Date, Time] = binary:split(Rest, <<" ">>),
  [Year, Month, Day] = binary:split(Date, <<"/">>, [global]),
  [Hour, Minute, Second] = binary:split(Time, <<":">>, [global]),
  {{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)},
    {binary_to_integer(Hour), binary_to_integer(Minute), binary_to_integer(Second)}}.

