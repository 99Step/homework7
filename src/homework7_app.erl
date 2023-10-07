-module(homework7_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    { '_',
      [{<<"/api/cache_server">>, homework7_handler, [] }]}]),
  {ok, _} = cowboy:start_clear(my_http_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
  ),
  case homework7_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.