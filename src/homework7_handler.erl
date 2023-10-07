-module(homework7_handler).

-behavior(cowboy_handler).

%% API
-export([init/2]).

init(Req0, State) ->
  Method = cowboy_req:method(Req0),
  HasBody = cowboy_req:has_body(Req0),
  Req = maybe_echo(Method, HasBody, Req0),
  {ok, Req, State}.

maybe_echo(<<"POST">>, true, Req0) ->
  {ok, [{PostVals, true}], _} = cowboy_req:read_urlencoded_body(Req0),
  TData = jsone:decode(PostVals, [{object_format, proplist}]),
  Action = proplists:get_value(<<"action">>, TData),
  Res = gen_server:call(homework7, {Action, TData}),
  cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"json">>},
    Res,
    Req0);
maybe_echo(<<"POST">>, false, Req0) ->
  cowboy_req:reply(
    400,
    #{<<"content-type">> => <<"json">>},
    jsone:encode(#{<<"result">> => <<"Missing body">>}),
    Req0);
maybe_echo(_, _, Req0) ->
  cowboy_req:reply(405, Req0).