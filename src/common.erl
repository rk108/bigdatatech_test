-module(common).
-export([value/1]).

value(Name) ->
  {ok, Val} = application:get_env(websocket, Name),
  Val.

