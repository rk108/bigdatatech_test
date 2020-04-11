-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([date_time_str/0]).

-include("include/common.hrl").

%for gproc registration:
-define(KEY, {p, l, main_room}).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts, #{idle_timeout => ?WEBSOCKET_TIMEOUT} }.

websocket_init(State) ->
    %new connection

    gproc:reg(?KEY),

    {ok, LastMsgs} = last_messages:get_all_msg(),

    Answer = [{text, <<"Hello!">>} | LastMsgs],
    {Answer, State}.

websocket_handle({text, Msg}, State) ->
    DateTime = date_time_str(),
    Answer = {text, binary:list_to_bin([DateTime, Msg]) },
    last_messages:push_msg(Answer),
    gproc:bcast(?KEY, [Answer] ),
    {[], State};

websocket_handle(_Data, State) ->
    {[], State}.

websocket_info({timeout, _Ref, _Msg}, State) ->
    %Answer = binary:list_to_bin([<<"timer_send_">>, pid_to_list(self())]),
    %erlang:start_timer(1000, self(), Answer),
    {[], State};

websocket_info([{text, Msg}], State) ->
    %Msg = io_lib:format("handle_info:: ~p", [Info]),
    {[{text, Msg}], State};

websocket_info(Info, State) ->
    Msg = io_lib:format("handle_info:: ~p", [Info]),
    {[{text, Msg}], State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

date_time_str() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(erlang:timestamp()),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])).
