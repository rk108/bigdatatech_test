-module(last_messages).
-behaviour(gen_server).


-include("include/common.hrl").

%% API.
-export([start_link/0]).

-export([push_msg/1, get_all_msg/0]).


%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push_msg(BinaryText) ->
    gen_server:call(?MODULE, {push, BinaryText}).

get_all_msg() ->
    gen_server:call(?MODULE, all_msg).

%% gen_server.

init([]) ->
    Queue = queue:new(),
    State = #{full_queue => false, fifo => Queue},
    {ok, State}.


handle_call({push, BinaryText}, _From, #{full_queue := IsFull, fifo := Queue} = State) ->
    NewQueue = queue:in(BinaryText, Queue),
    NewState =
        case (IsFull == true) orelse queue:len(NewQueue) > ?LAST_MSG_COUNT of
            true ->
                {_, Q} = queue:out(NewQueue),
                State#{full_queue => true, fifo => Q};
            _ ->
                State#{ fifo => NewQueue}
        end,
    {reply, ok, NewState};

handle_call(all_msg, _From, #{fifo := Queue} = State) ->
    Reply = {ok, queue:to_list(Queue)},
    {reply, Reply, State};



handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
