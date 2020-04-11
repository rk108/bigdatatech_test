-module(websocket_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    LastMessagesServer = {last_messages, {last_messages, start_link, []},
                   permanent,
                   5000,
                   worker, [last_messages]},
    Procs = [LastMessagesServer],
    {ok, {{one_for_one, 10, 10}, Procs}}.
