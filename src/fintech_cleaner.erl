-module(fintech_cleaner).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-define(NODE_CLEANUP_LOCK(Node), {node_cleanup_lock, Node}).
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    case net_kernel:monitor_nodes(true) of
        ok -> {ok, #{}};
        Error -> {stop, Error}
    end.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({nodedown, Node}, State) ->
    ok = cleanup(Node),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

cleanup(Node) ->
    LockRequest = {{node_cleanup_lock, Node}, self()},
    Nodes = [node() | nodes()],
    C = fun() ->
        ok = accounts:cleanup(Node)
    end,
    case global:trans(LockRequest, C, Nodes, 1) of
        aborted ->
            erlang:error("Error clean of node ~p", [Node]);
        {ok, Result} ->
            Result
    end.
            