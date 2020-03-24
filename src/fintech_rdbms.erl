-module(fintech_rdbms).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_pool/0, stop_pool/0]).
-export([query/1, query/2]).

-record(state, {conn}).

start_link() ->
   gen_server:start_link(?MODULE, [], []).

init(_Args) ->
   self() ! init, 
   {ok, #state{}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call({query, Query, Params}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:query(Conn, Query, Params),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(init, State) ->
    {ok, Opts} = application:get_env(fintech, mysql),
    {ok, Conn} = mysql:start_link(Opts),
    {noreply, State#state{conn = Conn}};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

query(Query) ->
    query(Query, []).

query(Query, Params) ->
    wpool:call(pool_name(), {query, Query, Params}).

pool_name() ->
    fintech_rdmbs.

start_pool() ->
    PoolOptions = [{workers, 10},
                   {worker, {?MODULE, []}}],
    {ok, _Pid} = wpool:start_sup_pool(pool_name(), PoolOptions).

stop_pool() ->
    wpool:stop_pool(pool_name()).