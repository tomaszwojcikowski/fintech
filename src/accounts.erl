-module(accounts).

-export([apply_transaction/2]).
-export([load/1]).
-export([get_all_accounts/0]).
-export([create_table/0]).
-export([get_or_create/1]).
-export([cleanup/1]).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type account_map() :: map().
-type id() :: binary().

-type match_spec() :: '_' | '$1'| '$2'.

-record(accounts, {id :: id() | match_spec(), 
                   pid :: pid() | match_spec(), 
                   node :: node() | match_spec()}).

apply_transaction(From, T) ->
    Pid = get_or_create(From),
    transactions:add_pending(T),
    gen_server:call(Pid, {transaction, T}, timer:seconds(10)).

% internals

start_link(Id) ->
   gen_server:start_link(?MODULE, [Id], []).

init([Id]) -> {ok, #{id => Id}}.

handle_call({transaction, T}, _From, State) ->
    Result = safe_execute(T), 
    {reply, Result, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) ->  {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, #{id := Id}) -> 
    mnesia:transaction(fun() ->
        mnesia:delete({accounts, Id})
    end),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

safe_execute(T) ->
    Result = try transactions:apply(T) of
        {ok, Id} ->
            transactions:remove_pending(T), 
            {ok, Id};
        {error, executing} ->
            maybe_retry(T) % todo noreply
    catch 
        _C:E ->
            transactions:remove_pending(T),
            error_logger:error_msg("error executing transaction [~p]: ~p", [T, E]),
            error
    end,
    Result.

maybe_retry(T) ->
    case transactions:check_timeout(T) of
        ok ->
            retry(T),
            wait;
        timeout ->
            {error, timeout}
    end.

retry(T) ->
    Interval = rand:uniform(500),
    erlang:send_after(Interval, self(), {transaction, T}).

load(File) ->
    {ok, Bin} = file:read_file(File),
    Accounts = jiffy:decode(Bin, [return_maps]),
    Updated = to_db(Accounts),
    error_logger:info_msg("Added ~p account to db.", [Updated]),
    ok.

-spec to_db([account_map()]) -> non_neg_integer().
to_db(Accounts) ->
    Conn = get_db_conn(),
    {atomic, Updated} = mysql:transaction(Conn, fun() ->
        lists:foldl(fun(Acc, Updated) ->
        Updated + add_account(Conn, Acc)    
        end, 0, Accounts)     
    end),
    Updated.

add_account(Conn, #{<<"id">> := Id, <<"balance">> := Balance} = _Acc) 
    when is_binary(Id), 
         is_integer(Balance),
         Balance >= 0 ->
    Query = <<"INSERT INTO `accounts` (`id`, `balance`) VALUES (?, ?)">>,
    ok = mysql:query(Conn, Query, [Id, Balance]),
    mysql:affected_rows(Conn).

get_all_accounts() ->
    Conn = get_db_conn(),
    Query = <<"SELECT `id`, `balance` from `accounts`">>,
    {ok, _Columns, Accounts} = mysql:query(Conn, Query),
    lists:foldl(fun([Id, Balance], M) ->
        maps:put(Id, Balance, M)
    end, #{}, Accounts).

get_db_conn() ->
    {ok, Opts} = application:get_env(fintech, mysql),
    {ok, Conn} = mysql:start_link(Opts),
    Conn.

create_table() ->
    mnesia:create_table(accounts,
                        [{ram_copies, [node()]},
                         {type, set},
                         {attributes, record_info(fields, accounts)}]),
    Result = mnesia:add_table_copy(accounts, node(), ram_copies),
    case Result of
        {atomic, ok} -> ok;
        {aborted,{already_exists,_,_}} -> ok
    end.

-spec get_or_create(id()) -> pid().
get_or_create(Id) ->
    {atomic, Pid} = mnesia:transaction(fun() ->
        case mnesia:read({accounts, Id}) of
            [#accounts{pid = Pid}] -> Pid;
            [] -> 
                {ok, Pid} = create(Id),
                mnesia:write(accounts, #accounts{id = Id, pid = Pid, node = node()}, write),
                Pid
        end
    end),
    Pid.

create(Id) ->
    ChildSpec = #{id => <<"account_", Id/binary>>,      
                  start => {?MODULE, start_link, [Id]},     
                  restart => temporary},
    supervisor:start_child(accounts_sup, ChildSpec).

cleanup(Node) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        Keys = mnesia:select(accounts, [{#accounts{pid = '$1', id = '$2', _ = '_'},
                                        [{'==', {node, '$1'}, Node}], ['$2']}]),
        lists:foreach(fun(Key) ->
                mnesia:delete({accounts, Key})
        end, Keys),
        ok  
    end),
    ok.