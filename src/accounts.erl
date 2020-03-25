-module(accounts).

-export([load/1]).
-export([get_all_accounts/0]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type account_map() :: map().


% internals

start_link() ->
   gen_server:start_link(?MODULE, [], []).

init(_Args) -> {ok, #{}}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) ->  {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


-spec load(file:name_all()) -> ok.
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