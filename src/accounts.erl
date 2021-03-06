-module(accounts).

-export([apply_transaction/1, cleanup/1, create_table/0,
	 exists/1, get_all_accounts/0, get_or_create/1, load/1,
	 start_link/1]).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-type account_map() :: map().

-type id() :: binary().

-type match_spec() :: '_' | '$1' | '$2'.

-record(accounts,
	{id  :: id() | match_spec(),
	 pid  :: pid() | match_spec(),
	 node  :: node() | match_spec()}).

apply_transaction(T) ->
    From = transactions:from(T),
    Pid = get_or_create(From),
	transactions:add_pending(T),
	Timeout = application:get_env(fintech, transaction_timeout, 
									timer:seconds(3)),
    Result = try gen_server:call(Pid, {transaction, T},
				 Timeout)
	     of
	       R -> R
	     catch
	       exit:{timeout, _} = Error ->
		   error_logger:error_msg("transaction timeout ~p: ~p",
					  [T, Error]),
		   {error, timeout};
	       Class:Error ->
		   error_logger:error_msg("transaction error ~p: ~p",
					  [T, {Class, Error}]),
		   {error, Error}
	     end,
    transactions:remove_pending(T),
    Result.

% internals

start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

init([Id]) -> {ok, #{id => Id}}.

handle_call({transaction, T}, _From, State) ->
    Result = safe_execute(T), {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #{id := Id}) ->
    mnesia:transaction(fun () ->
			       mnesia:delete({accounts, Id})
		       end),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

safe_execute(T) ->
    Result = try transactions:maybe_apply(T) of
	       {ok, Id} -> {ok, Id};
	       {error, timeout} -> {error, timeout};
	       {error, executing} -> {error, executing};
	       {error, insuficient_funds} -> {error, insuficient_funds}
	     catch
	       _C:E ->
		   error_logger:error_msg("error executing transaction [~p]: ~p",
					  [T, E]),
		   error
	     end,
    Result.

load(File) ->
    {ok, Bin} = file:read_file(File),
    Accounts = jiffy:decode(Bin, [return_maps]),
    Updated = to_db(Accounts),
    error_logger:info_msg("Added ~p account to db.",
			  [Updated]),
    ok.

-spec to_db([account_map()]) -> non_neg_integer().

to_db(Accounts) ->
    Updated = fintech_rdbms:transaction(fun (Conn) ->
						lists:foldl(fun (Acc,
								 Updated) ->
								    Updated +
								      add_account(Conn,
										  Acc)
							    end,
							    0, Accounts)
					end),
    Updated.

add_account(Conn,
	    #{<<"id">> := Id, <<"balance">> := Balance} = _Acc)
    when is_binary(Id), is_integer(Balance), Balance >= 0 ->
    Query = <<"INSERT INTO `accounts` (`id`, `balance`) "
	      "VALUES (?, ?)">>,
    ok = mysql:query(Conn, Query, [Id, Balance]),
    mysql:affected_rows(Conn).

get_all_accounts() ->
    Query = <<"SELECT `id`, `balance` from `accounts`">>,
    {ok, _Columns, Accounts} = fintech_rdbms:query(Query),
    lists:foldl(fun ([Id, Balance], M) ->
			maps:put(Id, Balance, M)
		end,
		#{}, Accounts).

-spec exists(id()) -> boolean().

exists(Id) ->
    Query = <<"SELECT `id`, `balance` from `accounts` "
	      "where `id` = ?">>,
    {ok, _Columns, Accounts} = fintech_rdbms:query(Query,
						   [Id]),
    case Accounts of
      [] -> false;
      [_] -> true
    end.

create_table() ->
    mnesia:create_table(accounts,
			[{ram_copies, [node()]}, {type, set},
			 {attributes, record_info(fields, accounts)}]),
    Result = mnesia:add_table_copy(accounts, node(),
				   ram_copies),
    case Result of
      {atomic, ok} -> ok;
      {aborted, {already_exists, _, _}} -> ok
    end.

-spec get_or_create(id()) -> pid().

get_or_create(Id) ->
    {atomic, Pid} = mnesia:transaction(fun () ->
					       case mnesia:read({accounts, Id})
						   of
						 [#accounts{pid = Pid}] -> Pid;
						 [] ->
						     {ok, Pid} = create(Id),
						     mnesia:write(accounts,
								  #accounts{id =
										Id,
									    pid
										=
										Pid,
									    node
										=
										node()},
								  write),
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
    {atomic, ok} = mnesia:transaction(fun () ->
					      Keys = mnesia:select(accounts,
								   [{#accounts{pid
										   =
										   '$1',
									       id
										   =
										   '$2',
									       _
										   =
										   '_'},
								     [{'==',
								       {node,
									'$1'},
								       Node}],
								     ['$2']}]),
					      lists:foreach(fun (Key) ->
								    mnesia:delete({accounts,
										   Key})
							    end,
							    Keys),
					      ok
				      end),
    ok.
