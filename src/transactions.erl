-module(transactions).

-export([new/3]).

-export([apply/1]).

-export([maybe_apply/1]).

-export([list/0]).

-export([create_table/0]).

-export([add_pending/1]).

-export([remove_pending/1]).

-export([list_pending/0]).

-export([set_executing/1]).

-export([check_executing_to/1]).

-export([from/1]).

-export([validate/1]).

-export([apply_t/2]).

-type id() :: binary().

-type amount() :: non_neg_integer().

-define(TABLE, pending_transactions).

-record(pending_transactions,
	{id  :: id(), from  :: accounts:id(),
	 to  :: accounts:id(), amount  :: amount(),
	 created  :: erlang:timestamp(),
	 executing = false  :: boolean()}).

-type transaction() :: #pending_transactions{}.

-export_type([transaction/0]).

-spec new(accounts:id(), accounts:id(),
	  transaction:amount()) -> transaction().

new(From, To, Amount) ->
    Id = generate_id(),
    #pending_transactions{id = Id, from = From, to = To,
			  amount = Amount, created = os:timestamp()}.

-spec maybe_apply(transaction()) -> {ok, id()} |
				    {error, executing} |
				    {error, insuficient_funds}.

maybe_apply(T = #pending_transactions{}) ->
    case check_timeout(T) of
      ok -> apply(T);
      timeout -> {error, timeout}
    end.

apply(T = #pending_transactions{}) ->
    case check_executing_to(T) of
      ok ->
	  set_executing(T),
	  fintech_rdbms:transaction(fun (Conn) ->
					    transactions:apply_t(Conn, T)
				    end);
      executing -> {error, executing};
      {error, insuficient_funds} -> {error, insuficient_funds}
    end.

apply_t(Conn,
	#pending_transactions{id = Id, from = From, to = To,
			      amount = Amount}) ->
    QueryFunds = <<"SELECT `balance` from `accounts` where "
		   "`id` = ?">>,
    {ok, _, [[CurrentAmount]]} = mysql:query(Conn,
					     QueryFunds, [From]),
    case CurrentAmount >= Amount of
      false -> {error, insuficient_funds};
      true ->
	  % substract from From
	  QueryA =
	      <<"UPDATE `accounts` set `balance` = `balance` "
		"- ? where `id` = ?">>,
	  % add to To
	  QueryB =
	      <<"UPDATE `accounts` set `balance` = `balance` "
		"+ ? where `id` = ?">>,
	  ok = mysql:query(Conn, QueryA, [Amount, From]),
	  1 = mysql:affected_rows(Conn),
	  ok = mysql:query(Conn, QueryB, [Amount, To]),
	  1 = mysql:affected_rows(Conn),
	  QueryT = <<"INSERT INTO `transactions` (`id`, `from`, "
		     "`to`, `amount`) VALUES (?, ?, ?, ?)">>,
	  ok = mysql:query(Conn, QueryT, [Id, From, To, Amount]),
	  {ok, Id}
    end.

list() ->
    Query = <<"SELECT `id`, `from`, `to`, `amount`, "
	      "`created_at` from `transactions` ORDER "
	      "BY `created_at`">>,
    {ok, _, Ts} = fintech_rdbms:query(Query),
    lists:map(fun ([Id, From, To, Amount, Created]) ->
		      #{id => Id, from => From, to => To, amount => Amount,
			created_at => Created}
	      end,
	      Ts).

generate_id() ->
    erlang:iolist_to_binary(uuid:uuid_to_string(uuid:get_v4())).

create_table() ->
    mnesia:create_table(?TABLE,
			[{ram_copies, [node()]}, {type, set},
			 {attributes,
			  record_info(fields, pending_transactions)}]),
    Result = mnesia:add_table_copy(?TABLE, node(),
				   ram_copies),
    case Result of
      {atomic, ok} -> ok;
      {aborted, {already_exists, _, _}} -> ok
    end.

add_pending(T = #pending_transactions{id = Id}) ->
    {atomic, ok} = mnesia:transaction(fun () ->
					      mnesia:write(?TABLE, T, write)
				      end),
    {ok, Id}.

remove_pending(_T = #pending_transactions{id = Id}) ->
    {atomic, ok} = mnesia:transaction(fun () ->
					      mnesia:delete({?TABLE, Id})
				      end),
    ok.

-spec list_pending() -> [map()].

list_pending() ->
    List = ets:tab2list(?TABLE),
    lists:foldl(fun (T, Acc) ->
			#pending_transactions{id = Id, from = From, to = To,
					      amount = Amount,
					      created = Created} =
			    T,
			[#{id => Id, from => From, to => To, amount => Amount,
			   created => now_to_utc_binary(Created)}
			 | Acc]
		end,
		[], List).

-spec set_executing(transaction()) -> ok.

set_executing(T) ->
    T2 = T#pending_transactions{executing = true},
    {atomic, ok} = mnesia:transaction(fun () ->
					      mnesia:write(?TABLE, T2, write)
				      end),
    ok.

-spec check_executing_to(accounts:id()) -> ok |
					   executing.

check_executing_to(To) ->
    Ids = mnesia:dirty_select(?TABLE,
			      [{#pending_transactions{to = To, id = '$1',
						      _ = '_'},
				[], ['$1']}]),
    case Ids of
      [] -> ok;
      _ -> executing
    end.

timeout() ->
    application:get_env(fintech, transaction_timeout,
			timer:seconds(10)).

check_timeout(_T = #pending_transactions{created =
					     Created}) ->
    Timeout = timeout(),
    case timer:now_diff(os:timestamp(), Created) of
      Diff when Diff > 1000 * Timeout -> timeout;
      _ -> ok
    end.

-spec from(transaction()) -> accounts:id().

from(#pending_transactions{from = From}) -> From.

-spec validate(transaction()) -> ok |
				 {amount_error, term()} |
				 {no_account, accounts:id()}.

validate(#pending_transactions{from = From, to = To,
			       amount = Amount}) ->
    case {accounts:exists(From), accounts:exists(To)} of
      {true, true} ->
	  case {is_integer(Amount), Amount > 0} of
	    {true, true} -> ok;
	    _ -> #{amount_error => Amount}
	  end;
      {false, _} -> #{no_account => From};
      {_, true} -> #{no_account => To}
    end.

-spec now_to_utc_string(erlang:timestamp()) -> string().

now_to_utc_string({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
	calendar:now_to_universal_time({MegaSecs, Secs,
					MicroSecs}),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6."
				".0wZ",
				[Year, Month, Day, Hour, Minute, Second,
				 MicroSecs])).

now_to_utc_binary(Timestamp) ->
    list_to_binary(now_to_utc_string(Timestamp)).
