-module(transactions).

-export([add_pending/1, apply/1, apply_t/2,
	 check_executing_to/1, create_table/0, from/1, list/0,
	 list_pending/0, maybe_apply/1, new/3, remove_pending/1,
	 set_executing/1, validate/1]).

-type id() :: binary().

-type amount() :: non_neg_integer().

-define(TABLE, pending_transactions).

-type match_spec() :: '_' | '$1' | '$2'.

-record(pending_transactions,
	{id  :: id() | match_spec(), from  :: accounts:id() | match_spec(),
	 to  :: accounts:id() | match_spec(), amount  :: amount() | match_spec(),
	 created  :: erlang:timestamp() | match_spec(),
	 executing = false  :: boolean() | match_spec()}).

-type transaction() :: #pending_transactions{}.

-export_type([transaction/0]).

-spec new(accounts:id(), accounts:id(),
	  transaction:amount()) -> transaction().

new(From, To, Amount) ->
    Id = generate_id(),
    #pending_transactions{id = Id, from = From, to = To,
			  amount = Amount, created = os:timestamp()}.

-spec maybe_apply(transaction()) -> {ok, id()} |
				    {error, timeout} |
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
      executing -> {error, executing}
    end.

-spec apply_t(pid(), transaction()) -> {ok, id()} | {error, insuficient_funds}.
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

-spec list() -> [map()].
list() ->
    Query = <<"SELECT `id`, `from`, `to`, `amount`, "
	      "`created_at` from `transactions` ORDER "
	      "BY `created_at`">>,
    {ok, _, Ts} = fintech_rdbms:query(Query),
    lists:map(fun ([Id, From, To, Amount, Created]) ->
		      #{id => Id, from => From, to => To, amount => Amount,
			created_at => now_to_utc_string(Created)}
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

transaction_life() ->
    application:get_env(fintech, transaction_life,
			timer:seconds(10)).

check_timeout(_T = #pending_transactions{created =
					     Created}) ->
    Timeout = transaction_life(),
    case timer:now_diff(os:timestamp(), Created) of
      Diff when Diff > 1000 * Timeout -> timeout;
      _ -> ok
    end.

-spec from(transaction()) -> accounts:id().

from(#pending_transactions{from = From}) -> From.

-spec validate(transaction()) -> ok | {error, map()}.

validate(#pending_transactions{from = From, to = To,
			       amount = Amount}) ->
    case {accounts:exists(From), accounts:exists(To)} of
      {true, true} ->
	  case {is_integer(Amount), Amount > 0} of
	    {true, true} -> ok;
	    _ -> {error, #{amount_error => Amount}}
	  end;
      {false, _} -> {error, #{no_account => From}};
      {_, false} -> {error, #{no_account => To}}
    end.

-spec now_to_utc_string(erlang:timestamp() | erlang:datetime()) -> string().

now_to_utc_string({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = 
        calendar:now_to_universal_time({MegaSecs, Secs,MicroSecs}),
now_to_utc_string({{Year, Month, Day}, {Hour, Minute, Second}});
now_to_utc_string({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
				[Year, Month, Day, Hour, Minute, Second])).

now_to_utc_binary(Timestamp) ->
    list_to_binary(now_to_utc_string(Timestamp)).
