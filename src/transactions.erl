-module(transactions).

-export([apply/3]).
-export([list/0]).
-export([create_table/0]).
-export([add_pending/3, add_pending/4]).
-export([remove_pending/1]).
-export([list_pending/0]).

-record(pending_transactions, {id, from, to, amount}).

apply(From, From, _Amount) ->
    erlang:error("From and To cannot be the same!"); 
apply(From, To, Amount) 
    when is_integer(Amount) andalso Amount > 0->
    fintech_rdbms:transaction(fun(Conn) ->
        apply(Conn, From, To, Amount)
    end).

apply(Conn, From, To, Amount) ->
    % substract from From
    QueryA = <<"UPDATE `accounts` set `balance` = `balance` - ? where `id` = ?">>,
    % add to To
    QueryB = <<"UPDATE `accounts` set `balance` = `balance` + ? where `id` = ?">>,
    ok = mysql:query(Conn, QueryA, [Amount, From]),
    1 = mysql:affected_rows(Conn),
    ok = mysql:query(Conn, QueryB, [Amount, To]),
    1 = mysql:affected_rows(Conn),
    Id = generate_id(),
    QueryT = <<"INSERT INTO `transactions` (`id`, `from`, `to`, `amount`) VALUES (?, ?, ?, ?)">>,
    ok = mysql:query(Conn, QueryT, [Id, From, To, Amount]),
    {ok, Id}.

list() ->
    Query = <<"SELECT * from `transactions` ORDER BY `created_at`">>,
    {ok, _, Ts} = fintech_rdbms:query(Query),
    Ts.

generate_id() ->
    erlang:iolist_to_binary(uuid:uuid_to_string(uuid:get_v4())).


create_table() ->
    mnesia:create_table(pending_transactions,
                        [{ram_copies, [node()]},
                         {type, set},
                         {attributes, record_info(fields, pending_transactions)}]),
    Result = mnesia:add_table_copy(pending_transactions, node(), ram_copies),
    case Result of
        {atomic, ok} -> ok;
        {aborted,{already_exists,_,_}} -> ok
    end.

add_pending(From, To, Amount) ->
    Id = generate_id(),
    add_pending(Id, From, To, Amount).
add_pending(Id, From, To, Amount) ->
    T = #pending_transactions{id = Id, from = From, to = To, amount = Amount},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(pending_transactions, T, write) end),
    {ok, Id}.

remove_pending(Id) ->
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:delete({pending_transactions, Id}) end),
    ok.

list_pending() ->
    {atomic, List} = mnesia:transaction(fun() -> 
        mnesia:foldl(fun(T, Acc) ->
            #pending_transactions{id = Id, from = From, to = To, amount = Amount} = T,
            [#{id => Id, from => From, to => To, amount => Amount} | Acc]
        end, [], pending_transactions)
    end),
    List.