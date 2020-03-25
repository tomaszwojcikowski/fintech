-module(transactions).

-export([apply/3]).
-export([list/0]).

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