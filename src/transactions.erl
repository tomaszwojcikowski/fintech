-module(transactions).

-export([apply/3]).
-export([list/0]).

apply(From, From, _Amount) ->
    erlang:error("From and To cannot be the same!"); 
apply(From, To, Amount) 
    when is_integer(Amount) andalso Amount > 0->
    ok = fintech_rdbms:transaction(fun(Conn) ->
        apply(Conn, From, To, Amount)
    end).

apply(Conn, From, To, Amount) ->
    QueryA = <<"UPDATE `accounts` set `amount` = `amount` + ? where `id` = ?">>,
    mysql:query(QueryA, [From, -Amount]),
    1 = mysql:affected_rows(Conn),
    mysql:query(QueryA, [To, Amount]),
    1 = mysql:affected_rows(Conn),
    Id = uuid:uuid_to_string(uuid:get_v4()),
    QueryT = <<"INSERT INTO `transactions` (`id`, `from`, `to`, `amount`) VALUES (?, ?, ?, ?)">>,
    ok = mysql:query(Conn, QueryT, [Id, From, To, Amount]).

list() ->
    Query = <<"SELECT * from `transactions` ORDER BY `created_at`">>,
    {ok, _, Ts} = fintech_rdbms:query(Query),
    Ts.