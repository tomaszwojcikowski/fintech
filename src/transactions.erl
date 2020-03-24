-module(transactions).

-export([add/3]).
-export([list/0]).

add(From, From, _Amount) ->
    erlang:error("From and To cannot be the same!"); 
add(From, To, Amount) 
when is_integer(Amount), Amount > 0 ->
    Id = uuid:uuid_to_string(uuid:get_v4()),
    Query = <<"INSERT INTO `transactions` (`id`, `from`, `to`, `amount`) VALUES (?, ?, ?, ?)">>,
    ok = fintech_rdbms:transaction(fun(Conn) ->
        ok = mysql:query(Conn, Query, [Id, From, To, Amount])
    end),
    Id.

list() ->
    Query = <<"SELECT * from `transactions` ORDER BY `created_at`">>,
    {ok, _, Ts} = fintech_rdbms:query(Query),
    Ts.

