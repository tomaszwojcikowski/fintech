-module(transactions).

-export([add/3]).

add(From, From, _Amount) ->
    erlang:error("From and To cannot be the same!"); 
add(From, To, Amount) 
when is_integer(Amount), Amount > 0 ->
    Id = uuid:uuid4(),
    Query = <<"INSERT INTO `transactions` (`id`, `from`, `to`, `amount`) VALUES (?, ?, ?, ?)">>,
    ok = fintech_rdbms:query(Query, [Id, From, To, Amount]).

