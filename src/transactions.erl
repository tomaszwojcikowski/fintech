-module(transactions).


add(From, From, _Amount) ->
    erlang:error("From and To cannot be the same!"); 
add(From, To, Amount) 
when is_integer(Amount), Amount > 0 ->
    Query = <<"INSERT INTO `transactions` (`id`, `from`, `to`, `amount`) VALUES (?, ?, ?, ?)">>,
    fintech_rdbms:query(Query, [Id, From, To, Amount])

