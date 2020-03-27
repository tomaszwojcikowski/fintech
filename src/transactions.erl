-module(transactions).

-export([new/3]).
-export([apply/1]).
-export([list/0]).
-export([create_table/0]).
-export([add_pending/1]).
-export([remove_pending/1]).
-export([list_pending/0]).

-record(pending_transactions, {id, from, to, amount, created}).

new(From, To, Amount) ->
    Id = generate_id(),
    #pending_transactions{id = Id, from = From, to = To, 
        amount = Amount, created = os:timestamp()}.

apply(T = #pending_transactions{}) ->
    fintech_rdbms:transaction(fun(Conn) ->
        apply_t(Conn, T)
    end).

apply_t(Conn, #pending_transactions{id = Id, from = From, to = To, amount = Amount}) ->
    % substract from From
    QueryA = <<"UPDATE `accounts` set `balance` = `balance` - ? where `id` = ?">>,
    % add to To
    QueryB = <<"UPDATE `accounts` set `balance` = `balance` + ? where `id` = ?">>,
    ok = mysql:query(Conn, QueryA, [Amount, From]),
    1 = mysql:affected_rows(Conn),
    ok = mysql:query(Conn, QueryB, [Amount, To]),
    1 = mysql:affected_rows(Conn),
    QueryT = <<"INSERT INTO `transactions` (`id`, `from`, `to`, `amount`) VALUES (?, ?, ?, ?)">>,
    ok = mysql:query(Conn, QueryT, [Id, From, To, Amount]),
    remove_pending(Id),
    {ok, Id}.

list() ->
    Query = <<"SELECT `id`, `from`, `to`, `amount`, `created_at` from `transactions` ORDER BY `created_at`">>,
    {ok, _, Ts} = fintech_rdbms:query(Query),
    lists:map(fun([Id, From, To, Amount, Created]) ->
        #{id => Id, from => From, to => To, amount => Amount, created_at => Created}
    end, Ts).

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

add_pending(T = #pending_transactions{id = Id}) ->
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(pending_transactions, T, write) end),
    {ok, Id}.

remove_pending(Id) ->
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:delete({pending_transactions, Id}) end),
    ok.

list_pending() ->
    {atomic, List} = mnesia:transaction(fun() -> 
        mnesia:foldl(fun(T, Acc) ->
            #pending_transactions{id = Id, from = From, 
                to = To, amount = Amount, created = Created} = T,
            [#{id => Id, from => From, to => To, amount => Amount, created => Created} | Acc]
        end, [], pending_transactions)
    end),
    List.

