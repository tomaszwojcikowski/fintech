-module(transactions).

-export([new/3]).
-export([apply/1]).
-export([list/0]).
-export([create_table/0]).
-export([add_pending/1]).
-export([remove_pending/1]).
-export([list_pending/0]).
-export([set_executing/1]).
-export([check_executing_to/1]).

-type id() :: binary().
-type amount() :: non_neg_integer().

-define(TABLE, pending_transactions).

-record(pending_transactions, {id :: id(), 
                               from :: accounts:id(), 
                               to :: accounts:id(), 
                               amount :: amount(), 
                               created :: erlang:timestamp(),
                               executing = false :: boolean()}).
-type transaction() :: #pending_transactions{}.


new(From, To, Amount) ->
    Id = generate_id(),
    #pending_transactions{id = Id, from = From, to = To, 
        amount = Amount, created = os:timestamp()}.

-spec apply(transaction()) -> {ok, id()} | {error, executing}.
apply(T = #pending_transactions{}) ->
    case check_executing_to(T) of
        ok ->
            set_executing(T),
            fintech_rdbms:transaction(fun(Conn) ->
                apply_t(Conn, T)
            end);
        executing ->
            {error, executing}
    end.

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
    mnesia:create_table(?TABLE,
                        [{ram_copies, [node()]},
                         {type, set},
                         {attributes, record_info(fields, pending_transactions)}]),
    Result = mnesia:add_table_copy(?TABLE, node(), ram_copies),
    case Result of
        {atomic, ok} -> ok;
        {aborted,{already_exists,_,_}} -> ok
    end.

add_pending(T = #pending_transactions{id = Id}) ->
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(?TABLE, T, write) end),
    {ok, Id}.

remove_pending(_T = #pending_transactions{id = Id}) ->
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:delete({?TABLE, Id}) end),
    ok.

-spec list_pending() -> [map()].
list_pending() ->
    {atomic, List} = mnesia:transaction(fun() -> 
        mnesia:foldl(fun(T, Acc) ->
            #pending_transactions{id = Id, from = From, 
                to = To, amount = Amount, created = Created} = T,
            [#{id => Id, from => From, to => To, amount => Amount, created => Created} | Acc]
        end, [], ?TABLE)
    end),
    List.

-spec set_executing(transaction()) -> ok.
set_executing(T) ->
    T2 = T#pending_transactions{executing = true},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(?TABLE, T2, write) end),
    ok.

-spec check_executing_to(accounts:id()) -> ok | executing.
check_executing_to(To) ->
    Ids = mnesia:dirty_select(?TABLE, [{#pending_transactions{to = To, id='$1', _='_'}, [], ['$1']}]),
    case Ids of
        [] -> ok;
        _ -> executing   
    end.
