-module(new_handler).

-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    Data = jiffy:decode(Body, [return_maps]),
    #{<<"from">> := From, <<"to">> := To,
      <<"amount">> := Amount} =
	Data,
    T = transactions:new(From, To, Amount),
    {Code, Result} = handle_new(T),
    Req1 = cowboy_req:reply(Code,
			    #{<<"content-type">> => <<"application/json">>},
			    jiffy:encode(Result), Req),
    {ok, Req1, State}.

handle_new(T) ->
    case transactions:validate(T) of
      ok ->
        case accounts:apply_transaction(T) of
          {ok, Id} -> {200, #{id => Id}};
          {error, timeout} -> {408, #{timeout => true}};
          {error, executing} -> retry(T);
          {error, insuficient_funds} ->
            {400, #{insuficient_funds => true}}
        end;
      {error, Error} -> {400, #{validation_error => Error}}
    end.

retry(T) ->
    Interval = rand:uniform(500),
    timer:sleep(Interval),
    handle_new(T).
