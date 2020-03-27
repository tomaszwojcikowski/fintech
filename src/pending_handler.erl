-module(pending_handler).

-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Pending = transactions:list_pending(),
   	Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jiffy:encode(Pending),
        Req0),
    {ok, Req, State}.
