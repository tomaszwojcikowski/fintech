%%%-------------------------------------------------------------------
%% @doc fintech public API
%% @end
%%%-------------------------------------------------------------------

-module(fintech_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([load_accounts/0]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
            {'_', [{"/", fintech_handler, []}]},
            {'_', [{"/new", new_handler, []}]},
            {'_', [{"/list", list_handler, []}]},
            {'_', [{"/pending", pending_handler, []}]}
        ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
        ),    
    fintech_rdbms:start_pool(),
    fintech_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

load_accounts() ->
    {ok, File} = application:get_env(fintech, accounts_file),
    accounts:load(File).