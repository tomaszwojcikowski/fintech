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
            {'_', [{"/ping", ping_handler, []}, 
                   {"/list", list_handler, []},
                   {"/new", new_handler, []},
                   {"/pending", pending_handler, []}]}
        ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
        ),    
    fintech_rdbms:start_pool(),
    accounts:create_table(),
    transactions:create_table(),
    fintech_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

load_accounts() ->
    {ok, File} = application:get_env(fintech, accounts_file),
    accounts:load(code:priv_dir(fintech) ++ "/" ++ File).