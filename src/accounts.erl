-module(accounts).

-expot([load/1]).

load(File) ->
    {ok, Bin} = file:read_file(File),
    Accounts = jiffy:decode(Bin, [return_maps]),
    ok.


to_db(Accounts) ->
    {ok, Opts} = application:get_env(fintech, mysql),
    {ok, Mysql} = mysql:start_link(Opts).