%%% ----------------------------------------------------------------------------
%%% @doc
%%% <pre>
%%% This part has to be provided by the application using gen_httpd:
%%%                               _____
%%%                              |     |
%%%                              |     | <- Application / HTTP Supervisor
%%%                              |_____|
%%%               
%%%                                 |
%%%                                 |
%%% This is provided by gen_httpd:  |
%%%                                 |
%%%                               _____
%%%                              |     |
%%%                              |     | <- gen_httpd supervisor
%%%                              |_____|
%%%               
%%%                               |   \
%%%                               |     \
%%%                             _____       ___
%%%                            |     |    /     \
%%% http workers supervisor -> |     |    |     |   <- gen_tcpd process
%%%                            |_____|    \ ___ /
%%%
%%%                            /  |  \
%%%                          /    |    \
%%%                     ___      ___      ___
%%%                   /     \  /     \  /     \
%%%                   |     |  |     |  |     | <- gen_http_handler processes
%%%                   \ ___ /  \ ___ /  \ ___ /
%%% </pre>
%%% @end
%%% ----------------------------------------------------------------------------
-module(gen_httpd_sup).

-export([start_link/4]).
-export([init/1]).

start_link(Callback, CallbackArgs, Port, Timeout) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE,
		[Callback, CallbackArgs, Port, Timeout]).

init([Callback, Args, Port, Timeout]) ->
	GenHTTPD = {
		gen_httpd,
		{gen_httpd, start_link, [Callback, Args, Port, Timeout]},
		permanent, 1000, worker
	},
	HTTPDHandlerSup = {
		gen_httpd_handler_sup,
		{gen_httpd_handler_sup, start_link, [Callback, Args, Timeout]},
		temporary, infinity, supervisor
	},
	{ok, {{one_for_one, 100, 1}, [GenHTTPD, HTTPDHandlerSup]}}.
