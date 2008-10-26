-module(httpd_test_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
	httpd_test_sup:start_link().

stop(_) ->
	ok.
