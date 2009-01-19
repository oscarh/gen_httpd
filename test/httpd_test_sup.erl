-module(httpd_test_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_server/5, stop_server/1]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	{ok, {{one_for_one, 10, 1}, []}}.

start_server(Id, Callback, CallbackArg, Port, Pipeline) ->
	HTTPd = child(Id, Callback, CallbackArg, Port, Pipeline),
	supervisor:start_child(?MODULE, HTTPd).

stop_server(Id) ->
	supervisor:terminate_child(?MODULE, Id).

child(Id, Callback, CallbackArg, Port, Pipeline) ->
	HTTPdOpts = [{concurrent_pipeline, Pipeline}],
	SocketOpts = [{reuseaddr, true}],
	HTTPdArgs = [Callback, CallbackArg, Port, 60000, SocketOpts, HTTPdOpts],
	{
		Id, {gen_httpd, start_link, HTTPdArgs},
		permanent, 1000, worker, [gen_httpd, Callback]
	}.
