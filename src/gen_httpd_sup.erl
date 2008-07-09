-module(gen_httpd_sup).

-export([start_link/3, spawn_handler/0]).
-export([init/1]).

start_link(Callback, Args, Timeout) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE,
		[Callback, Args, Timeout]).

init([Callback, Args, Timeout]) ->
	Handler = {
		gen_httpd_handler,
		{gen_httpd_handler, start_link, [Callback, Args, Timeout]},
		temporary, 5000, worker
	},
	% Don't really care if children are dying.
	{ok, {{simple_one_for_one, 1000, 1}, [Handler]}}.

spawn_handler() ->
	supervisor:start_child(?MODULE, []).
