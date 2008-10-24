-module(gen_httpd).
-behaviour(gen_tcpd).

-export([start_link/6, start_link/7]).
-export([init/1, handle_connection/2, handle_info/2, terminate/2]).
-export([wait_for_socket/1]).

-export([behaviour_info/1]).

-record(state, {callback, callback_args, timeout, pipeline}).

start_link(Callback, CallbackArgs, Port, Timeout, SockOpts, Options) ->
	validate_sock_opts(SockOpts),
	validate_options(Options),
	Opts = [{active, false} | SockOpts],
	InitArgs = [Callback, CallbackArgs, Timeout, Options],
	gen_tcpd:start_link(?MODULE, InitArgs, tcp, Port, Opts).
	
start_link(Callback, CallbackArgs, Port, Timeout, SockOpts, SSL, Options) ->
	validate_sock_opts(SockOpts),
	validate_options(Options),
	Opts = [{active, false} | SockOpts] ++ SSL,
	InitArgs = [Callback, CallbackArgs, Timeout, Options],
	gen_tcpd:start_link(?MODULE, InitArgs, ssl, Port, Opts).

init([Callback, CallbackArgs, Timeout, Options]) ->
	process_flag(trap_exit, true),
	Pipeline = proplists:get_value(pipeline, Options, 0),
	State = #state{
		callback = Callback,
		callback_args = CallbackArgs,
		timeout = Timeout,
		pipeline = Pipeline
	},
	{ok, State}.

handle_connection(Socket, State) ->
	Pid = spawn_link(?MODULE, wait_for_socket, [State]),
	ok = gen_tcpd:controlling_process(Socket, Pid),
	Pid ! {socket, Socket},
	{noreply, State}.

handle_info({'EXIT', _, closed}, State) ->
	{noreply, State};
handle_info({'EXIT', _, tcp_timeout}, State) ->
	{noreply, State};
handle_info({'EXIT', _, normal}, State) ->
	{noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
	Report = ["HTTP handler died", {pid, Pid}, {reason, Reason}],
	error_logger:error_report(Report),
	{noreply, State};
handle_info(_, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

wait_for_socket(State) ->
	receive
		{socket, Socket} ->
			Socket
	end,
	CB = State#state.callback,
	CBArgs = State#state.callback_args,
	Timeout = State#state.timeout,
	Pipeline = State#state.pipeline,
	case catch gen_httpd_handler:start(CB, CBArgs, Socket, Timeout, Pipeline) of
		{'EXIT', Reason} -> exit(Reason);
		_                -> ok
	end.


validate_sock_opts([{active, _} = O | _]) ->
	exit({bad_socket_option, O});
validate_sock_opts([binary = O | _]) ->
	exit({bad_socket_option, O});
validate_sock_opts([list = O | _]) ->
	exit({bad_socket_option, O});
validate_sock_opts([_ | T]) ->
	validate_sock_opts(T);
validate_sock_opts([]) ->
	ok.

validate_options([{pipeline, N}| T]) when is_integer(N) ->
	validate_options(T);
validate_options([O | _]) ->
	exit({bad_option, O});
validate_options([]) ->
	ok.

behaviour_info(callbacks) ->
	[
		{init,1},
		{handle_get, 5},
		{handle_put, 6},
		{handle_head, 5},
		{handle_post, 6},
		{handle_options, 5},
		{handle_trace, 5},
		{handle_connect, 6}
	].
