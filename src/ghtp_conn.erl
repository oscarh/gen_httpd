-module(ghttpd_conn).
-export([start/6]).

-record(ghtp_conn, {
		parent,
		reader_pid,
		callback,
		callback_arg,
		socket,
		sock_timeout,
		pipeline_len,
		pipeline_queue
	}).

start(Parent, Callback, CallbackArg, Socket, SockTimeout, PipelineLength) ->
	State = #ghtp_conn{
		parent = Parent,
		callback = Callback,
		callback_arg = CallbackArg,
		socket = Socket,
		sock_timeout = SockTimeout,
		pipeline_len = PipelineLength,
		pipeline_queue = ghtp_pl_queue:new(PipelineLength)
	},
	process_flag(trap_exit, true),
	Reader = spawn_link(ghtp_reader, start, [self(), Socket]),
	loop(State#ghtp_conn{reader_pid = Reader}).

loop(#ghtp_conn{reader_pid = Reader} = State) ->
	Timeout = case ghtp_pl_queue:is_empty(State#ghtp_conn.pipeline_queue) of
		true  -> State#ghtp_conn.sock_timeout;
		false -> infinity
	end,
	NextState = receive
		{request, Request} ->
			handle_request(Request, State);
		{response, Response} ->
			handle_response(Response, State);
		{'EXIT', Reader, bad_request} ->
			handle_response(bad_request(State), State);
		{'EXIT', Pid, Reason} ->
			handle_exit(Pid, Reason, State)
	after
		Timeout ->
			% Don't need to send these to the parent
			unlink(State#ghtp_conn.parent),
			exit(normal)
	end,
	loop(NextState).

handle_request(_Request, State) ->
	State.

handle_response(_Response, State) ->
	State.

handle_exit(_Response, _Reason, State) ->
	State.

bad_request(State) ->
	State.
