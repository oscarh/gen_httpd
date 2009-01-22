-module(ghttpd_conn).
-export([start/6]).

-record(ghtp_conn, {
		parent,
		reader,
		callback,
		callback_arg,
		socket,
		sock_timeout,
		pipeline_queue
	}).

start(Parent, Callback, CallbackArg, Socket, SockTimeout, PipelineLength) ->
	State = #ghtp_conn{
		parent = Parent,
		callback = Callback,
		callback_arg = CallbackArg,
		socket = Socket,
		sock_timeout = SockTimeout,
		pipeline_queue = ghtp_pl_queue:new(PipelineLength)
	},
	process_flag(trap_exit, true),
	Reader = spawn_link(ghtp_reader, start, [self(), Socket]),
	loop(State#ghtp_conn{reader = Reader}).

loop(#ghtp_conn{reader = Reader} = State) ->
	Timeout = case ghtp_pl_queue:is_empty(State#ghtp_conn.pipeline_queue) of
		true  -> State#ghtp_conn.sock_timeout;
		false -> infinity
	end,
	NextState = receive
		% XXX:
		% This would be changed to all the matches in the reader to try
		% out active once instead of recv/2 in a reader process.
		% The reader process might not scale as well as using active once
		{request, Request} ->
			handle_request(Request, State);
		{response, Id, Response} ->
			handle_response(Id, Response, State);
		{'EXIT', Reader, {bad_request, Reason}} ->
			handle_bad_request(Reason, State);
		% XXX:
		% Should we really just close and exit, or should we try to send
		% responses back first? It might not be closed for the receiving
		% part?
		{'EXIT', Reader, closed} ->
			close_and_exit(State);
		{'EXIT', Reader, Reason} ->
			exit(Reason); % XXX: is this good?
		{'EXIT', Pid, Reason} ->
			handle_exit(Pid, Reason, State)
	after
		Timeout ->
			close_and_exit(State)
	end,
	loop(NextState).

handle_request(_Request, State) ->
	State.

handle_response(_Id, _Response, State) ->
	State.

handle_exit(_Response, _Reason, State) ->
	State.

handle_bad_request(_Reason, State) ->
	State.

close_and_exit(#ghtp_conn{parent = Parent, reader = Reader, socket = Socket}) ->
	% Kill the reader since it's no longer needed.
	% (But we don't want to get the exit signal back.)
	unlink(Reader),
	exit(Reader, kill),
	gen_tcpd:close(Socket),
	% Don't need to send {'EXIT', self(), normal} to the parent
	unlink(Parent),
	exit(normal).
