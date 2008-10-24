%%% ----------------------------------------------------------------------------
%%% Copyright 2008
%%% Martin Carlson, martin@erlang-consulting.com
%%% Oscar Hellström, oscar@hellstrom.st
%%%
%%% All rights reserved
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in the
%%%       documentation and/or other materials provided with the distribution.
%%%     * The names of its contributors may not be used to endorse or promote
%%%       products derived from this software without specific prior written
%%%       permission.
%%%
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%% ----------------------------------------------------------------------------
%%% @copyright 2008 Martin Carlson, Oscar Hellström
%%% @author Martin Carlson <martin@erlang-consulting.com>
%%% @author Oscar Hellström <oscar@hellstrom.st> [http://oscar.hellstrom.st]
%%% @version {@version}, {@date}, {@time}
%%% @doc
%%% The gen_httpd behaivour implements a generic HTTP server interface.
%%% @end
%%% ----------------------------------------------------------------------------
-module(gen_httpd).
-behaviour(gen_tcpd).

-export([start_link/6, start_link/7]).
-export([init/1, handle_connection/2, handle_info/2, terminate/2]).
-export([wait_for_socket/1]).

-export([behaviour_info/1]).

-record(state, {callback, callback_args, timeout, pipeline}).

%% @spec start_link(Callback, CallbackArg, Port, Timeout, SockOpts, Options) ->
%%                               {ok, Pid}
%% Callback = atom()
%% CallbackArg = term()
%% Port = integer()
%% Timeout = integer()
%% SockOpts = [SockOpt]
%% Options = [Opt]
%% Pid = pid()
%% @doc Starts a gen_httpd process and links to it.
%% This function should normally be called from a supervisor.
start_link(Callback, CallbackArg, Port, Timeout, SockOpts, Options) ->
	validate_sock_opts(SockOpts),
	validate_options(Options),
	Opts = [{active, false} | SockOpts],
	InitArgs = [Callback, CallbackArg, Timeout, Options],
	gen_tcpd:start_link(?MODULE, InitArgs, tcp, Port, Opts).
	
%% @spec start_link(Callback, CallbackArg, Port, Timeout, SockOpts,
%%                  SSL, Options) -> {ok, Pid}
%% Callback = atom()
%% CallbackArg = term()
%% Port = integer()
%% Timeout = integer()
%% SockOpts = [SockOpt]
%% SSL = [SSLOpt]
%% Options = [Opt]
%% Pid = pid()
%% @doc Starts a gen_httpd process with an SSL backend and links to it.
%% This function should normally be called from a supervisor.
start_link(Callback, CallbackArg, Port, Timeout, SockOpts, SSL, Options) ->
	validate_sock_opts(SockOpts),
	validate_options(Options),
	Opts = [{active, false} | SockOpts] ++ SSL,
	InitArgs = [Callback, CallbackArg, Timeout, Options],
	gen_tcpd:start_link(?MODULE, InitArgs, ssl, Port, Opts).

%% @hidden
init([Callback, CallbackArg, Timeout, Options]) ->
	process_flag(trap_exit, true),
	Pipeline = proplists:get_value(concurrent_pipeline, Options, 1),
	State = #state{
		callback = Callback,
		callback_args = CallbackArg,
		timeout = Timeout,
		pipeline = Pipeline
	},
	{ok, State}.

%% @hidden
handle_connection(Socket, State) ->
	Pid = spawn_link(?MODULE, wait_for_socket, [State]),
	ok = gen_tcpd:controlling_process(Socket, Pid),
	Pid ! {socket, Socket},
	{noreply, State}.

%% @hidden
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

%% @hidden
terminate(_Reason, _State) ->
	ok.

%% @private
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

validate_options([{concurrent_pipeline, N}| T]) when is_integer(N) ->
	validate_options(T);
validate_options([O | _]) ->
	exit({bad_option, O});
validate_options([]) ->
	ok.

%% @hidden
behaviour_info(callbacks) ->
	[
		{init,1},
		{handle_get, 5},
		{handle_put, 6},
		{handle_head, 5},
		{handle_post, 6},
		{handle_options, 5},
		{handle_trace, 5},
		{handle_connect, 6},
		{terminate, 2}
	].
