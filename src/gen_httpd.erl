%%% ----------------------------------------------------------------------------
%%% Copyright 2008
%%% Martin Carlson, martin@martinc.eu
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
%%% @author Martin Carlson <martin@martinc.eu>
%%% @author Oscar Hellström <oscar@hellstrom.st> [http://oscar.hellstrom.st]
%%% @version {@version}, {@date}, {@time}
%%% @doc
%%% The gen_httpd behaivour implements a generic HTTP server interface.
%%%
%%% <pre>
%%% gen_httpd module            Callback module
%%% ----------------            ---------------
%%% gen_httpd:start_link -----> Module:init/2
%%% -                    -----> Module:handle_request/6
%%% -                    -----> Module:handle_continue/5
%%% -                    -----> Module:terminate/2
%%% </pre>
%%%
%%% == Callbacks ==
%%% <pre>
%%% Module:init(Socket, Arg) -> Result
%%%     Types ConnInfo = #gen_httpd_conn{}
%%%           Arg = term()
%%%           Result = {ok, State} | {stop, Reason}
%%% </pre>
%%% The init function is called every time a new connection is established
%%% to a client. The <code>Socket</code> argument is a <code>gen_tcpd</code>
%%% (note the d in gen_tcpd) data structure representing the socket. This
%%% data structure can be used to call functions such as
%%% {@link //gen_tcpd/gen_tcpd:peername/1},
%%% {@link //gen_tcpd/gen_tcpd:sockname/1} etc.
%%% 
%%% <code>Arg</code> is the <code>CallbackArg</code> passed
%%% to {@link start_link/5} or {@link start_link/6}.
%%%
%%% This function should return <code>{ok, State}</code> where
%%% <code>State</code> is the State which will be passed to
%%% <a href="#callback:handle_request/5"> <code>handle_request/5</code>
%%%
%%% <strong>Note!</strong> This
%%% callback will <strong>not</strong> be called by the same process that
%%% will call the handle_request later if <code>{ok, State}</code>
%%% is returned.
%%%
%%% <a name="callback:handle_request/5">
%%% <pre>
%%% Module:handle_request(Method, URI, Vsn, Headers, RequestBody, State) -> Result
%%%     Types Method = 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' |
%%%                    'DELETE' | 'TRACE' | string()
%%%           URI = '*' | {absoluteURI, http |
%%%                 https, Host=string(), Port=int() |
%%%                 undefined, Path=string()} |
%%%                 {scheme, Scheme=string(), string()} | {abs_path, string} |
%%%                 string()
%%%           Vsn = {Major, Minor}
%%%           Major = Minor = integer()
%%%           Headers = [{Name, Value}]
%%%           RequestBody = binary()
%%%           Name = Value = string()
%%%           State = term()
%%%           Result = {reply, Status, Headers, Body, State}
%%%           Status = StatusCode | {StatusCode, Description}
%%%           StatusCode = integer()
%%%           Description = string()
%%%           Body = io_list()
%%% </pre>
%%% Handle a HTTP request.
%%%
%%%
%%% <pre>
%%% Module:handle_continue(Method, URI, Vsn, Headers, State) -> Result
%%%     Types Method = 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' |
%%%                    'DELETE' | 'TRACE' | string()
%%%           URI = '*' | {absoluteURI, http | https, Host=string(), Port=int() |
%%%                 undefined, Path=string()} |
%%%                 {scheme, Scheme=string(), string()} | {abs_path, string} |
%%%                 string()
%%%           Vsn = {Major, Minor}
%%%           Major = Minor = integer()
%%%           Headers = [{Name, Value}]
%%%           Name = Value = string()
%%%           State = term()
%%%           Result = {continue, Headers, NextState} |
%%%                    {reply, Status, Headers, Body, State}
%%%           Status = StatusCode | {StatusCode, Description}
%%%           StatusCode = integer()
%%%           Description = string()
%%%           Body = io_list()
%%% </pre>
%%% If the HTTP client sends the the <code>100-continue</code> token in the
%%% <code>Expect</code> header this function will be called to allow the
%%% callback module to decide if it wants to continue with the request or not.
%%% For more information on <code>Expect: 100-continue</code> see:
%%% <a
%%% href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.1.1">HTTP/1.1
%%% Section 10.1.1</a> and <a
%%% href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec8.html#sec8.2.3">Use
%%% of the 100 (Continue) Status</a>.
%%%
%%% If the server wishes to continue with the request, i.e. send a status
%%% 100 Continue and read the request body, this function should return
%%% <code>{continue, State}</code>. If
%%% <code>{reply, Status, Headers, Body, State}</code> is returned the
%%% server will return an appropriate HTTP response and try to read the next
%%% request from the client.
%%%
%%% <strong>Note!</strong> This
%%% callback will <strong>not</strong> be called by the same process that
%%% will call the handle_request later if <code>{continue, State}</code>
%%% is returned.
%%% @end
%%% ----------------------------------------------------------------------------
-module(gen_httpd).
-behaviour(gen_tcpd).

-export([start_link/5, start_link/6, port/1]).
-export([init/1, handle_connection/2, handle_info/2, terminate/2]).
-export([wait_for_socket/1]).
-export([behaviour_info/1]).

-record(gen_httpd, {callback, callback_arg, timeout}).

%% @spec start_link(Callback, CallbackArg, Port, Timeout, SockOpts) ->
%%                               {ok, Pid}
%% Callback = atom()
%% CallbackArg = term()
%% Port = integer()
%% Timeout = integer()
%% SockOpts = [SockOpt]
%% Pid = pid()
%% @doc Starts a gen_httpd process and links to it.
%% The process created will call <code>Callback:init/2</code> with
%% <code>CallbackArg</code> to initialise an internal state.
%% The HTTP server will listen on port <code>Port</code> and keep persistent
%% connections open for <code>Timeout</code> milliseconds.
%%
%% For <code>SockOpts</code>, see backend module
%% <a
%% href="http://www.erlang.org/doc/man/gen_tcp.html"><code>gen_tcp</code></a>.
%%
%% This function should normally be called from a supervisor.
start_link(Callback, CallbackArg, Port, Timeout, SockOpts) ->
	validate_sock_opts(SockOpts),
	Opts = [{active, false}, binary | SockOpts],
	InitArg = [Callback, CallbackArg, Timeout],
	gen_tcpd:start_link(?MODULE, InitArg, tcp, Port, Opts).
	
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
%%
%% For <code>SockOpts</code> and <code>SSLOpts</code>, see backend module
%% <a href="http://www.erlang.org/doc/man/ssl.html"><code>ssl</code></a>.
%%
%% This function should normally be called from a supervisor.
start_link(Callback, CallbackArg, Port, Timeout, SockOpts, SSL) ->
	validate_sock_opts(SockOpts),
	Opts = [{active, false}, binary | SockOpts] ++ SSL,
	InitArg = [Callback, CallbackArg, Timeout],
	gen_tcpd:start_link(?MODULE, InitArg, ssl, Port, Opts).

%% @spec port(Ref) -> {ok, Port}
%% Ref = pid()
%% Port = integer()
%% @doc
%% Returns the port a gen_tcpd process is listening on.
%% @end
-spec port(pid()) -> {ok, 1..65535}.
port(Ref) ->
	gen_tcpd:port(Ref).

%% @hidden
init([Callback, CallbackArg, Timeout]) ->
	process_flag(trap_exit, true),
	State = #gen_httpd{
		callback = Callback,
		callback_arg = CallbackArg,
		timeout = Timeout
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
	CB = State#gen_httpd.callback,
	CBArg = State#gen_httpd.callback_arg,
	Timeout = State#gen_httpd.timeout,
	ghtp_conn:init(self(), CB, CBArg, Socket, Timeout).

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

%% @hidden
behaviour_info(callbacks) ->
	[
		{init,2},
		{handle_request, 6},
		{handle_continue, 5},
		{terminate, 2}
	].
