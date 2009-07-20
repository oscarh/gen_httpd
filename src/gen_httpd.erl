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
%%% <a href="#callback:handle_request/6">`handle_request/6'</a>
%%%
%%% <strong>Note!</strong> This callback will <strong>not</strong> be called
%%% by the same process that will call the handle_request later.
%%%
%%% <a name="callback:handle_request/6" />
%%% <pre>
%%% Module:handle_request(Method, URI, Vsn, Headers, Body, State) -> Result
%%%     Types Method = string()
%%%           URI = string()
%%%           Vsn = {Major, Minor}
%%%           Major = Minor = integer()
%%%           Headers = [{Name, Value}]
%%%           Body = {identity, EntityState} | {chunked, EntitiyState} |
%%%                  undefined
%%%           Chunk = binary()
%%%           Reason = term()
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
%%% If the content length is known, `Body' will be `{identity, State}'.
%%% `State' is an opaque data structure that can be passed to
%%% {@link gen_httpd:read_body/2}.
%%%
%%% If the content length is unknown, but the entity body is with chunked
%%% transfer encoding `Body' will instead be `{chunked, State}'. Where
%%% `State' can be passed to {@link gen_httpd:read_body/2}. In this case,
%%% {@link gen_httpd:read_body/2} can read maximum a chunk at the time, and
%%% will have to be called several times to get all chunks.
%%%
%%% If you don't want to use the API to read the entity body, it is possible
%%% to use the `Socket' passed to `init/2' when the connection is
%%% established. This socket can be used through the {@link //gen_tcpd} API.
%%%
%%% Currently, `GET', `HEAD' and `TRACE' methods will return 400 (Bad
%%% Request), if they include indications that they have content. For all
%%% other requests `gen_httpd' will try to read the content length or the
%%% transfer encoding header to find out the content length. If this
%%% fails, and `undefined' `Body' will be passed to the callback, which can
%%% decide to return either 411 (Length Required) or do something else with
%%% the request. If content is sent but isn't read from the socket,
%%% any subsequent request will fail.
%%%
%%% <pre>
%%% Module:handle_continue(Method, URI, Vsn, Headers, State) -> Result
%%%     Types Method = 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' |
%%%                    'DELETE' | 'TRACE' | string()
%%%           URI = string()
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
%%% <pre>
%%% Module:terminate(Reason, State)
%%% Reason = term()
%%% State = term()
%%% </pre>
%%% This callback is called when the connection is closed. It's not a
%%% termination of the HTTP server as such. But only the connection.
%%% @end
%%% ----------------------------------------------------------------------------
-module(gen_httpd).
-behaviour(gen_tcpd).

-export([start_link/5, start_link/6, read_body/3, port/1, stop/1]).
-export([init/1, handle_connection/2, handle_info/2, terminate/2]).
-export([behaviour_info/1]).

-include_lib("gen_tcpd/include/gen_tcpd_types.hrl").

-include("gen_httpd_types.hrl").

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
-spec start_link(atom(), term(), 0..65535, timeout(), [_]) ->
	{ok, pid()} | {error, term()} | ignore.
start_link(Callback, CallbackArg, Port, Timeout, SockOpts) ->
	validate_sock_opts(SockOpts),
	InitArg = [Callback, CallbackArg, Timeout],
	Options = [
		{acceptors, 20},
		{socket_options, [{active, false}, binary | SockOpts]}
	],
	gen_tcpd:start_link(?MODULE, InitArg, tcp, Port, Options).
	
%% @spec start_link(Callback, CallbackArg, Port, Timeout, SockOpts,
%%                  SSLOpts) -> {ok, Pid}
%% Callback = atom()
%% CallbackArg = term()
%% Port = integer()
%% Timeout = integer()
%% SockOpts = [SockOpt]
%% SSLOpts = [SSLOpt]
%% Pid = pid()
%% @doc Starts a gen_httpd process with an SSL backend and links to it.
%%
%% For <code>SockOpts</code> and <code>SSLOpts</code>, see backend module
%% <a href="http://www.erlang.org/doc/man/ssl.html"><code>ssl</code></a>.
%%
%% This function should normally be called from a supervisor.
-spec start_link(atom(), term(), 0..65535, timeout(), [_], [_]) ->
	{ok, pid()} | {error, term()} | ignore.
start_link(Callback, CallbackArg, Port, Timeout, SockOpts, SSLOpts) ->
	validate_sock_opts(SockOpts),
	InitArg = [Callback, CallbackArg, Timeout],
	Options = [
		{acceptors, 20},
		{ssl_accept_timeout, 30000},
		{socket_options, [{active, false}, binary | SockOpts] ++ SSLOpts}
	],
	gen_tcpd:start_link(?MODULE, InitArg, ssl, Port, Options).

%% @spec port(Ref) -> {ok, Port}
%% Ref = pid()
%% Port = integer()
%% @doc
%% Returns the port a gen_tcpd process is listening on.
%% @end
-spec port(pid()) -> 1..65535.
port(Ref) ->
	gen_tcpd:port(Ref).

%% @spec (Pid) -> ok
%% Pid = pid()
%% @doc
%% Stops the instance of gen_httpd. Mainly used during testing since
%% gen_httpd should otherwise be part of a supervision tree.
%% @end
-spec stop(pid()) -> ok.
stop(Pid) ->
	gen_tcpd:stop(Pid).

%% @spec (Length, Timeout, State) -> Result
%%   Length = integer() | complete
%%   Timeout = integer() | infinity
%%   Result = {ok, {Body, NextState}} | {ok, {Body, complete}} |
%%            {chunk, {Chunk, NextState}} | {trailers, Trailers} | 
%%            {error, Reason}
%%   Chunk = binary()
%%   Body = binary()
%%   Trailers = {string(), string()}
%% @doc
%% Reads an entity body. This is called by applications using gen_httpd to
%% read entity bodies.
%%
%% `Length' can aither be an integer, or the atom `complete'.
%% If `Length' is  the atom `complete', the complete
%% entity body will be read and returned.
%% 
%% If is an integer, it must be >= 1. This means that `Length' bytes will be
%% read from the socket. If the remaining body is less than `Length' the
%% remaining body will be returned.
%%
%% Timeout is the milliseconds to wait for data on the socket.
%% 
%% If the transfer encoding is "identity", the function returns
%% `{ok, {Body, NextState}}' 
%% NextState' is the updated state to use in the next call to this function.
%% If the returned body is the complete body or last piece of the body,
%% `NextState' will be the atom `done'.
%%
%% If the transfer encoding is "chunked" the function will return
%% `{chunk, {Chunk, NextState}' or `{trailers, Trailers}'.
%% `NextState' is the updated state to pass to the next call to this
%% function.  This functions shouldn't be called again after `{trailers,
%% Trailers}' has been returned.
%% 
%% The transfer encoding is indicated by the first element of the `Body'
%% tuple in the <a href="#callback:handle_request/6">`handle_request/6'</a>
%% callback. Currently only "identity" and "chunked" are supported by
%% gen_httpd. These are represented by the atoms `identity' and `chunked'.
%% @end
-spec read_body(pos_integer() | complete, timeout(), _) ->
    {ok, {binary(), _}} | {ok, {binary(), done}} |
    {chunk, {binary(), _}} | {trailers, [header()]}.
read_body(Length, Timeout, State) ->
	ghtp_request:read_body(Length, Timeout, State).

%% @hidden
-spec init(_) -> {ok, #gen_httpd{}}.
init([Callback, CallbackArg, Timeout]) ->
	process_flag(trap_exit, true),
	State = #gen_httpd{
		callback = Callback,
		callback_arg = CallbackArg,
		timeout = Timeout
	},
	{ok, State}.

%% @hidden
-spec handle_connection(gen_tcpd_socket(), #gen_httpd{}) -> _.
handle_connection(Socket, State) ->
	CB = State#gen_httpd.callback,
	CBArg = State#gen_httpd.callback_arg,
	Timeout = State#gen_httpd.timeout,
	ghtp_conn:init(self(), CB, CBArg, Socket, Timeout).

%% @hidden
-spec handle_info(_, #gen_httpd{}) -> noreply.
handle_info({'EXIT', _, normal}, _) ->
	noreply;
handle_info({'EXIT', Pid, Reason}, _) ->
	Report = ["HTTP handler died", {pid, Pid}, {reason, Reason}],
	error_logger:error_report(Report),
	noreply;
handle_info(_, _) ->
	noreply.

%% @hidden
-spec terminate(_, #gen_httpd{}) -> ok.
terminate(_Reason, _State) ->
	ok.

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
-spec behaviour_info(callbacks) -> [{atom(), non_neg_integer()}].
behaviour_info(callbacks) ->
	[
		{init,2},
		{handle_request, 6},
		{handle_continue, 5},
		{terminate, 2}
	].
