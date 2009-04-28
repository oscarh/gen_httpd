-module(http_methods).
-export([
        init/2,
        handle_continue/5,
        handle_request/6,
        terminate/2
    ]).

-include_lib("eunit/include/eunit.hrl").

%%% eunit tests
http_methods_test_() ->
	{setup, fun listen/0, fun stop/1, {with, [
				fun http_get/1,
				fun http_options/1,
				fun http_head/1,
				fun http_post/1,
				fun http_put/1,
				fun http_delete/1,
				fun http_trace/1,
				fun http_non_standard/1
			]}}.

http_get({_, Port}) ->
	method("GET", Port).

http_options({_, Port}) ->
	method("OPTIONS", Port).

http_head({_, Port}) ->
	method("HEAD", Port).

http_post({_, Port}) ->
	method("POST", Port).

http_put({_, Port}) ->
	method("PUT", Port).

http_delete({_, Port}) ->
	method("DELETE", Port).

http_trace({_, Port}) ->
	method("TRACE", Port).

http_non_standard({_, Port}) ->
	method("NONSTANDARD", Port).

method(Method, Port) ->
	{ok, S} = http_client:connect(Port, gen_tcp, []),
	{ok, Response} = send_receive(S, Method, "/methods", "HTTP/1.0"),
	Status = element(2, Response),
	Hdrs = element(3, Response),
	MethodHdr = ghtp_utils:header_value("x-http-method", Hdrs),
	Data = gen_tcp:recv(S, 0),
	?assertEqual({202, "oK"}, Status),
	?assertEqual(Method, MethodHdr),
	?assertEqual({error, closed}, Data),
	gen_tcp:close(S).

expect_continue_test_() ->
	{setup, fun listen/0, fun stop/1, {with, [
				fun expect_continue/1
			]}}.

expect_continue({_, Port}) ->
	{ok, S} = http_client:connect(Port, gen_tcp, []),
	not_continue(S),
	continue(S),
	gen_tcp:close(S).

not_continue(S) ->
	Hdrs = [{"Content-Length", "65536"}, {"Expect", "100-continue"}],
	{ok, Response} = send_receive(S, "PUT", "/continue", Hdrs, "HTTP/1.1"),
	StatusCode = element(1, element(2, Response)),
	?assertEqual(417, StatusCode).
	
continue(S) ->
	Body = lists:map(fun erlang:integer_to_list/1, lists:seq(1, 200)),
	Size = iolist_size(Body),
	Hdrs = [
		{"Content-Length", integer_to_list(Size)},
		{"Expect", "100-continue"}
	],
	{ok, Response1} = send_receive(S, "PUT", "/continue", Hdrs, "HTTP/1.1"),
	StatusCode1 = element(1, element(2, Response1)),
	?assertEqual(100, StatusCode1),
	gen_tcp:send(S, Body),
	{ok, Response2} = http_client:receive_response(S),
	StatusCode2 = element(1, element(2, Response2)),
	RecvBody = element(4, Response2),
	?assertEqual(200, StatusCode2),
	?assertEqual(iolist_to_binary(Body), RecvBody).

upload_test_() ->
	{setup, fun listen/0, fun stop/1, {with, [
				fun upload/1,
				fun upload_chunked/1
			]}}.

upload({_, _Port}) ->
	ok.

upload_chunked({_, _Port}) ->
	ok.

%%% test help functions
listen() ->
	{ok, Pid} = gen_httpd:start_link(?MODULE, nil, 0, 300000, []),
	{Pid, gen_httpd:port(Pid)}.

stop({Pid, _}) ->
	gen_httpd:stop(Pid).

send_receive(Socket, Method, URI, Vsn) ->
	send_receive(Socket, Method, URI, [], Vsn).

send_receive(Socket, Method, URI, Hdrs, Vsn) ->
	send_receive(Socket, Method, URI, Hdrs, Vsn, []).

send_receive(Socket, Method, URI, Hdrs, Vsn, Body) ->
	Request = http_client:format_request(Method, URI, Vsn, Hdrs, Body),
	gen_tcp:send(Socket, Request),
	http_client:receive_response(Socket).

%%% gen_httpd callbacks
init(_, _) ->
	{ok, nil}.

handle_continue("PUT", "/continue", {1,1}, ReqHdrs, State) ->
	CL = ghtp_utils:header_value("content-length", ReqHdrs, "0"),
	ContentLength = list_to_integer(CL),
	if
		ContentLength > 65535 -> 
			{reply, {417, "oK"}, [], <<>>, State};
		ContentLength < 65536 ->
			{continue, [], State}
	end.

handle_request("PUT", "/continue", {1,1}, _, {identity, Reader}, State) ->
	{ok, Body} = Reader(10000),
	{reply, 200, [], Body, second};

handle_request(Method, "/methods", {1,0}, _, _, State) ->
	{reply, {202, "oK"}, [{"X-HTTP-Method", Method}], <<>>, State}.

terminate(_, _) ->
	ok.
