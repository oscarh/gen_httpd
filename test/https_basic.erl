-module(https_basic).

-export([
        init/2,
        handle_request/6,
        terminate/2
    ]).

-include_lib("eunit/include/eunit.hrl").

-define(HTTPS_RESPONSE, "HTTPS response string").

%%% eunit tests
https_test_() ->
	{setup, fun listen_https/0, fun stop_https/1, {with, [
				fun simple_request/1
			]}}.

%%% test help functions
listen_https() ->
	SSLOpts = [{certfile, "/tmp/key-cert.pem"}, {verify, 0}],
	{ok, Pid} = gen_httpd:start_link(?MODULE, nil, 0, 300000, [], SSLOpts),
	{Pid, gen_httpd:port(Pid)}.

stop_https({Pid, _}) ->
	gen_httpd:stop(Pid).

simple_request({_, Port}) ->
	Opts = [{verify, 0}, {active, false}, binary],
	{ok, Socket} = http_client:connect(Port, ssl, Opts),
	Request = http_client:format_request("GET", "/ssl", "HTTP/1.1", []),
	ssl:send(Socket, Request),
	ssl:getopts(Socket, [active]),
	{ok, Response} = http_client:receive_response(Socket, ssl),
	Status = element(2, Response),
	StatusCode = element(1, Status),
	Body = element(4, Response),
	?assertEqual(200, StatusCode),
	?assertEqual(<<?HTTPS_RESPONSE>>, Body),
	ssl:close(Socket).

%%% gen_httpd callbacks
init(_, nil) ->
	{ok, nil}.

handle_request(_, _, _, _, _, State) ->
	{reply, 200, [], ?HTTPS_RESPONSE, State}.

terminate(_, _) ->
    ok.
