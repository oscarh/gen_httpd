-module(http_methods).
-behaviour(gen_httpd).

-export([run_test/0, run_pipeline_test/0]).
-export([init/1, handle_continue/6, handle_request/7, terminate/2]).

run_test() ->
	application:start(httpd_test),
	httpd_test_sup:start_server(httpd_methods, ?MODULE, [], 9876, 0),
	timer:sleep(2),
	get_empty(),
	post_empty(),
	get_content(),
	post_content(),
	post_continue(),
	post_cancel(),
	http_client:close_connections(),
	application:stop(httpd_test).

run_pipeline_test() ->
	application:start(httpd_test),
	httpd_test_sup:start_server(httpd_methods, ?MODULE, [], 9876, 10),
	timer:sleep(2),
	get_empty(),
	post_empty(),
	get_content(),
	post_content(),
	post_continue(),
	post_cancel(),
	http_client:close_connections(),
	application:stop(httpd_test).

get_empty() ->
	URL = "http://localhost:9876/empty",
	Socket = http_client:send_request(URL, "GET", [], []),
	{ok, Response} = http_client:receive_response(Socket),
	{{1,1}, {200, "OK"}, ResponseHeaders, <<>>} = Response,
	"get /empty" = gen_httpd_util:header_value("x-test", ResponseHeaders).

post_empty() ->
	URL = "http://localhost:9876/empty",
	Socket = http_client:send_request(URL, "POST", [], "POST"),
	{ok, Response} = http_client:receive_response(Socket),
	{{1,1}, {200, "OK"}, ResponseHeaders, <<>>} = Response,
	"post /empty" = gen_httpd_util:header_value("x-test", ResponseHeaders).

get_content() ->
	URL = "http://localhost:9876/content",
	Socket = http_client:send_request(URL, "GET", [], []),
	{ok, Response} = http_client:receive_response(Socket),
	ResponseBody = <<"GET content">>,
	{{1,1}, {200, "OK"}, ResponseHeaders, ResponseBody} = Response,
	"get /content" = gen_httpd_util:header_value("x-test", ResponseHeaders).

post_content() ->
	URL = "http://localhost:9876/content",
	Socket = http_client:send_request(URL, "POST", [], "POST"),
	{ok, Response} = http_client:receive_response(Socket),
	ResponseBody = <<"content: POST">>,
	{{1,1}, {200, "OK"}, ResponseHeaders, ResponseBody} = Response,
	"post /content" = gen_httpd_util:header_value("x-test", ResponseHeaders).

post_continue() ->
	URL = "http://localhost:9876/continue",
	Headers = [{"expect", "100-continue"}, {"content-length", "9"}],
	Socket = http_client:send_request(URL, "POST", Headers, []),
	{ok, {{1,1}, {100, "Continue"}, _, _}} =
		http_client:receive_response(Socket),
	gen_tcp:send(Socket, <<"CONTINUE?">>),
	ResponseBody = <<"content: CONTINUE?">>,
	{ok, Response} = http_client:receive_response(Socket),
	{{1,1}, {200, "OK"}, ResponseHeaders , ResponseBody} = Response,
	"post /continue" = gen_httpd_util:header_value("x-test", ResponseHeaders).

post_cancel() ->
	URL = "http://localhost:9876/cancel",
	Headers = [{"expect", "100-continue"}],
	Socket = http_client:send_request(URL, "POST", Headers, []),
	{ok, Response} = http_client:receive_response(Socket),
	ResponseStatus = {417, "Expectation Failed"},
	{{1,1}, ResponseStatus, ResponseHeaders, <<"test cancel">>} = Response,
	"post /cancel" = gen_httpd_util:header_value("x-test", ResponseHeaders).

init(_) ->
	{ok, no_state}.

handle_continue(_, {abs_path, "/cancel"}, _, _, _, State) ->
	{reply, 417, [{"X-test", "POST /cancel"}], <<"test cancel">>, State};
handle_continue(_, {abs_path, "/continue"}, _, _, _, State) ->
	{continue, State}.

handle_request('GET', {abs_path, "/empty"}, _, _, _, _, State) ->
	{reply, 200, [{"X-test", "GET /empty"}], <<>>, State};
handle_request('GET', {abs_path, "/content"}, _, _, _, _, State) ->
	{reply, 200, [{"X-test", "GET /content"}], <<"GET content">>, State};
handle_request('POST', {abs_path, "/empty"}, _, _, <<"POST">>, _, State) ->
	{reply, 200, [{"X-test", "POST /empty"}], [], State};
handle_request('POST', {abs_path, "/content"}, _, _, Body, _, State) ->
	ResponseBody = [<<"content">>, ": ", Body],
	{reply, 200, [{"X-test", "POST /content"}], ResponseBody, State};
handle_request('POST', {abs_path, "/continue"}, _, _, Body, _, State) ->
	ResponseBody = [<<"content">>, ": ", Body],
	{reply, 200, [{"X-test", "POST /continue"}], ResponseBody, State}.

terminate(_, _) ->
	ok.
