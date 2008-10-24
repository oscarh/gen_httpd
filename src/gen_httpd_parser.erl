-module(gen_httpd_parser).

-export([parse/1]).

parse(Request) ->
	{Method, URL, Vsn, Rest0} = parse_request(Request),
	{Headers, Rest1} = parse_headers(Rest0, []),
	{{Method, URL, Vsn, Headers}, Rest1}.

parse_request("\r\n" ++ Rest) ->
	parse_request(Rest);
parse_request(Req) ->
	{Meth, Rest0} = parse_method(Req, []),
	{Path, Rest1} = parse_path(Rest0, []),
	{VSN, Rest2} = parse_vsn(Rest1, []),
	{Meth, Path, VSN, Rest2}.

parse_headers("\r\n" ++ Rest, Headers) ->
	{Headers, Rest};
parse_headers(Req, Headers) ->
	{Key, Rest0} = parse_header_key(Req, []),
	{Value, Rest1} = parse_header_value(Rest0, []),
	parse_headers(Rest1, [{Key, Value}|Headers]).

parse_method(" " ++ T, Acc) ->
	{lists:reverse(Acc), string:strip(T)};
parse_method([H|T], Acc) ->
	parse_method(T, [H|Acc]);
parse_method([], _) ->
	exit(incomplete).

parse_path(" " ++ T, Acc) ->
	{lists:reverse(Acc), string:strip(T)};
parse_path([H|T], Acc) ->
	parse_path(T, [H|Acc]);
parse_path([], _) ->
	exit(incomplete).

parse_vsn("\r\n" ++ T, Acc) ->
	{lists:reverse(Acc), string:strip(T)};
parse_vsn([H|T], Acc) ->
	parse_vsn(T, [H|Acc]);
parse_vsn([], _) ->
	exit(incomplete).

parse_header_key(":" ++ T, Acc) ->
	{lists:reverse(Acc), string:strip(T)};
parse_header_key([H|T], Acc) ->
	parse_header_key(T, [H|Acc]);
parse_header_key([], _) ->
	exit(incomplete).

parse_header_value("\r\n" ++ T, Acc) ->
	{lists:reverse(Acc), string:strip(T)};
parse_header_value([H|T], Acc) ->
	parse_header_value(T, [H|Acc]);
parse_header_value([], _) ->
	exit(incomplete).

