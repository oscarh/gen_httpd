-module(gen_httpd_util).

-export([
		parse_query/1,
		uri_encode/1,
		uri_decode/1
	]).

-define(URI_ENCODE_ESCAPE,
	[
		% Reserved (RFC 2396: 2.2:
		$;, $/, $?, $:, $@, $&, $=, $+, $$, $,,
		% Excluded (RFC 2396: 2.4.3)
		0,1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
		20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 16#7f,
		$ , $<, $>, $#, $%, $", ${, $}, $|, $\\, $^, $[, $], $`
	]).

parse_query(QueryStr) ->
	case string:tokens(QueryStr, ";") of
		[] ->
			[];
		Args ->
			lists:map(fun(Arg) ->
						[Key, Value] = lists:tokens(Arg, "="),
						{Key, Value}
				end, Args)
	end.

uri_decode(Str) ->
	uri_decode(Str, []).

uri_decode([$%, A, B | Rest], Acc) ->
	uri_decode(Rest, [erlang:list_to_integer([A, B], 16) | Acc]);
uri_decode([H | Rest], Acc) ->
	uri_decode(Rest, [H | Acc]);
uri_decode([], Acc) ->
	lists:reverse(Acc).

uri_encode(Str) ->
	uri_encode(Str, []).

uri_encode([H | T], Acc) ->
	uri_encode(T, case lists:member(H, ?URI_ENCODE_ESCAPE) of
		true ->
			[A, B] = char_to_hex(H),
			[B, A, $% | Acc];
		false ->
			[H | Acc]
	end);
uri_encode([], Acc) ->
	lists:reverse(Acc).

char_to_hex(Char) ->
	string:right(erlang:integer_to_list(Char, 16), 2, $0).
