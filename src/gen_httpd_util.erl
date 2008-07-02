-module(gen_httpd_util).

-export([parse_query/1]).

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
