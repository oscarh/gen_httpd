-module(gen_httpd_pipeline_queue).

-export([new/1, next_id/1, push/2, response/3, is_full/1, id/2]).

-define(NEXT(N, TUPLE), if N =:= size(TUPLE) -> 1; true -> N + 1 end).

new(Length) when Length > 1 ->
	{1, 1, erlang:make_tuple(Length, nil)}.

next_id({_, Tail, _}) ->
	Tail.

push(Ref, {Head, Tail, Elements}) ->
	NextTail = if Tail =:= size(Elements) -> 1; true -> Tail + 1 end,
	{Head, NextTail, setelement(Tail, Elements, Ref)}.

response(Head, Response, {Head, Tail, Elements0}) ->
	NextHead = ?NEXT(Head, Elements0),
	Elements1 = setelement(Head, Elements0, nil),
	{Responses, Queue} = responses({NextHead, Tail, Elements1}, []),
	{[Response | Responses], Queue};
response(N, Response, {Head, Tail, Elements}) ->
	{[], {Head, Tail, setelement(N, Elements, {response, Response})}}.

id(Ref, {_, _, Elements}) ->
	id(Ref, Elements, lists:seq(1, size(Elements))).

is_full({N, N, Elements}) ->
	element(N, Elements) =/= nil;
is_full(_) ->
	false.

responses({N, Tail, Elements0}, Acc) ->
	case element(N, Elements0) of
		{response, R} ->
			Elements1 = setelement(N, Elements0, nil),
			responses({?NEXT(N, Elements1), Tail, Elements1}, [R | Acc]);
		_ ->
			{lists:reverse(Acc), {N, Tail, Elements0}}
	end.

id(Ref, Elements, [N | T]) ->
	case element(N, Elements) of
		Ref -> N;
		_   -> id(Ref, Elements, T)
	end.
