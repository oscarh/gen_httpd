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
%%% @private
%%% @doc
%%% 
%%% @end
%%% ----------------------------------------------------------------------------
-module(gen_httpd_pipeline_queue).

-export([new/1, next_id/1, push/2, response/3, is_full/1, id/2]).

-define(NEXT(N, TUPLE), if N =:= size(TUPLE) -> 1; true -> N + 1 end).

new(Length) when Length > 1 ->
	{1, 1, erlang:make_tuple(Length, nil)}.

next_id({_, Tail, _}) ->
	Tail.

push(Ref, {Head, Tail, Elements}) ->
	NextTail = ?NEXT(Tail, Elements),
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
