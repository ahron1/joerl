-module(image_helpers).
-export([extract/1]).

extract(List) ->
	Acc0 = [[picid, []], [adj1, []], [adj2, []], [uri, []]],
	lists:foldl(func(), Acc0, List).

%anon fun for use in foldl accumulator
func() ->
	fun(Item, List) -> 
			[{picid, PicId}, {adj1, Adj1}, {adj2, Adj2}, {uri, Uri}] = make_tuple_list(Item),
			[[picid, ListPicId], [adj1, ListAdj1], [adj2, ListAdj2], [uri, ListUri]] = List,
			[[picid, ListPicId ++ [PicId]], [adj1, ListAdj1 ++ [Adj1]], [adj2, ListAdj2 ++ [Adj2]], [uri, ListUri ++ [Uri]]]
	end.

%internal function. pad the db output to make a proper list of tuples. 
make_tuple_list({PicId, {citext, Adj1}, {citext, Adj2}, Uri}) ->
	 TupleList = [{picid, PicId}, {adj1, Adj1}, {adj2, Adj2}, {uri, Uri}],
	 TupleList.


%old funs
%tuple_list_to_json(L) ->
%	lists:filtermap(fun(X) -> {true, pic_adj_set_to_json(X)} end, L).
%
%%internal function. pad the db output to make a proper tuple of tuples. 
%pic_adj_set_to_json({PicId, {citext, Adj1}, {citext, Adj2}, Uri}) ->
%	 ListOfTuples = [{<<"picid">>, PicId}, {<<"adj1">>, Adj1}, {<<"adj2">>, Adj2}, {<<"uri">>, Uri}],
%	 %ListOfTuples = erlang:tuple_to_list(TupleOfTuples),
%	 jsx:encode(ListOfTuples).
%
%internal function. pad the db output to make a proper list of tuples. 
%make_tuple_list({PicId, {citext, Adj1}, {citext, Adj2}, Uri}) ->
%	 ListOfTuples = [{picid, PicId}, {adj1, Adj1}, {adj2, Adj2}, {uri, Uri}],
%	 ListOfTuples.
%	
%
%func() ->
%	fun(X, L) -> 
%			{_, Y} = X,
%			L ++ [Y] end.
%

%original test functions
%extract(List) ->
%	%lists:foldl(fun(X, L) -> L ++ [X] end, [], List).
%	lists:foldl(func(), [], List).


%original test functions
%extract(List) ->
%	%lists:foldl(fun(X, L) -> L ++ [X] end, [], List).
%	%lists:foldl(func(), [{a, []}, {b, []}], List).

%func() -> fun(X,L) -> 
%{{a, Value1}, {b, Value2}} = X,
%[{a, List_a}, {b, List_b}] = L,
%[{a, List_a ++ [Value1]}, {b, List_b ++ [Value2]}]
%end.

%TERMINAL TEST
%mylistapp:extract(Y).
%[{a,[2,4,6]},{b,[3,5,7]}]
%54> Y.
%[{a,2},{b,3}},{a,4},{b,5}},{a,6},{b,7}}]}}}]]}]}])
%
