-module(image_helpers).
-export([extract/1]).

extract(List) ->
	Acc0 = [{<<"picid">>, []}, {<<"adj1">>, []}, {<<"adj1_id">>, []}, {<<"adj2">>, []}, {<<"adj2_id">>, []}, {<<"uri">>, []}],
	lists:foldl(func(), Acc0, List).

%anon fun for use in foldl accumulator. get the data bits (e.g. adj1, picid) from the db output and append them to their respective sublists. 
func() ->
	fun(Item, List) -> 
		%{PicIdInt, {citext, Adj1}, Adj1Id, {citext, Adj2}, Adj2Id, Uri} = Item,
		{PicIdInt, Adj1, Adj1Id, Adj2, Adj2Id, Uri} = Item,
		PicId = erlang:list_to_binary(erlang:integer_to_list(PicIdInt)),

		[{<<"picid">>, ListPicId}, {<<"adj1">>, ListAdj1}, {<<"adj1_id">>, ListAdj1Id}, {<<"adj2">>, ListAdj2}, {<<"adj2_id">>, ListAdj2Id}, {<<"uri">>, ListUri}] = List,
		[{<<"picid">>, ListPicId ++ [PicId]}, {<<"adj1">>, ListAdj1 ++ [Adj1]}, {<<"adj1_id">>, ListAdj1Id ++ [Adj1Id]}, {<<"adj2">>, ListAdj2 ++ [Adj2]}, {<<"adj2_id">>, ListAdj2Id ++ [Adj2Id]}, {<<"uri">>, ListUri ++ [Uri]}]
	end.

%internal function. pad the db output to make a proper list of tuples. 
%make_tuple_list({PicIdInt, {citext, Adj1}, {citext, Adj2}, Uri}) ->
%	PicId = erlang:list_to_binary(erlang:integer_to_list(PicIdInt)),
%	TupleList = [{<<"picid">>, PicId}, {<<"adj1">>, Adj1}, {<<"adj2">>, Adj2}, {<<"uri">>, Uri}],
%	TupleList.


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
