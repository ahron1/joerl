-module(image_helpers).
-export([tuple_list_to_json/1]).

pic_adj_set_to_json({PicId, {citext, Adj1}, {citext, Adj2}, Uri}) ->
	 TupleOfTuples = {{<<"picid">>, PicId}, {<<"adj1">>, Adj1}, {<<"adj2">>, Adj2}, {<<"uri">>, Uri}},
	 ListOfTuples = erlang:tuple_to_list(TupleOfTuples),
	 jsx:encode(ListOfTuples).

tuple_list_to_json(L) ->
	lists:filtermap(fun(X) -> {true, pic_adj_set_to_json(X)} end, L).

