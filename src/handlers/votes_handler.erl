%% @doc Image handler.
-module(votes_handler).
-export([init/2]).

init(Req0, Opts) ->
	{CookieStatus, User} = entry_helpers:check_session_cookie(Req0),
	{ok, _Headers1, Req2} = cowboy_req:read_part(Req0),
	{ok, VotesData, _Req3} = cowboy_req:read_part_body(Req2),

	{ResponseBody, ResponseStatus} = votes_processor(CookieStatus, User, VotesData),

	Req = cowboy_req:reply(ResponseStatus, #{
		%<<"content-type">> => <<"text/html">>
		<<"content-type">> => <<"application/json">>
		}, ResponseBody, Req0),
		{ok, Req, Opts}.

%% check logged in status and send list of images, adjs
votes_processor(has_session_cookie, User, VotesData) ->
	[{<<"images">>, ImageList}, {<<"adj1">>, Adj1List}, {<<"adj2">>, Adj2List}, {<<"choice">>, ChoiceList}] = jsx:decode(VotesData),

	% case ___ of votes_record 
%	db_helpers:record_votes(Adj1, Adj2, Choice, Image, UserId),

	VotesRecord = general_helpers:zip4(ImageList, Adj1List, Adj2List, ChoiceList),
	%erlang:display(VotesRecord),
	record_votes(User, VotesRecord),

	RBody = <<"votes received ok">>,
	RStatus = 200,
	{RBody, RStatus};
votes_processor(has_no_session_cookie, _, _) ->
	RBody = <<"Not logged in">>,
	RStatus = 400,
	{RBody, RStatus};
votes_processor(_, _, _) ->
	RBody = <<"Unable to process votes">>,
	RStatus = 500,
	{RBody, RStatus}.


record_votes(User, VotesRecord) ->
	lists:foreach(
			fun({ImageId, Adj1, Adj2, Vote}) ->
				%{ImageId, Adj1, Adj2, Vote} = VotesRecord,
				Choice = get_choice(Vote),
%				erlang:display(this_goes_in_db________check),
%				erlang:display(erlang:binary_to_integer(Adj1)),
%				erlang:display(erlang:binary_to_integer(Adj2)),
%				erlang:display(Choice),
%				erlang:display(erlang:binary_to_integer(ImageId)),
%				erlang:display(User),
%				erlang:display(this_goes_in_db________check),
				db_helpers:record_votes(Adj1, Adj2, Choice, erlang:binary_to_integer(ImageId), User)
%				erlang:display(this_went_in_db________check)
			end
		  , VotesRecord).

%func() ->
%	fun({ImageId, Adj1, Adj2, Vote}) ->
%			Choice = get_choice(Vote),
%			db_helpers:record_votes(Adj1, Adj2, Choice, Image, User)
%	end.

get_choice(<<"button1">>) ->
	1;
get_choice(<<"button2">>) ->
	2;
get_choice(<<"button3">>) ->
	0.




