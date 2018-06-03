%% based on the upload example packaged with cowboy
%% @doc Upload handler.
-module(message_handler).
-export([init/2]).

%%to do: add new funs to check req body to send ERRONEOUS REQ like in login handler
%%to do later: abstract above into new module

init(Req0, Opts) ->

	{_Cookie_status, _User} = entry_helpers:check_session_cookie(Req0),
%% remove cookie status check in both erl and js - anyone can send message. 
%% use this format - both erl and js - for uploads. 

%	case Cookie_status of
%		has_no_session_cookie ->
%			ResponseBody = <<"Not logged in">>,
%			ResponseStatus = 400,
%			{ResponseBody, ResponseStatus};
%		has_session_cookie ->
			{SenderName, SenderEmail, MessageContent} = extract_message_contents(Req0),
			io:format("~nSender's Name ~n~p~n", [SenderName]),
			io:format("Sender's Email ~n~p~n", [SenderEmail]),
			io:format("Message ~n~p~n", [MessageContent]),
			%might need to check message contents first. for security. 
			%db_helpers:store_message(User, SenderName, SenderEmail, MessageContent),
%			ResponseBody = <<"Message received by server">>,
			%only for testing - send back received mesasge to display on client screen
			ResponseBody = general_helpers:escape_html(MessageContent),
			io:format("Message ~n~p~n", [ResponseBody]),
			ResponseStatus = 200,
			%{ResponseBody, ResponseStatus},
%	end,
	Req = cowboy_req:reply(ResponseStatus, #{
		<<"content-type">> => <<"text/html">>
		}, ResponseBody, Req0),
		{ok, Req, Opts}.


%%extract the File data, file name and adjectives from incoming req
extract_message_contents(Req) ->
	{ok, _Headers, Req2} = cowboy_req:read_part(Req),
	
	%Data0 is the file data
	{ok, Data0, Req3} = cowboy_req:read_part_body(Req2),
	%io:format("DATA0 ~n ~p~n", [Data0]),

	%FileSize = byte_size(Data0),
	%{file, <<"inputfile">>, FileName, _} = cow_multipart:form_data(Headers),

	{ok, _Headers1, Req4} = cowboy_req:read_part(Req3),
	{ok, Data1, Req5} = cowboy_req:read_part_body(Req4),

	{ok, _Headers2, Req6} = cowboy_req:read_part(Req5),
	{ok, Data2, _Req7} = cowboy_req:read_part_body(Req6),

%	{ok, Headers3, Req8} = cowboy_req:read_part(Req7),
%	{ok, Data3, Req9} = cowboy_req:read_part_body(Req8),

	{Data0, Data1, Data2}.

