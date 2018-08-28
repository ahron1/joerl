%% based on the upload example packaged with cowboy
%% @doc Upload handler.
-module(message_handler).
-export([init/2]).

%%to do: add new funs to check req body to send ERRONEOUS REQ like in login handler
%%to do later: abstract above into new module

init(Req0, Opts) ->

	{_CookieStatus, _User} = entry_helpers:check_session_cookie(Req0),
%% remove cookie status check in both erl and js - anyone can send message. 

%	case CookieStatus of
%		has_no_session_cookie ->
%			ResponseBody = <<"Not logged in">>,
%			ResponseStatus = 400,
%			{ResponseBody, ResponseStatus};
%		has_session_cookie ->
			{SenderName, SenderEmail, MessageContent} = extract_message_contents(Req0),
			
%			erlang:display(xxxxxxxxxxxxxxxxxxxxx),
%			erlang:display(MessageContent),
%			erlang:display(SenderName),
%			erlang:display(SenderEmail),
%			erlang:display(xxxxxxxxxxxxxxxxxxxxx),

			MessageContentCleaned = general_helpers:escape_html(MessageContent),
			SenderNameCleaned = general_helpers:escape_html(SenderName),
			_SenderEmailCleaned = general_helpers:escape_html(SenderEmail),
			email_helper:send_website_message(binary:bin_to_list(SenderNameCleaned), binary:bin_to_list(SenderEmail), binary:bin_to_list(MessageContentCleaned)),

			ResponseBody = <<"Thank you for your message! We will read it shortly.">>,
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
%	erlang:display(xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy),
%	io:format("REQ2 ~n ~p~n", [Req2]),
%	erlang:display(xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy),
	
	%Data0 is the file data
	{ok, Data0, Req3} = cowboy_req:read_part_body(Req2),
%	erlang:display(xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy),
%	io:format("DATA0 ~n ~p~n", [Data0]),
%	io:format("Req3 ~n ~p~n", [Req3]),
%	erlang:display(xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy),

	{ok, _Headers1, Req4} = cowboy_req:read_part(Req3),
	{ok, Data1, Req5} = cowboy_req:read_part_body(Req4),
%	erlang:display(xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy),
%	io:format("DATA1 ~n ~p~n", [Data1]),
%	io:format("Req4 ~n ~p~n", [Req4]),
%	io:format("Req5 ~n ~p~n", [Req5]),
%	erlang:display(xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy),

	{ok, _Headers2, Req6} = cowboy_req:read_part(Req5),
	{ok, Data2, _Req7} = cowboy_req:read_part_body(Req6),
%	erlang:display(xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy),
%	io:format("DATA2 ~n ~p~n", [Data2]),
%	io:format("Req6 ~n ~p~n", [Req6]),
%	erlang:display(xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy),

%	{ok, Headers3, Req8} = cowboy_req:read_part(Req7),
%	{ok, Data3, Req9} = cowboy_req:read_part_body(Req8),

	{Data0, Data1, Data2}.

