-module(login_handler).
-export([init/2]).

init(Req0, Opts) ->
	{SessionCookieStatus, _UserId} = entry_helpers:check_session_cookie(Req0),
	ReqBodyStatus = entry_helpers:check_has_body(Req0),
	{Body, Status, Req_r} = set_reply_values(Req0, SessionCookieStatus, ReqBodyStatus),
	Req = cowboy_req:reply(Status, #{
		<<"content-type">> => <<"text/html">>
	}, Body, Req_r),
	{ok, Req, Opts}.

%% to do : check if user trying to sign in has signed up but isn't yet activated. HasSignedUp from join_handler. 

%%generate response/reply values based on cookie status and body 
set_reply_values(OriginalRequest, has_no_session_cookie, has_body) ->
	%No cookie but has body = login req. 
	erlang:display(login_req),
	check_in(OriginalRequest);
set_reply_values(OriginalRequest, has_session_cookie, _) ->
	erlang:display(ongoing_session),
	%Has cookie with value matching key 'session' = ongoing session 
	{<<"ONGOING SESSION">>, 200, OriginalRequest};
set_reply_values(OriginalRequest, _, _) ->
	erlang:display(erroneous_req),
	% No cookie and no body = error
	{<<"ERRONEOUS REQUEST">>, 400, OriginalRequest}.

%%check login details, return http status, and set a cookie if okay. 
check_in(OriginalRequest) ->
	{FormLogin, FormPassword} = entry_helpers:extract_login_pw(OriginalRequest),
%%	receiving json data and extracting info from it
%	{ok, Data, _} = cowboy_req:read_body(OriginalRequest),
%	erlang:display(data),
%	io:format("~p~n", [Data]),
%	%extract login and pw via pattern matching from decoded post data in req 
%	{struct, [{<<"login">>, FormLogin},{<<"password">>,FormPassword}]} = mochijson2:decode(Data),

	%get stored password from db and number of rows matching the given login
	{{select,N}, IdPwTupleList} = db_helpers:id_pw_given_login(FormLogin),
	erlang:display(IdPwTupleList),
	% case on number of rows returned by the query
	% todo bring password check to this module.
	case N of
		1 -> 
			[{Id, StoredPassword}] = IdPwTupleList,
			set_cookie(StoredPassword, Id, FormPassword, OriginalRequest);
		_ -> 
			{<<"USERNAMES ERROR">>, 400, OriginalRequest}
	end.

%%set cookie with key 'session' if password matches
set_cookie(StoredPassword, Id, FormPassword, OriginalRequest) ->
	% todo move password check to previous module.?
	%%todo: hash/salt pw in db and check user pw directly in db, not in server code. or hash form pw and compare with stored hash. 
	case StoredPassword =:= FormPassword of
		true ->
			% first check if db already has a valid cookie 
			% for the case where user closed browser window and lost valid cookie
			{N, ExistingCookieTupleList} = db_helpers:cookie_given_id(Id),
			NewCookieValue = case N of 
				1 -> 
					[{ExistingCookie}] = ExistingCookieTupleList,
					ExistingCookie;
				_ -> 
					% if valid cookie doesn't exist, make new
					db_helpers:create_session_cookie(Id)
			end,
			%after cookie has gotten a value (new/old) procees with Req
			Req1 = cowboy_req:set_resp_cookie(<<"session">>, NewCookieValue, OriginalRequest, #{path => <<"/">>, http_only => true, secure => true}),
			{<<"WELCOME">>, 200, Req1};
		false ->
			{<<"wrong password">>, 400, OriginalRequest}
	end.

