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
	%erlang:display(login_req),
	check_in(OriginalRequest);
set_reply_values(OriginalRequest, has_session_cookie, _) ->
	%erlang:display(ongoing_session),
	%Has cookie with value matching key 'session' = ongoing session 
	{<<"ONGOING SESSION">>, 200, OriginalRequest};
set_reply_values(OriginalRequest, _, _) ->
	%erlang:display(erroneous_req),
	% No cookie and no body = error
	{<<"ERRONEOUS REQUEST">>, 400, OriginalRequest}.

%%check login details, return http status, and set a cookie if okay. 
check_in(OriginalRequest) ->
	{FormLogin, FormPassword} = entry_helpers:extract_login_pw(OriginalRequest),
	%get user id from db and if the given login/pw are a match
	{{select,N}, IdTupleList} = db_helpers:id_given_login_pw(FormLogin, FormPassword),
	erlang:display(IdTupleList),
	% case on number of rows returned by the query
	case N of
		1 -> 
			[{Id}] = IdTupleList,
			set_cookie(Id, OriginalRequest);
		_ -> 
			{<<"Erroneous login and/or password. Please check.">>, 400, OriginalRequest}
	end.

%%set cookie with key 'session' 
set_cookie(Id, OriginalRequest) ->
	% todo move password check to previous module.?
	%%todo: hash/salt pw in db and check user pw directly in db, not in server code. or hash form pw and compare with stored hash. 
		NewCookieValue = db_helpers:create_session_cookie(Id),
		%after cookie has gotten a value (new/old) procees with Req
		Req1 = cowboy_req:set_resp_cookie(<<"session">>, NewCookieValue, OriginalRequest, #{path => <<"/">>, http_only => true, secure => true}),
		ok = db_helpers:log_signin(Id, NewCookieValue),
		{<<"WELCOME">>, 200, Req1}.

