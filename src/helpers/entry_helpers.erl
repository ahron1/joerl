-module(entry_helpers).
-export([check_session_cookie/1, check_has_body/1, extract_login_pw/1, extract_signup_details/1 ]).

%%check if request has valid cookie and return tuple with status and userid if any
check_session_cookie(OriginalRequest) ->
	#{session := SessionCookie} = cowboy_req:match_cookies([{session, [], <<>>}], OriginalRequest),
	case SessionCookie of
		<<>> -> 
			{has_no_session_cookie, no_such_user};
		_ -> 
			%db_helpers:check_session_cookie(SessionCookie)
			db_helpers:id_given_cookie(SessionCookie)
	end.

%%check if request has a body and respond with atom
check_has_body(OriginalRequest) ->
	case cowboy_req:has_body(OriginalRequest) of
		true -> has_body;
		false -> has_no_body
	end.

%%extract login/pw from req and return as tuple
extract_login_pw(OriginalRequest) ->
	{ok, _Headers1, Req2} = cowboy_req:read_part(OriginalRequest),
	{ok, FormLogin, Req3} = cowboy_req:read_part_body(Req2),

	{ok, _Headers2, Req4} = cowboy_req:read_part(Req3),
	{ok, FormPassword, _Req5} = cowboy_req:read_part_body(Req4),
	{FormLogin, FormPassword}.

%% extract user details from signup form
extract_signup_details(OriginalRequest) ->
	{ok, _Headers1, Req2} = cowboy_req:read_part(OriginalRequest),
	{ok, FormLogin, Req3} = cowboy_req:read_part_body(Req2),

	{ok, _Headers2, Req4} = cowboy_req:read_part(Req3),
	{ok, FirstName, _Req5} = cowboy_req:read_part_body(Req4),

%	{ok, _Headers3, Req6} = cowboy_req:read_part(Req5),
%	{ok, InviterEmail, _Req7} = cowboy_req:read_part_body(Req6),

%	{ok, _Headers4, Req8} = cowboy_req:read_part(Req7),
%	{ok, BirthYear, _Req9} = cowboy_req:read_part_body(Req8),

	SignupDetails = {FormLogin, FirstName},
	erlang:display(SignupDetails),
	SignupDetails.

%%get Id given handle
%extract_id_from_handle()

