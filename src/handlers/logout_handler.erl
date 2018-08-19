-module(logout_handler).
-export([init/2]).

init(Req0, Opts) ->
	{SessionCookieStatus, UserId} = entry_helpers:check_session_cookie(Req0),
	{Body, Status, Req_r} = set_reply_values(Req0, SessionCookieStatus, UserId),
	Req = cowboy_req:reply(Status, #{
		<<"content-type">> => <<"text/html">>
	}, Body, Req_r),
	{ok, Req, Opts}.

%%generate response/reply values based on cookie status and body 
set_reply_values(OriginalRequest, has_no_session_cookie, _UserId) ->
	%No cookie but has body = login req. 
	erlang:display(not_signed_in),
	{<<"NOT SIGNED IN">>, 200, OriginalRequest};
set_reply_values(OriginalRequest, has_session_cookie, UserId) ->
	erlang:display(ongoing_session_will_sign_out),
	%Has cookie with value matching key 'session' = ongoing session 
	{1, [{CookieValue}]} = db_helpers:cookie_given_id(UserId),
	ok = db_helpers:log_signout(CookieValue),
	{{delete, 1}, _} = db_helpers:delete_session_cookie(UserId),
	{<<"See you later!">>, 200, OriginalRequest};
set_reply_values(OriginalRequest, _, _UserId) ->
	erlang:display(erroneous_req),
	% No cookie and no body = error
	{<<"ERRONEOUS REQUEST">>, 400, OriginalRequest}.

