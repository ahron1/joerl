%% @doc Image handler.
-module(new_images_handler).
-export([init/2]).

init(Req0, Opts) ->
	{CookieStatus, User} = entry_helpers:check_session_cookie(Req0),
	{ResponseBody, ResponseStatus} = next_images(CookieStatus, User),

	Req = cowboy_req:reply(ResponseStatus, #{
		%<<"content-type">> => <<"text/html">>
		<<"content-type">> => <<"application/json">>
		}, ResponseBody, Req0),
		{ok, Req, Opts}.

%% check logged in status and send list of images, adjs
next_images(has_session_cookie, User) ->
	NextImagesInfo = db_helpers:get_new_pics(User),
	NextImagesAdjsList = image_helpers:extract(NextImagesInfo),
	RBody = jsx:encode(NextImagesAdjsList),
	%io:format("Body ~n~p~n", [RBody]),
	RStatus = 200,
	{RBody, RStatus};

next_images(_, _) ->
	RBody = <<"Not logged in">>,
	RStatus = 400,
	{RBody, RStatus}.
