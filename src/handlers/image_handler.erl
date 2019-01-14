%% @doc Image handler.
%% send new image(s) to client

-module(image_handler).
-export([init/2]).

init(Req0, Opts) ->
	{CookieStatus, User} = entry_helpers:check_session_cookie(Req0),

	{ok, _Headers1, Req2} = cowboy_req:read_part(Req0),
	{ok, WhichImage, _Req3} = cowboy_req:read_part_body(Req2),

	erlang:display(whichImage),
	erlang:display(WhichImage),
	{ResponseBody, ResponseStatus} = send_image(WhichImage, CookieStatus, User),

	Req = cowboy_req:reply(ResponseStatus, #{
		%<<"content-type">> => <<"text/html">>
		<<"content-type">> => <<"application/json">>
		}, ResponseBody, Req0),
		{ok, Req, Opts}.

send_image(WhichImage, has_session_cookie, User) when ((WhichImage == <<>>) or (WhichImage == <<"/">>)) ->
	NextImagesInfo = db_helpers:get_new_pics(User),
	NextImagesAdjsList = image_helpers:extract(NextImagesInfo),
	RBody = jsx:encode(NextImagesAdjsList),
	%erlang:display(RBody),
	RStatus = 200,
	{RBody, RStatus};
send_image(WhichImage, has_session_cookie, User) ->
	[<<>>, ThisImage] = binary:split(WhichImage, [<<"/images/">>]),
	ThisImageId = erlang:list_to_integer(erlang:binary_to_list(ThisImage)),
	%erlang:display(ThisImageId),
	ImageInfo = db_helpers:get_this_pic(ThisImageId),
	NextImagesInfo = db_helpers:get_new_pics(User),
	ImagesToSendInfo = lists:append(ImageInfo, NextImagesInfo),
	NextImagesAdjsList = image_helpers:extract(ImagesToSendInfo),
	RBody = jsx:encode(NextImagesAdjsList),
	%erlang:display(RBody),
	RStatus = 200,
	{RBody, RStatus};
send_image(_, _, _) ->
	%erlang:display(cookie_not_ok_in_new_img_handler),
	RBody = <<"Not logged in">>,
	RStatus = 400,
	{RBody, RStatus}.

