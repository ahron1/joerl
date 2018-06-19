-module(general_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
	Req = cowboy_req:reply(404, 
						   #{<<"content-type">> => <<"text/plain">>},
						  <<"This page doesn't seem to exist. Please try again \n">>,
						  Req0),
	{ok, Req, State}.
