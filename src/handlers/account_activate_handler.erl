-module(account_activate_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
	ActivationToken = cowboy_req:binding(activation_token, Req0),
	io:format("~p~n", [ActivationToken]),
	Req = cowboy_req:reply(200, 
						   #{<<"content-type">> => <<"text/plain">>},
						  <<"Account Activation \n">>,
						  Req0),
	{ok, Req, State}.
