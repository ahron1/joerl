-module(jo_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([reload_routes/0]).
-on_reload(reload_routes/0).

-include_lib("deps/erl_img/include/erl_img.hrl").

start(_Type, _Args) ->
	Dispatch = make_dispatch(),
%	PrivDir = code:priv_dir(jo),
%	{ok, _} = cowboy:start_tls(https, [
%	{port, 8443},
%	{cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
%	{certfile, PrivDir ++ "/ssl/server.crt"},
%	{keyfile, PrivDir ++ "/ssl/server.key"}
%	], #{env => #{dispatch => Dispatch}}),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	jo_sup:start_link().

make_dispatch() ->
	cowboy_router:compile([
					   {'_', [{"/login", login_handler, []} 
							 ,{"/logout", logout_handler, []}
							 ,{"/uploadhandler", upload_handler, []}
							 ,{"/uploadhandlerspecial", upload_handler_special, []}
							 ,{"/messagehandler", message_handler, []}
							 ,{"/imagehandler", image_handler, []}
							 ,{"/voteshandler", votes_handler, []}
							 ,{"/join/[:join_token]", join_handler, []}
							 %,{"/resetpassword/[:pw_token]", pw_reset_handler, []}
							 ,{"/password/[:pw_token]", pw_handler, []}
							 ,{'_', general_handler, []}
							 ]}
						 ]).

reload_routes() ->
	cowboy:set_env(https, dispatch, make_dispatch()).

stop(_State) ->
	ok.
