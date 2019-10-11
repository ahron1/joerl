%%% handler to show and edit profile pages. 

-module(profile_handler).
-behavior(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
	AllBindings = cowboy_req:bindings(Req0),
	ProfileAction = cowboy_req:binding(profile_action, Req0, <<"view">>),
	Handle = cowboy_req:binding(handle, Req0, <<"test">>),
	{{select, N}, IdTupleList} = db_helpers:id_given_handle(Handle),

	{Status, Headers, Body} = case N of
		1 -> 
			[{Id}] = IdTupleList,
			case ProfileAction of
				<<"view">> ->
					%get_profile_details(Id),
					console_log(Handle, Id, AllBindings, action_view),
					erlang:display(option_view),
					{200, #{<<"content-type">> => <<"text/plain">>}, <<"profile details">>};
				<<"submit">> ->
					console_log(Handle, Id, AllBindings, action_submit),
					{200, #{<<"content-type">> => <<"text/plain">>}, <<"submit changes">>};
				_ -> 
					console_log(Handle, Id, AllBindings, this_is_not_a_valid_action),
					{400, #{<<"content-type">> => <<"text/plain">>}, <<"this is not a valid action">>}
			end;
		_ -> 
			erlang:display(this_is_not_a_valid_handle),
			{400, #{<<"content-type">> => <<"text/plain">>}, <<"Erroneous handle. Please check.">>}
	end,

	Req = cowboy_req:reply(Status, Headers, Body, Req0),
	{ok, Req, State}.

%%get profile details

%%output things to console to check/test
console_log(Handle, Id, AllBindings, ProfileAction) ->
	erlang:display(in_profile_handler),
	erlang:display(id),
	erlang:display(Id),
	erlang:display(all_bindings),
	erlang:display(AllBindings),
	erlang:display(profile_action),
	erlang:display(ProfileAction),
	erlang:display(handle),
	erlang:display(Handle).

