-module(join_handler).
-behavior(cowboy_handler).
-export([init/2]).
%-compile([export_all]).
%% to do: update tokens to work better with nginx. from js hit ____ for new join req, /new/ for ... so nginx can pattern match for /join/[0-9a-z]+/ for the token

init(Req0, State) ->
	%JoinToken is a parameter bound to the req that either contains the token value or denotes a brand new user request with the value "new"
	JoinToken = cowboy_req:binding(join_token, Req0),
	{Status, Headers, Body} = case JoinToken of
		<<"new">> ->
			new_join_req(Req0);
		_ -> 
			process_token(JoinToken)
	end,
	Req = cowboy_req:reply(Status, Headers, Body, Req0),
	{ok, Req, State}.

%% handle new signup request
%for a brand new user request, check various conditions and process the request
new_join_req(Req0) ->
	{Login, FirstName} = entry_helpers:extract_signup_details(Req0),
	%{HasAccount, Id} = has_account(Login), 
	{{select, N}, IdTupleList} = db_helpers:id_given_login(Login),

	process_new_request(N, IdTupleList, Login, FirstName).

%process new account depending on if there already is a db entry
%for a completely fresh user, just make a new account. 
process_new_request(0, _, Login, FirstName) ->
	create_new_account(Login, FirstName);

process_new_request(1, IdTupleList, Login, FirstName) ->
	%fun to check is_account_active
	%this gets set to true after the account is activated (by clicking on token)
	
	%is in inorganic db - has been invited

	[{Id}] = IdTupleList,
	AlreadyActive = already_active(Id) % check in ppc is_acc_active
		IsInvited = is_invited(Id), % check presence in ppa_invited table
			HasSignedUp = has_signed_up(Id) % check presence in ppa_fresh table, and check waiting period isn't up. 
				IsWaitingOver = is_waiting_over(Id)
				TokenGenerated = token_generated(Id)
					IsTokenExpired = is_token_expired(Id)





	%TODO: also update first name based on signup form input.. 
.

%check join req details and call new account creation if needed.
%new_account_if_needed(HasAccount, HasSignedUp, IsPreactivatedAccount, Login, ).
new_account_if_needed(has_account, _, _, _) ->
	%given login already has active functioning account
	%todo: use new status code for js redir to homepage/reload
	{200, #{<<"content-type">> => <<"text/plain">>}, <<"there's already an account for this email. try logging in.">>};

new_account_if_needed(_, has_signed_up, _, _) ->
	erlang:display(already_on_waiting_list),
	%todo: use new status code for js redir to homepage/reload
	{200, #{<<"content-type">> => <<"text/plain">>}, <<"you're already on the waiting list. We'll activate your account ASAP">>};

new_account_if_needed(_, _, IsPreactivatedAccount, Login) ->
	create_new_account(Login, IsPreactivatedAccount ).

%%actual creation of the account depending on conditions
%create_new_account(Login, _FirstName, is_preactivated_account) ->
%	create_new_account(Login),
%	activate_new_account(login, Login)
%	use atom login/token in tuple to distinguish direct activation vs activation via emailed url with token
%	;

create_new_account(Login, FirstName) -> 
		{{select, N}, NewUserIdTupleList} = db_helpers:new_account_creation(Login),
		[{NewUserId}] = NewUserIdTupleList,
		case N of 
			1 ->
				%%todo: token will be created and sent when waiting period is over. via periodic script/cron job that checks for waiting period. for now it is done directly here. 
				{{select, N}, TokenTupleList} = db_helpers:create_signup_token(NewUserId),
				[{TokenValue}] = TokenTupleList,

				% todo ---- 
				% create email body (Token, Login, FirstName)
				% send email.
				%email_helper:
				%db_helpers:signup_token_sent_date(NewUserId),

				{200, #{<<"content-type">> => <<"text/plain">>}, <<"successfully signed up. please check email">>};

			_ ->
				{400, #{<<"content-type">> => <<"text/plain">>}, <<"there was a problem signing up. please try again">>}
		end.

%% compose and send mail with new token value and link
% write a file containing the token value and a simple message to click on link
compose_token_email(FileName, TokenValue) ->
	FileName = "/usr/home/yc/vm_shared_dir/temp/mailer/out",
	{ok, WriteHandle} = file:open(FileName, write),
	io:format(WriteHandle, "Hello, ~n~nPlease click on the following link to activate your account: https://192.168.43.220:8765/join/~s", [TokenValue]),
	% consider opening file in raw mode and using file:write (faster)
	erlang:display(mail_composed),
	ok = file:close(WriteHandle).

% send the file containing the token value to the submitted login email
send_token_email(FileName, Login) ->
	%ssmtp receiver@email.com < filename
	NewLogin = binary:bin_to_list(Login),
	S = io_lib:format("ssmtp ~p < ~p", [NewLogin, FileName]),
	[] = os:cmd(S),
	erlang:display(mail_sent),
	ok.

%% process token
%the url contains a token, check if it is valid. (if user doesn't have account??) call variously defined fun to process further (activate account)
%todo: send html form to enter password (pre-entered email id) if token is valid. as in pw reset. hide pw field in signup form. 
process_token(JoinToken) ->
	{{select,N}, IdTupleList} = db_helpers:id_given_signup_token(JoinToken),
	case N of
		1 ->
			[{Id, TokenStatus}] = IdTupleList,
			IsTokenActivated = is_token_activated(TokenStatus),
			activate_new_account(IsTokenActivated, Id);
		_ ->
			erlang:display(invalid_token),
			{200, #{<<"content-type">> => <<"text/plain">>}, <<"invalid token">>}
	end.

%% check if the signup token is activated and return atom
is_token_activated(IsTokenActivated) ->
	case IsTokenActivated of
		true ->
			token_is_activated;
		false ->
			token_is_not_activated;
		_ ->
			erroneous_token
	end.

%%activate account by creating db entry
activate_new_account(token_is_not_activated, Id) ->
	ActivationSuccess = db_helpers:activate_new_account(Id),
	case ActivationSuccess of
		ok ->
			{200, #{<<"content-type">> => <<"text/plain">>}, <<"account activated ok. try logging in.">>};
		_ ->
			{200, #{<<"content-type">> => <<"text/plain">>}, <<"problem with account activation. try again.">>}
	end;

activate_new_account(token_is_activated, _) ->
	{200, #{<<"content-type">> => <<"text/plain">>}, <<"account already activated. try logging in">>};

activate_new_account(_, _) ->
	{200, #{<<"content-type">> => <<"text/plain">>}, <<"erroneous activation request">>}.

