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

%%for a brand new user request, check various conditions and process the request
new_join_req(Req0) ->
	{Login, Inviter} = entry_helpers:extract_login_pw_inviter(Req0),

	%check if there are any details in the db for that account to verify that it is indeed a "brand new" user
	{{select,N}, ActivationTupleList} = db_helpers:activation_given_login(Login),
	erlang:display(ActivationTupleList),
	HasActiveAccount = has_active_account(N, ActivationTupleList), %this gets set to true when the account is activated (by clicking on token)
	HasSignedUp = has_signed_up(N, ActivationTupleList), % this is set to true when the user signs up
	IsValidInviter = is_valid_inviter(Inviter), 
	IsPreactivatedAccount = is_preactivated_account(N, ActivationTupleList), %Preactivated account is one that was originally invited by the system. 
	
	case HasActiveAccount of 
		has_active_account -> 
			%given login already has active functioning account
			{200, #{<<"content-type">> => <<"text/plain">>}, <<"there's already an account for this email. try logging in.">>}
			%todo: use new status code for js redir to homepage/reload
			;
		_ ->
			%given login doesn't have account.
			%but if user has signed up (and is on waiting list), ask to wait. 
			case HasSignedUp of
				has_signed_up ->
					erlang:display(already_on_waiting_list),
					{200, #{<<"content-type">> => <<"text/plain">>}, <<"you're already on the waiting list. We'll activate your account ASAP">>}
					%todo: use new status code for js redir to homepage/reload
					;
				_ ->
					create_new_account(Login, Inviter, IsPreactivatedAccount, IsValidInviter)
			end
	end.

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

%%actual creation of the account depending on conditions
%create_new_account(Login, Inviter, IsPreactivatedAccount, IsValidInviter)
%create_new_account(Login, _, is_preactivated_account, _) ->
%	create_new_account(Login),
%	activate_new_account(login, Login)
%	use atom login/token in tuple to distinguish direct activation vs activation via emailed url with token
%	;

%create_new_account(Login, Inviter, IsPreactivatedAccount, IsValidInviter)
%create_new_account(Login, Inviter, _, is_valid_inviter) -> 
%	create_new_account(Login),
%	;

%create_new_account(Login, Inviter, IsPreactivatedAccount, IsValidInviter)
create_new_account(Login, _, _, _) -> 
		erlang:display(new_acc_creation_req),
		{{select, N}, TokenTupleList} = db_helpers:new_account_creation(Login),
		case N of
			1 ->
				erlang:display(TokenTupleList),
				[{TokenValue}] = TokenTupleList,
				FileName = "/usr/home/yc/vm_shared_dir/temp/mailer/out",
				ok = compose_token_email(FileName, TokenValue),
				ok = send_token_email(FileName, Login),
				% todo: send confirmation email. (later) send email with token. 
				{200, #{<<"content-type">> => <<"text/plain">>}, <<"successfully signed up. please check email">>};
			_ ->
				{400, #{<<"content-type">> => <<"text/plain">>}, <<"there was a problem signing up. please try again">>}
		end
		.

%% write a file containing the token value and a simple message to click on link
compose_token_email(FileName, TokenValue) ->
	FileName = "/usr/home/yc/vm_shared_dir/temp/mailer/out",
	{ok, WriteHandle} = file:open(FileName, write),
	io:format(WriteHandle, "Hello, ~n~nPlease click on the following link to activate your account: https://192.168.43.220:8765/join/~s", [TokenValue]),
	% consider opening file in raw mode and using file:write (faster)
	erlang:display(mail_composed),
	ok = file:close(WriteHandle).

%% send the file containing the token value to the submitted login email
send_token_email(FileName, Login) ->
	%ssmtp receiver@email.com < filename
	NewLogin = binary:bin_to_list(Login),
	S = io_lib:format("ssmtp ~p < ~p", [NewLogin, FileName]),
	[] = os:cmd(S),
	erlang:display(mail_sent),
	ok.

%check if inviter email is valid (inviter has active account)
is_valid_inviter(Inviter) ->
	erlang:display(in_is_valid_inviter),
	{{select,N}, ActivationTupleList1} = db_helpers:activation_given_login(Inviter),
	erlang:display(after_query),
	erlang:display(ActivationTupleList1),
	HasActiveAccount = has_active_account(N, ActivationTupleList1),
	case HasActiveAccount of
		true ->
			is_valid_inviter;
		_ ->
			is_not_valid_inviter
	end.

%has_active_account(N, ActivationTupleList) --- return atom
has_active_account(1, ActivationTupleList) ->
	[{_Id, IsAccountActive, _IsPreactivatedAccount, _HasSignedUp}] = ActivationTupleList,
	case IsAccountActive of 
		true ->
			has_active_account;
		_ ->
			has_no_active_account
	end;
has_active_account(_, _) ->
	has_no_active_account.

%has_signed_up(N, ActivationTupleList) --- return atom
has_signed_up(1, ActivationTupleList) ->
	[{_Id, _IsAccountActive, _IsPreactivatedAccount, HasSignedUp}] = ActivationTupleList,
	case HasSignedUp of 
		true ->
			has_signed_up;
		_ ->
			has_not_signed_up
	end;
has_signed_up(_, _) ->
	has_not_signed_up.

%check if account is preactivated (i.e. invited by system)
is_preactivated_account(1, ActivationTupleList) ->
	[{_Id, _IsAccountActive, IsPreactivatedAccount, _HasSignedUp}] = ActivationTupleList,
	case IsPreactivatedAccount of 
		true ->
			is_preactivated_account;
		_ ->
			is_not_preactivated_account
	end;
is_preactivated_account(_, _) ->
	is_not_preactivated_account.

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

