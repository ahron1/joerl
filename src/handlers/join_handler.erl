%%% handler for making new user accounts. this design of this handler is tightly coupled with the marketing db tables - for making waitlists, inviting potential users, etc. The tentative usage, however, does not leverage this coupling, mainly because the marketing db tables (pp_acq_fresh/invited) aren't used. 

-module(join_handler).
-behavior(cowboy_handler).
-export([init/2]).
%% to do: update tokens to work better with nginx. from js hit ____ for new join req, /new/ for ... so nginx can pattern match for /join/[0-9a-z]+/ for the token

init(Req0, State) ->
	%JoinToken is a parameter bound to the req that either contains the token value or denotes a brand new user request with the value "new"
	JoinToken = cowboy_req:binding(join_token, Req0),
	{Status, Headers, Body} = case JoinToken of
		<<"new">> ->
			new_join_req(Req0); %called via login page
		_ -> 
			process_token(JoinToken) %called when user clicks on emailed activation link with token. 
	end,
	Req = cowboy_req:reply(Status, Headers, Body, Req0),
	{ok, Req, State}.

% %% handle signup request with token "new"
new_join_req(Req0) ->
	{Login, FirstName} = entry_helpers:extract_signup_details(Req0),
	{{select, N}, IdTupleList} = db_helpers:id_given_login(Login),

	case N of 
		0 ->
			% the id is not present in pp_credentials, thus there's no record of this person in the system. So create a new account, send the user an activation token (todo: introduce waiting list)

			create_new_account(Login, FirstName); 
		1 ->
			% there is a record for this email, thus the person has either been invited or has signed up (to waiting list) previously. if it is an invited person, immediately email a signup token. if it is a voluntary signup, check if waiting period is over, etc.

			[{Id}] = IdTupleList,
			{{select, _}, [{ActiveStatus}]} = db_helpers:active_status_given_id(Id),
			InvitedOrFresh = invited_or_fresh(Id),
			process_existing_account(Login, FirstName, Id, ActiveStatus, InvitedOrFresh);
		_ ->
			send_error()
	end.

%process existing account depending on conditions
%process_existing_account(Login, FirstName, Id, ActiveStatus, InvitedOrFresh).
process_existing_account(_, _, _, true, _) ->
	{200, #{<<"content-type">> => <<"text/plain">>}, <<"there's already an account for this email. please try logging in.">>};
process_existing_account(Login, FirstName, Id, false, invited) ->
	%there exists an entry for this login - either via marketing or user invite. 
	{{update, 1}, _} = db_helpers:update_name(Id, FirstName),
	create_and_send_token(FirstName, Login, Id);
process_existing_account(Login, FirstName, Id, false, fresh) ->
	%user had signed up afresh - so check if the waiting period is up.
	{{select, N}, [{IsWaitingOver}]} = db_helpers:is_waiting_over(Id),
	case N of 
		1 -> 
			case IsWaitingOver of 
				true ->
					create_and_send_token(FirstName, Login, Id); %this should happen automatically when the waiting period ends. but just in case the it hasn't or the user has missed it. or the token has expired. 
				false ->
					{200, #{<<"content-type">> => <<"text/plain">>}, <<"you're already on the waiting list. We'll activate your account ASAP">>}
			end;
		_ ->
			send_error()
	end;
process_existing_account(_, _, _, _, _) ->
	send_error().

%create brand new account
create_new_account(Login, FirstName) -> 
		{{select, N}, NewUserIdTupleList} = db_helpers:new_account_creation(Login, FirstName),
		[{NewUserId}] = NewUserIdTupleList,
		case N of 
			1 ->
				create_and_send_token(FirstName, Login, NewUserId);

				%%todo: token will be created and sent when waiting period is over. via periodic script/cron job (or run manually on the server) that checks for waiting period. for now it is done directly here. the above line will be replaced with a different func for sending a signup/welcome to waiting list email.
				%%send_signup_email(Login, FirstName),
				%%{200, #{<<"content-type">> => <<"text/plain">>}, <<"You have successfully signed up. Your account is in the waiting list and will be activated soon. Please check your email">>};

			_ ->
				send_error()
		end.

%check if user was invited or signed up afresh and return atom invited/fresh/error
invited_or_fresh(Id) ->
	IsInvited = db_helpers:is_invited_given_id(Id),
	IsFresh = db_helpers:is_fresh_given_id(Id),
	InvitedOrFresh = {IsInvited, IsFresh},
	case InvitedOrFresh of
		{true, false} ->
			invited;
		{false, true} ->
			fresh;
		_ ->
			error
	end.

%send error message to client
send_error() ->
	{400, #{<<"content-type">> => <<"text/plain">>}, <<"There was a problem during sign up. Please try again. If the problem persists, please contact us with the details.">>}.

%create (or update) new signup token, email, return response tuple to send to client
create_and_send_token(FirstName, Login, Id) ->
	{{select, _N}, TokenTupleList} = db_helpers:create_signup_token(Id),
	[{TokenValue}] = TokenTupleList,
	%case N of 
	erlang:display(Login),
	erlang:display(TokenValue),
	email_helper:send_signup_token_email(TokenValue, Id, Login, FirstName),
	%db_helpers:signup_token_sent_date(NewUserId),

	{200, #{<<"content-type">> => <<"text/plain">>}, <<"We have sent you an activation link which will expire soon. Please check your email right away!">>}.

% %% process actual token
%the url contains a token, check if it exists. (if user doesn't have account??) call variously defined fun to process further (activate account)
%todo: check if token is still valid (is_signup_token_valid func in db)
process_token(JoinToken) ->
	{{select,N}, IdActTupleList} = db_helpers:id_given_signup_token(JoinToken),
	case N of
		1 ->
			[{Id, TokenStatus}] = IdActTupleList,
			%TokenStatus is a true/false tuple - about whether the token has already been activated 
			%IsTokenActivated = is_token_activated(TokenStatus), %function to check true/false status and return a meaningful atom. 
			activate_new_account(TokenStatus, Id);
		_ ->
			erlang:display(invalid_token),
			send_error()
	end.

%%activate account by creating db entry
%activate_new_account(TokenStatus, Id) %TokenStatus is true/false status whether the token has already been activated 
activate_new_account(false, Id) ->
	ActivationSuccess = db_helpers:activate_new_account(Id),
	case ActivationSuccess of
		ok ->
			% account activated. now send form to set password. 
			% setting (first time) password is treated the same as pw reset. so a pw token is created and the pw handler is called from the form. 
			{{insert, 1}, [{_TokenValue}]} = db_helpers:create_pw_token(Id),
			%directly activate the pw token immediately after creation - so that the user does not have to click on yet another pw token activation link. 
			%To improve ux, during the signup process, user is not exposed to pw tokens/activation - it is done internally. 
			{{update, 1}, _} = db_helpers:activate_pw_token(Id),
			Body_form = pw_form_body(),
			H_form = #{<<"content-type">> => <<"text/html">>},
			%todo: send welcome email. 
			{200, H_form, Body_form};
			%{200, #{<<"content-type">> => <<"text/plain">>}, <<"Congratulations! Your account has been activated. Try logging in.">>};
		_ ->
			send_error()
	end;
activate_new_account(true, _) ->
	{200, #{<<"content-type">> => <<"text/plain">>}, <<"Your account is already activated. Try logging in.">>};
activate_new_account(_, _) ->
	send_error().

%% html form and js to set/submit password
pw_form_body() ->
<<"
<html>
	<head>
		<script>
			function new_pw() {
			var xhr = new XMLHttpRequest;
			xhr.open(\'POST\', \"/password/new\");
			xhr.onload = function() {
				alert(this.response);
				document.getElementById(\"new-password\").value=\"\";
				document.getElementById(\"user-email\").value=\"\";
				if (xhr.status == 200) {
					window.location.href=\"https://192.168.64.2\";
					//alert(\"js alert: password changed. login again\")
					}
//				else {
//					alert(this.response);
//					}
				};
			//create FormData variable and append values from text input elements
			var data = new FormData();
			data.append(\"login-email\", document.getElementById(\"user-email\").value);
			data.append(\"login-password\", document.getElementById(\"new-password\").value)
			xhr.send(data);
			}
		</script>
	</head>
	<body>
		<u><b>This is a secure form to set your password </b></u>
		<br><br>
		<label for=\"Email\" >Enter your email: </label>
		<br>
		<input type=\"email\" data-clear-btn=\"true\" name=\"email\" id=\"user-email\" value=\"\" >
		<br>
		<label for=\"Password\">Enter your password:</label>
		<br>
		<input type=\"password\" data-clear-btn=\"true\" name=\"password\" id=\"new-password\" value=\"\" >
		<br>
		<button id=\"password-set-button\" onclick=\"new_pw()\" ><i>Set password</i></button>
	</body>
</html>
">>.
