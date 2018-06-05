-module(pw_reset_handler).
-behavior(cowboy_handler).

-export([init/2]).
%% to do: update tokens to work better with nginx. from js hit /forgot/ for new reset req, /new/ for ... so nginx can pattern match for /resetpassword/[0-9a-z]+/ for the token
init(Req0, State) ->
	PwToken = cowboy_req:binding(pw_token, Req0),
	erlang:display(PwToken),

	{Status, Headers, Body} = case PwToken of
		<<"forgot">> ->
			reset_request(Req0);
		<<"new">> ->
			set_new_pw(Req0);
		_ -> 
			token_validation(PwToken)
	end,

	Req = cowboy_req:reply(Status, Headers,
						   %#{<<"content-type">> => <<"text/plain">>},
						   Body,
						  Req0),
	{ok, Req, State}.


%new reset req. get form elements and create new token based on email
reset_request(Req0) ->
	erlang:display(no_token_so_new_req),
	{Login, _Password} = entry_helpers:extract_login_pw(Req0),
	erlang:display(Login),
	{{select, N}, IdPwTupleList} = db_helpers:id_pw_given_login(Login),
    H = #{<<"content-type">> => <<"text/plain">>},
	{S, H, B} = case N of %check if username is valid
		1 ->
			%Valid login id, get userid and create entry in tokens table
			[{Id, _Pw}] = IdPwTupleList,
			erlang:display(Id),
			{{insert, 0, 1}, [{TokenValue}]} = db_helpers:create_pw_token(Id),
			erlang:display(TokenValue),
%todo: 			%send it to user by mail
			{200, H, <<"password change">>}
			;
		_ ->
			{400, H, <<"invalid login">>}
			%Invalid login. try again
	end,
	{S, H, B}.

%receive submitted form with new pw and update db with new pw
set_new_pw(Req0) ->
	erlang:display(new_pw_set),
	{Login, NewPassword} = entry_helpers:extract_login_pw(Req0),
    H0 = #{<<"content-type">> => <<"text/plain">>},
	% first check if the login is valid
	{{select,N}, IdPwTupleList} = db_helpers:id_pw_given_login(Login),
	{S, H, B} = case N of 
		1 ->
			%since login is valid, get the id
			[{Id, _StoredPassword}] = IdPwTupleList,
			%before updating check if the token is still valid. 
			{{select, N1}, _} = db_helpers:check_valid_pw_token(Id),
			case N1 of
				1 -> 
					{{update, 1}, _} = db_helpers:update_pw(Id, NewPassword),
					%after updating, set is_token_used to true, to prevent reuse/abuse
					{{update, 1}, _} = db_helpers:disable_pw_token(Id),
					%also delete any session cookies for that Id
					{{delete, _}, _} = db_helpers:delete_session_cookie(Id),
					%server redirect upon post isn't standard practice
					%so send 200 and have js do the redirection
					%H_redir = #{<<"Location">> => <<"https://192.168.43.220:8765">>},
					%{303, H_redir, <<>>}
					{200, H0, <<"password changed. login again">>}
					;
				_ ->
					{400, H0, <<"invalid token">>}
			end
			;
		_ ->
			%login is invalid
			{400, H0, <<"no such login">>}
	end,
	%to do: send alert that pw is changed and a 3xx redirect response to homepage
	{S, H, B}.

%has token, so it is a user clicking the reset link. 
token_validation(PwToken) ->
	%check token in db. if valid...
	erlang:display(has_token_so_old_req),
	%get the id from the token (also checking if token is usable)
	{{select,N}, IdTupleList} =	db_helpers:id_given_valid_pw_token(PwToken),
    H = #{<<"content-type">> => <<"text/plain">>},
	%erlang:display(in_pw_r_h_after_db),
	{S1, H1, B1} = case N of 
		1 ->
			[{Id}] = IdTupleList,
			erlang:display(token_okay),
			%to do: add code in nginx to return 404 for this location unless referrer matched own domain. ??

			%activate token and send the form
			%token is activated by clicking on the link containing it. this is to make sure the actual user has activated it via their email. since the pw reset form doesn't submit the token, there is the risk that a malicious user resets the pw of someone else in the interval that the other party has a valid reset token.
			{{update, 1}, _} = db_helpers:activate_pw_token(Id),

			%H_redir = #{<<"Location">> => <<"https://192.168.64.2:8443/pwresetform">>},
			%{302, H_redir, <<"token is okay">>}
			Body_form = reset_form_body(),
			H_form = #{<<"content-type">> => <<"text/html">>},
			{200, H_form, Body_form}
			;
		_ ->
			{400, H, <<"invalid token">>}
			% to do: consider sending just a custom 404 instead of a meaningful response to the client, less communicative server is better for security
	end,
	{S1, H1, B1}.

%%prepare the html/js pw reset form to be sent to client. 
%%safest to send it bespoke from server in response to valid token 
%%instead of a prebuilt page that can also be accessed from elsewhere. 
reset_form_body() ->
<<"
<html>
	<head>
		<script>
			function new_pw() {
			var xhr = new XMLHttpRequest;
			xhr.open(\'POST\', \"/resetpassword/new\");
			xhr.onload = function() {
				alert(this.response);
				document.getElementById(\"password-reset\").value=\"\";
				document.getElementById(\"password-reset-email\").value=\"\";
				if (xhr.status == 200) {
					window.location.href=\"https://192.168.43.220:8765\";
					//alert(\"js alert: password changed. login again\")
					}
//				else {
//					alert(this.response);
//					}
				};
			//create FormData variable and append values from text input elements
			var data = new FormData();
			data.append(\"login-email\", document.getElementById(\"password-reset-email\").value);
			data.append(\"login-password\", document.getElementById(\"password-reset\").value)
			xhr.send(data);
			}
		</script>
	</head>
	<body>
		<u><b>This is a secure form to reset your password </b></u>
		<br><br>
		<label for=\"Email\" >Enter your email: </label>
		<br>
		<input type=\"email\" data-clear-btn=\"true\" name=\"email\" id=\"password-reset-email\" value=\"\" >
		<br>
		<label for=\"Password\">Enter your new password:</label>
		<br>
		<input type=\"password\" data-clear-btn=\"true\" name=\"password\" id=\"password-reset\" value=\"\" >
		<br>
		<button id=\"password-reset-button\" onclick=\"new_pw()\" ><i>Set new password</i></button>
	</body>
</html>
">>.
