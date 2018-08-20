-module(email_helper).
-export([send_pw_reset_email/3]).

%send email
send(UserEmail, Message) ->
	gen_smtp_client:send({"JoChoice Bouncer", [UserEmail], Message}, [{relay, "smtp.gmail.com"}, {tls, always}, {port, 587}, {username, "arunanda1985@gmail.com"}, {password, "Dakini1234"}]).

% create email body to be sent after receiving pw reset request
build_pw_reset_email(TokenValue, Id, UserEmail) -> 
	Domain = "https://192.168.64.2/",
	ResetRoute = "resetpassword/",
	TokenString = binary:bin_to_list(TokenValue),
	ResetLink = general_helpers:list_merge_no_sort([Domain, ResetRoute, TokenString]),
	UserName = binary:bin_to_list(db_helpers:name_given_id(Id)),
	EmailBody1 = "Hi ",
	EmailBody2 = "So you forgot your password, like the rest of us.." ,
	EmailBody3 = "It is easy to reset, just click the link below and enter the new password",
	EmailBody4 = "We look forward to seeing you soon at JoChoice.." ,
	NewLine = "\n",
	EmailBody5 = "Cheers!",
	EmailBody = general_helpers:list_merge_no_sort([EmailBody1, UserName, ",", NewLine, NewLine, EmailBody2, NewLine, NewLine, EmailBody3, NewLine, ResetLink, NewLine, NewLine, EmailBody4, NewLine, NewLine, EmailBody5]),
	%erlang:display(EmailBody),
	EmailHeader1 = "Subject: Jochoice Password Reset \r\nFrom: JoChoice Bouncer <arunanda1985@gmail.com>\r\nTo: ",
	EmailHeader2 = UserName,
	EmailHeader3 = "<",
	EmailHeader4 = UserEmail,
	EmailHeader5 = ">\r\n\r\n",
	EmailHeaders = general_helpers:list_merge_no_sort([EmailHeader1, EmailHeader2, EmailHeader3, EmailHeader4, EmailHeader5]),
	%erlang:display(EmailHeaders),
	{EmailHeaders, EmailBody}.

%send the password reset token email
send_pw_reset_email(TokenValue, Id, Login) ->
	UserEmail = binary:bin_to_list(Login),
	{EmailHeaders, EmailBody} = build_pw_reset_email(TokenValue, Id, UserEmail),
	EmailContent = string:concat(EmailHeaders, EmailBody),
	send(UserEmail, EmailContent).

