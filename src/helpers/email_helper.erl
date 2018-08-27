-module(email_helper).
-export([send_pw_reset_email/3, send_signup_token_email/4]).

%send email
send(Sender, UserEmail, Message) ->
	gen_smtp_client:send({Sender, [UserEmail], Message}, [{relay, "smtp.gmail.com"}, {tls, always}, {port, 587}, {username, "arunanda1985@gmail.com"}, {password, "Dakini1234"}]).

%password reset emails
%send the password reset token email
send_pw_reset_email(TokenValue, Id, Login) ->
	UserEmail = binary:bin_to_list(Login),
	{EmailHeaders, EmailBody} = build_pw_reset_email(TokenValue, Id, UserEmail),
	EmailContent = string:concat(EmailHeaders, EmailBody),
	Sender = "JoChoice Bouncer",
	send(Sender, UserEmail, EmailContent).

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

%signup emails
%send the signup token email
send_signup_token_email(TokenValue, Id, Login, UserName) ->
	UserEmail = binary:bin_to_list(Login),
	{EmailHeaders, EmailBody} = build_signup_token_email(TokenValue, Id, UserEmail, UserName),
	EmailContent = string:concat(EmailHeaders, EmailBody),
	Sender = "JoChoice Host",
	send(Sender, UserEmail, EmailContent).

% create email body to be sent after receiving signup request
build_signup_token_email(TokenValue, _Id, UserEmail, Name) -> 
	Domain = "https://192.168.64.2/",
	ResetRoute = "join/",
	TokenString = binary:bin_to_list(TokenValue),
	ResetLink = general_helpers:list_merge_no_sort([Domain, ResetRoute, TokenString]),
	%UserName = binary:bin_to_list(db_helpers:name_given_id(Id)),
	UserName = binary:bin_to_list(Name),
	EmailBody1 = "Hi ",
	EmailBody2 = "Congratulations, your beta account is now ready to access. You are now one of very few people that have this exclusive privilege. On behalf of the whole team, I welcome you to JoChoice. For now, you can view and vote on photos, but you will soon be able to do more." ,
	EmailBody3 = "Just click the link below to set your password, and you can login. ",
	EmailBody4 = "We look forward to seeing you soon at JoChoice.." ,
	NewLine = "\n",
	EmailBody5 = "Cheers! \n Jo.",
	EmailBody = general_helpers:list_merge_no_sort([EmailBody1, UserName, ",", NewLine, NewLine, EmailBody2, NewLine, NewLine, EmailBody3, NewLine, ResetLink, NewLine, NewLine, EmailBody4, NewLine, NewLine, EmailBody5]),
	%erlang:display(EmailBody),
	EmailHeader1 = "Subject: Welcome to Jochoice \r\nFrom: JoChoice Host <arunanda1985@gmail.com>\r\nTo: ",
	EmailHeader2 = UserName,
	EmailHeader3 = "<",
	EmailHeader4 = UserEmail,
	EmailHeader5 = ">\r\n\r\n",
	EmailHeaders = general_helpers:list_merge_no_sort([EmailHeader1, EmailHeader2, EmailHeader3, EmailHeader4, EmailHeader5]),
	%erlang:display(EmailHeaders),
	{EmailHeaders, EmailBody}.


