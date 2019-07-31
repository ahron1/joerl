-module(email_helper).
-export([send_website_message/3, send_pw_reset_email/3, send_signup_token_email/4, send_welcome_email/2, send_website_message_autoreply/2]).
%-export([send_website_message/3, send_pw_reset_email/3, send_signup_token_email/4, send_website_message_autoreply/2]).

%send email
send(Sender, ReceiverEmail, Message) ->
	gen_smtp_client:send({Sender, [ReceiverEmail], Message}, [{relay, "smtp.zoho.com"}, {tls, always}, {port, 587}, {username, "mail@jochoice.com"}, {password, "4runN$nd$"}]).

%user welcome email - after they set their password. 
send_welcome_email(Login, Id) ->
	UserEmail = binary:bin_to_list(Login),
	{EmailHeaders, EmailBody} = build_welcome_email(Id, UserEmail),
	EmailContent = string:concat(EmailHeaders, EmailBody),
	Sender = "JoChoice Messenger",
	send(Sender, UserEmail, EmailContent).

% create email body to be sent after receiving pw reset request
build_welcome_email(Id, UserEmail) -> 
	Domain = "https://jochoice.com/",
	UserName = binary:bin_to_list(db_helpers:name_given_id(Id)),
	EmailBody1 = "Hi ",
	EmailBody2 = "Your new beta account is now ready for you. Your password has been set, and your login id/username is your email address." ,
	EmailBody3 = "You are now all set, to login, browse, and vote as you choose.",
	EmailBody4 = "We look forward to seeing you quite often at JoChoice.." ,
	NewLine = "\n",
	EmailBody5 = "Cheers! \n Jo Choice.",
	EmailBody = general_helpers:list_merge_no_sort([EmailBody1, UserName, ",", NewLine, NewLine, EmailBody2, NewLine, NewLine, EmailBody3, NewLine, NewLine, EmailBody4, NewLine, NewLine, EmailBody5, NewLine, Domain]),
	%erlang:display(EmailBody),
	EmailHeader1 = "Subject: Welcome to Jochoice \r\nFrom: JoChoice Messenger <mail@jochoice.com>\r\nTo: ",
	EmailHeader2 = UserName,
	EmailHeader3 = "<",
	EmailHeader4 = UserEmail,
	EmailHeader5 = ">\r\n\r\n",
	EmailHeaders = general_helpers:list_merge_no_sort([EmailHeader1, EmailHeader2, EmailHeader3, EmailHeader4, EmailHeader5]),
	%erlang:display(EmailHeaders),
	{EmailHeaders, EmailBody}.

%password reset emails
%send the password reset token email
send_pw_reset_email(TokenValue, Id, Login) ->
	UserEmail = binary:bin_to_list(Login),
	{EmailHeaders, EmailBody} = build_pw_reset_email(TokenValue, Id, UserEmail),
	EmailContent = string:concat(EmailHeaders, EmailBody),
	Sender = "JoChoice Messenger",
	send(Sender, UserEmail, EmailContent).

% create email body to be sent after receiving pw reset request
build_pw_reset_email(TokenValue, Id, UserEmail) -> 
	Domain = "https://jochoice.com/",
	ResetRoute = "password/",
	TokenString = binary:bin_to_list(TokenValue),
	ResetLink = general_helpers:list_merge_no_sort([Domain, ResetRoute, TokenString]),
	UserName = binary:bin_to_list(db_helpers:name_given_id(Id)),
	EmailBody1 = "Hi ",
	EmailBody2 = "So you forgot your password, like we all sometimes do.." ,
	EmailBody3 = "It is easy to reset, just click the link below and enter the new password",
	EmailBody4 = "We look forward to seeing you soon at JoChoice.." ,
	NewLine = "\n",
	EmailBody5 = "Cheers! \n Jo Choice.",
	EmailBody = general_helpers:list_merge_no_sort([EmailBody1, UserName, ",", NewLine, NewLine, EmailBody2, NewLine, NewLine, EmailBody3, NewLine, ResetLink, NewLine, NewLine, EmailBody4, NewLine, NewLine, EmailBody5]),
	%erlang:display(EmailBody),
	EmailHeader1 = "Subject: Jochoice Password Reset \r\nFrom: JoChoice Messenger <mail@jochoice.com>\r\nTo: ",
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
	%some of the variable names in this function (e.g. ResetRoute, etc.) are inappropriate, because they are derived from the earlier build_pw_reset_email.
	%todo - change the variable names to be appropriate to this module. 
	Domain = "https://jochoice.com/",
	ResetRoute = "join/",
	TokenString = binary:bin_to_list(TokenValue),
	ResetLink = general_helpers:list_merge_no_sort([Domain, ResetRoute, TokenString]),
	%UserName = binary:bin_to_list(db_helpers:name_given_id(Id)),
	UserName = binary:bin_to_list(Name),
	EmailBody1 = "Hi ",
	EmailBody2 = "Congratulations, your new beta account is now almost ready to access. You are now one of very few people who have this exclusive privilege. For now, you can view and vote on photos, but you will soon be able to do more." ,
	EmailBody3 = "All that remains is to activate your new account by setting a password. Just click the link below, this will let you set your password, so you can login. ",
	EmailBody4 = "We look forward to seeing you soon at JoChoice.." ,
	NewLine = "\n",
	EmailBody5 = "Cheers! \n Jo Choice.",
	EmailBody = general_helpers:list_merge_no_sort([EmailBody1, UserName, ",", NewLine, NewLine, EmailBody2, NewLine, NewLine, EmailBody3, NewLine, ResetLink, NewLine, NewLine, EmailBody4, NewLine, NewLine, EmailBody5]),
	%erlang:display(EmailBody),
	EmailHeader1 = "Subject: Just one step to access Jochoice \r\nFrom: JoChoice Messenger <mail@jochoice.com>\r\nTo: ",
	EmailHeader2 = UserName,
	EmailHeader3 = "<",
	EmailHeader4 = UserEmail,
	EmailHeader5 = ">\r\n\r\n",
	EmailHeaders = general_helpers:list_merge_no_sort([EmailHeader1, EmailHeader2, EmailHeader3, EmailHeader4, EmailHeader5]),
	%erlang:display(EmailHeaders),
	{EmailHeaders, EmailBody}.

%send messages from website via contact form to own email address. 
send_website_message(WebUserName, WebUserEmail, MessageContent) ->
	%Message = general_helpers:list_merge_no_sort(["Sender Name - ", WebUserName, "\n", "Sender Email -  ", WebUserEmail, "\n", MessageContent]),
	Body = general_helpers:list_merge_no_sort(["Sender Name - ", WebUserName, "\n", "Sender Email -  ", WebUserEmail, "\n", MessageContent ]),
	{Header} = build_message_header(),
	EmailContent = string:concat(Header, Body),
	%Message = general_helpers:list_merge_no_sort([ WebUserName, "\n", WebUserEmail, "\n", MessageContent]),
	%erlang:display(EmailContent),
	erlang:display(new_user_message_via_contactform),
	SenderName = "JoChoice Messenger",
	%ReceiverEmail = <<"contact@jochoice.com">>,
	ReceiverEmail = <<"mailjochoice@gmail.com">>,
	send(SenderName, ReceiverEmail, EmailContent).

%send automatic reply to people sending message via the webform. 
send_website_message_autoreply(SenderName, SenderEmail) ->
	{EmailHeaders, EmailBody} = build_website_message_autoreply(SenderName, SenderEmail),
	EmailContent = string:concat(EmailHeaders, EmailBody),
	SenderName1 = "JoChoice Messenger",
	send(SenderName1, SenderEmail, EmailContent).

% create email body to be sent in reply to user message received via webform.
build_website_message_autoreply(SenderName, SenderEmail) -> 
	EmailBody1 = "Hi ",
	EmailBody2 = "Thank you for writing to JoChoice. We welcome any and all feedback from our users." ,
	EmailBody3 = "While it sometimes takes us longer than we would like, we make sure to read, and if necessary, reply to, all the messages our users write to us.",
	EmailBody4 = "We look forward to seeing you back at JoChoice.." ,
	NewLine = "\n",
	EmailBody5 = "Cheers! \n Jo Choice.",
	EmailBody = general_helpers:list_merge_no_sort([EmailBody1, SenderName, ",", NewLine, NewLine, EmailBody2, NewLine, NewLine, EmailBody3, NewLine, NewLine, EmailBody4, NewLine, NewLine, EmailBody5]),
	%erlang:display(EmailBody),
	EmailHeader1 = "Subject: Re - Your message to JoChoice \r\nFrom: JoChoice Messenger <mail@jochoice.com>\r\nTo: ",
	EmailHeader2 = SenderName,
	EmailHeader3 = "<",
	EmailHeader4 = SenderEmail,
	EmailHeader5 = ">\r\n\r\n",
	EmailHeaders = general_helpers:list_merge_no_sort([EmailHeader1, EmailHeader2, EmailHeader3, EmailHeader4, EmailHeader5]),
	%erlang:display(EmailHeaders),
	{EmailHeaders, EmailBody}.

% create message header for use in send_website_message (to own address) function
build_message_header() ->
	EmailHeader1 = "Subject: User message to JoChoice \r\nFrom: JoChoice Messenger <mail@jochoice.com>\r\nTo: ",
	EmailHeader2 = ["JoChoice Messenger"],
	EmailHeader3 = "<",
	EmailHeader4 = ["mail@jochoice.com"],
	EmailHeader5 = ">\r\n\r\n",
	EmailHeaders = general_helpers:list_merge_no_sort([EmailHeader1, EmailHeader2, EmailHeader3, EmailHeader4, EmailHeader5]),
	%erlang:display(EmailHeaders),
	{EmailHeaders}.

