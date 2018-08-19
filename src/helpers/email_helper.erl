-module(email_helper).
-export([send/3]).

send(SenderName, SenderEmail, MessageContent) -> 
	EmailHeaders = "Subject: website email\r\nFrom: Arun Nanda <arunanda1985@gmail.com>\r\nTo: Master Yogesh <shriyogeshchandra@gmail.com>\r\n\r\n",
	EmailBody = general_helpers:list_merge_no_sort(["Sender Name: ", SenderName, "\n", "Sender Email: ", SenderEmail, "\n", "Message: ", MessageContent, "\n"]),
	EmailContent = string:concat(EmailHeaders, EmailBody),

	gen_smtp_client:send({"arunanda1985222", ["shriyogeshchandra@gmail.com"], EmailContent},   [{relay, "smtp.gmail.com"}, {tls, always}, {port, 587}, {username, "arunanda1985@gmail.com"}, {password, "Dakini1234"}]).



