-module(db_helpers).
-export([id_pw_given_login/1, check_session_cookie/1, create_session_cookie/1, cookie_given_id/1, create_pw_token/1, id_given_valid_pw_token/1, update_pw/2, activate_pw_token/1, check_valid_pw_token/1, disable_pw_token/1, activation_given_login/1, new_account_creation/2, id_given_signup_token/1, activate_new_account/1, image_details_to_db/4, get_new_pics/1]).

%% add is_account_active to id/pw/queries
%% hash pw, and check pw directly in db.
%% consolidate activation_given_login and id_pw_given_login (rename to credentials_given_login?), here and in all caller funs; move has_active_account into entry_helper. ??

% %% account/id/pw
%% get the stored password from the database given the submitted login
id_pw_given_login(FormLogin) ->
	{{select,N}, IdPwTupleList} = pp_db:extended_query("select property_of, password_text from person_property_credentials where email = $1 and is_account_active = true", [FormLogin]),
	{{select,N}, IdPwTupleList}.

%% get account activation status from the database given the submitted login
activation_given_login(FormLogin) ->
	erlang:display(in_activation_given_form_login),
	{{select,N}, ActivationTupleList} = pp_db:extended_query("select property_of, is_account_active, is_account_preactivated, has_signed_up from person_property_credentials where email = $1", [FormLogin]),
	erlang:display(after_query_db_helpers),
	{{select,N}, ActivationTupleList}.

%% new (inactive) account creation using given login/pw
new_account_creation(Login, Password) ->
	{{select, 1}, TokenTupleList} = pp_db:extended_query("
		with 
			person as (
			insert into person 
			values(default) 
			returning id
		    )
 		    ,person_property as (
			insert into person_property (property_of) 
			select id from person 
			returning id
			)
			,person_property_credentials as (
			insert into person_property_credentials (property_of, email, password_text, has_signed_up) 
			select id, (select $1), (select $2), true from person
			returning id
			)
			,person_property_signup_tokens as (
			insert into person_property_signup_tokens (property_of)
			select id from person
			returning token_value
			)
		select token_value from person_property_signup_tokens
		", [Login, Password]),
	{{select, 1}, TokenTupleList}.

%%signup token validation
id_given_signup_token(JoinToken) ->
	{{select,N}, IdTupleList} = pp_db:extended_query("select property_of, is_token_activated from person_property_signup_tokens where token_value = $1", [JoinToken]),
	{{select,N}, IdTupleList}.

%%account activation
activate_new_account(Id) ->
	{{update, 1}, _} = pp_db:extended_query("update person_property_signup_tokens set is_token_activated = true where property_of=$1", [Id]),
	{{update, 1}, _} = pp_db:extended_query("update person_property_credentials set is_account_active = true where property_of=$1", [Id]),
	ok.

% %% cookies
%%check cookie in db given ID. if user closed window losing still valid cookie 
cookie_given_id(Id) ->
	{{select,N}, CookieTupleList} = pp_db:extended_query("select cookie from person_property_session p where property_of = $1 and p.is_cookie_valid = true", [Id]),
	%update last update timestamp if cookie exists
	case N of 
		1 -> 
			pp_db:extended_query("update person_property_session set time_of_last_update = CURRENT_TIMESTAMP where property_of=$1", [Id]);
		_ ->
			no_ongoing_session
	end,
	{N, CookieTupleList}.

%% check in db if cookie exists. Get Id given cookie
check_session_cookie(Cookie) ->
	%add something like where is_valid=1 to query to check only for valid cookies
	{{select,N}, IdTupleList} = pp_db:extended_query("select property_of from person_property_session p where cookie = $1 and p.is_cookie_valid = true", [Cookie]),
	case N of 
		1 -> 
			%update last update timestamp if cookie exists
			[{Id}] = IdTupleList,
			erlang:display(Id),
			{{update, 1}, _} = pp_db:extended_query("update person_property_session set time_of_last_update = CURRENT_TIMESTAMP where property_of=$1", [Id]),
			{has_session_cookie, Id};
		_ ->
			{has_no_session_cookie, no_such_user}
	end.

%% insert login id into cookie table returning value of generated cookie
create_session_cookie(Id) ->
	%first delete any old (invalid) cookie for the userid then insert
	{{delete, _}, _} = delete_session_cookie(Id),
	%{{insert, 0, 1}, [{CookieValue}]} = pp_db:extended_query("insert into person_property_session(property_of) values($1) returning cookie", [Id]),
	{{insert, 1}, [{CookieValue}]} = pp_db:extended_query("insert into person_property_session(property_of) values($1) returning cookie", [Id]),
	CookieValue.

%% delete session cookie given Id
delete_session_cookie(Id) ->
	{{delete, _}, _} = pp_db:extended_query("delete from person_property_session where property_of=$1", [Id]).

% %% password reset
%%insert new token for resetting pasword
create_pw_token(Id) ->
	%first delete any old token for the userid then insert
	{{delete, _}, _} = pp_db:extended_query("delete from person_property_pw_tokens where property_of = $1", [Id]),
	%{{insert, 0, 1}, _} = 	pp_db:extended_query("insert into person_property_pw_tokens(property_of) values($1) returning token_value", [Id]).
	{{insert, 1}, _} = 	pp_db:extended_query("insert into person_property_pw_tokens(property_of) values($1) returning token_value", [Id]).

%%check if password token is valid and return id if it is
id_given_valid_pw_token(Token) ->
	%erlang:display(in_id_given_pw_token_db),
	%the is_token_activated field is to ensure the user has actually clicked on the email link, and it is not a malicious user somehow sending the form.
	%check for is_token_activated because user might click on pw reset link but not actually change the pw. so the link can be clicked only once. 
	%also check for is_token_used to prevent reuse/abuse of token
	{{select,N}, IdTupleList} = pp_db:extended_query("select property_of from person_property_pw_tokens p where token_value = $1 and is_token_used = false and is_token_activated = false and p.is_pw_token_valid = true", [Token]),
	%it is possible to call the function check_valid_pw_token, but better to have a single query and add a condition to it. 
	%erlang:display(IdTupleList),
	{{select,N}, IdTupleList}.

%%update password given login
update_pw(Id, NewPassword) ->
	{{update, 1}, _} = pp_db:extended_query("update person_property_credentials set password_text = $1 where property_of=$2", [NewPassword, Id]).

%%update password given login
activate_pw_token(Id) ->
	{{update, 1}, _} = pp_db:extended_query("update person_property_pw_tokens set is_token_activated = true where property_of=$1", [Id]).

%%check if there is valid and activated pw token
check_valid_pw_token(Id) ->
	{{select, 1}, _} = pp_db:extended_query("select 1 from person_property_pw_tokens p where property_of=$1 and is_token_used = false and is_token_activated = true and p.is_pw_token_valid = true", [Id]).

%%set is_token_used to true to prevent reuse/abuse
disable_pw_token(Id) ->
	{{update, 1}, _} = pp_db:extended_query("update person_property_pw_tokens set is_token_used = true where property_of=$1", [Id]).

% %% imagery
%% save image details to db
image_details_to_db(UserId, NewFileName, Adj1, Adj2) ->
	{{select, 1}, [{NewImageId, {citext, Adj1Text}, {citext, Adj2Text}}]} = pp_db:extended_query("
		with 
		   -- input details from server
		   userid as (
		   select ($1::int) as id
		   )
		   ,picfilename as ( 
		   select ($2) as filename
		   )
		   ,inputword1 as ( 
		   select ($3) as inputword
		   )
		   ,inputword2 as (
		   select ($4) as inputword
		   )
		   -- check conditions and insert into db
		   ,w1 as ( 
		   select word, property_of
		   from   adjective_property
		   where  word = (select inputword from inputword1)
		   )
		   ,a1 as (
		   insert into adjective            
		   select                          
		   where not exists (select from w1)
		   returning id
		   )
		   ,ap1 as (
		   insert into adjective_property (word, property_of, is_adjective_active) 
		   select (select inputword from inputword1), id, 'true'
		   from   a1
		   returning word, property_of
		   )
		   ,w2 as (
		   select word, property_of
		   from   adjective_property
		   where  word = (select inputword from inputword2)
		   )
		   ,a2 as (
     	   insert into adjective            
   		   select                          
		   where not exists (select from w2)
		   returning id
		   )
		   ,ap2 as (
		   insert into adjective_property (word, property_of, is_adjective_active) 
		   select (select inputword from inputword2), id, 'true'
		   from   a2
		   returning word, property_of
		   ) 
		   ,p as ( 
		   insert into picture
   		   values(default)
   		   returning id
 		   )     
 		   ,pp as (
 		   insert into picture_property(property_of, is_picture_active, filename)
 		   select id, 'true', (select filename from picfilename) from p
 		   )
 		   ,person2p as (
 		   insert into person_to_picture(source, target)
 		   select (select id from userid), id from p
 		   returning id
 		   )
 		   ,person2pp as ( 
 		   insert into person_to_picture_property(property_of, is_uploader)
 		   select id, 'true' from person2p
		   )
		   ,a2p1 as ( 
		   insert into adjective_to_picture(source, target) 
		   values ((select property_of from ap1 union all select property_of from w1), (select id from p))
		   returning id, target, source 
		   )
		   ,a2pp1 as (
		   insert into adjective_to_picture_property(property_of, coupled_by) 
		   select id, (select id from userid) from a2p1
		   )
		   ,a2p2 as (
		   insert into adjective_to_picture(source, target) 
		   values ((select property_of from ap2 union all select property_of from w2), (select id from p))
		   returning id, target, source
		   )
		   ,a2pp2 as (
		   insert into adjective_to_picture_property(property_of, coupled_by) 
		   select id, (select id from userid) from a2p2
		   )
	  	   ,a2a_new as (
		   insert into adjective_to_adjective(source, target)
		   values ( 
				(select property_of from w1 union all select property_of from ap1) 
				,(select property_of from w2 union all select property_of from ap2)
				)
		   on conflict  
		   do nothing 
		   returning source, target, id
		   )
		   ,a2ap as ( 
		   insert into adjective_to_adjective_property (property_of, is_pair)
		   select id, 'true' from a2a_new
		   where exists (select 1 from a2a_new) 
		   returning id, property_of
		   )
		   ,person2a1 as ( 
		   insert into person_to_adjective(source, target) 
		   select (select id from userid), property_of from ap1
		   where exists (select 1 from ap1) 
		   returning id, target, source
		   )
		   ,person2a1p as (
		   insert into person_to_adjective_property(property_of, is_uploader) 
		   select id, 'true' from person2a1
		   where exists (select 1 from person2a1)
		   )    
		   ,person2a2 as (
		   insert into person_to_adjective(source, target) 
		   select (select id from userid), property_of from ap2
		   where exists (select 1 from ap2)
		   returning id, target, source
		   )
		   ,person2a2p as ( 
		   insert into person_to_adjective_property(property_of, is_uploader) 
		   select id, 'true' from person2a2
		   where exists (select 1 from person2a2)
		   )    
		   -- join tables and select output
	    select p.id as pic, w1.word as adj1, w2.word as adj2 
		from p
		join a2p1 on p.id = a2p1.target
		join w1 on w1.property_of = a2p1.source
		join a2p2 on p.id = a2p2.target
		join w2 on w2.property_of = a2p2.source

		union all 

		select p.id as pic, ap1.word as adj1, ap2.word as adj2 
		from p
		join a2p1 on p.id = a2p1.target
		join ap1 on ap1.property_of = a2p1.source
		join a2p2 on p.id = a2p2.target
		join ap2 on ap2.property_of = a2p2.source

		union all 

		select p.id as pic, w1.word as adj1, ap2.word as adj2 
		from p
		join a2p1 on p.id = a2p1.target
		join w1 on w1.property_of = a2p1.source
		join a2p2 on p.id = a2p2.target
		join ap2 on ap2.property_of = a2p2.source

		union all 

		select p.id as pic, ap1.word as adj1, w2.word as adj2 
		from p
		join a2p1 on p.id = a2p1.target
		join ap1 on ap1.property_of = a2p1.source
		join a2p2 on p.id = a2p2.target
		join w2 on w2.property_of = a2p2.source
	", [UserId, NewFileName, Adj1, Adj2]),
	{NewImageId, Adj1Text, Adj2Text}.

%% get list of next 10 pics (with adjectives) not seen by given user
get_new_pics(UserId) ->
	{{select, _N}, ImgAdjTupleList} = pp_db:extended_query("
		with 
	    userid as ( 
	    select ($1::int) as id
	    )
		,p as (
		select target from person_to_picture
		where source = (select id from userid)
		)    
   	   select *
  	   from pic_adj_adj
  	   where pic_adj_adj.picture not in (select target from p)
  	   fetch first 10 rows only
	", [UserId]),
	JsonList = image_helpers:tuple_list_to_json(ImgAdjTupleList),
	erlang:display(JsonList).

%% store message to server. 
%store_message(User, SenderName, SenderEmail, MessageContent) ->

%if it is necessary to use quoted column names escape the quotes as below:
%{{select,N}, UserId} = pp_db:extended_query("select \"userid\" from cookietable where value = $1", [Cookie]),
