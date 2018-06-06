%% based on the upload example packaged with cowboy
%% @doc Upload handler.
-module(upload_handler).
-export([init/2]).

%%to do: add new funs to check req body to check cookie status and send ERRONEOUS REQ like in login handler
%%to do later: abstract above into new module across (upload, message, profile?, etc.) handlers

init(Req0, Opts) ->
	{CookieStatus, User} = entry_helpers:check_session_cookie(Req0),
	%check if user is logged in before processing uploaded file/info
	{ResponseBody_r, ResponseStatus_r} = upload_processor(CookieStatus, User, Req0),

%	MaxFileSize = 4500000, %bytes
%	MaxFileName = 200, %characters

%	{Body, Status} = case CookieStatus of
%		has_no_session_cookie ->
%			ResponseBody = <<"Not logged in">>,
%			ResponseStatus = 400,
%			{ResponseBody, ResponseStatus};
%		has_session_cookie ->
%			{FileData, FileName, FileSize, Adj1, Adj2} = extract_file_and_adjs(Req0),
%			{FileNameWithPrefix, FileNameWithPath} = create_file_name_for_storage(FileName),
%			{ResponseBody, ResponseStatus} = file_storage(FileSize, MaxFileSize, MaxFileName, FileNameWithPath, FileData),
%			{_NewImageId, _Adj1Text, _Adj2Text} = db_helpers:image_details_to_db(User, FileNameWithPrefix, Adj1, Adj2),
%			{ResponseBody, ResponseStatus}
%	end,
%	{ResponseBody_r, ResponseStatus_r} = {Body, Status},

	Req = cowboy_req:reply(ResponseStatus_r, #{
		<<"content-type">> => <<"text/html">>
		}, ResponseBody_r, Req0),
		{ok, Req, Opts}.

%% check loggedin status and process upload request
upload_processor(has_session_cookie, User, Req0) ->
	MaxFileSize = 4500000, %bytes
	MaxFileName = 200, %characters
	{FileData, FileName, FileSize, Adj1, Adj2} = extract_file_and_adjs(Req0),
	{FileNameWithPrefix, FileNameWithPath} = create_file_name_for_storage(FileName),
	{ResponseBody, ResponseStatus} = file_storage(FileSize, MaxFileSize, MaxFileName, FileNameWithPath, FileData),
	{_NewImageId, _Adj1Text, _Adj2Text} = db_helpers:image_details_to_db(User, FileNameWithPrefix, Adj1, Adj2),
	{ResponseBody, ResponseStatus};

upload_processor(_, _, _) ->
	ResponseBody = <<"Not logged in">>,
	ResponseStatus = 400,
	{ResponseBody, ResponseStatus}.


%%extract the File data, file name and adjectives from incoming req
extract_file_and_adjs(Req) ->
	{ok, Headers, Req2} = cowboy_req:read_part(Req),
	
	%Data0 is the file data
	{ok, Data0, Req3} = cowboy_req:read_part_body(Req2),
	%io:format("DATA0 ~n ~p~n", [Data0]),

	FileSize = byte_size(Data0),
	{file, <<"inputfile">>, FileName, _} = cow_multipart:form_data(Headers),

%add checks to be sure Headers are what they should be. prevent tampered requests. 
	{ok, _Headers1, Req4} = cowboy_req:read_part(Req3),
	{ok, Data1, Req5} = cowboy_req:read_part_body(Req4),

	{ok, _Headers2, Req6} = cowboy_req:read_part(Req5),
	{ok, Data2, _Req7} = cowboy_req:read_part_body(Req6),

%	{ok, Headers3, Req8} = cowboy_req:read_part(Req7),
%	{ok, Data3, Req9} = cowboy_req:read_part_body(Req8),

	{Data0, FileName, FileSize, Data1, Data2}.

%%prepend timestamp to the uploaded file name
create_file_name_for_storage(FileName) ->
	Name = binary:bin_to_list(FileName),
	Location = "/usr/home/yc/vm_shared_dir/temp/uploaded/",
	Hyphen = "-",
	Slash = "/",

	{{YYYY, MM, DD},{H,M,S}} = calendar:local_time(),
	Year = integer_to_list(YYYY),
	Month = integer_to_list(MM),
	Date = integer_to_list(DD),
	Hours = integer_to_list(H),
	Minutes = integer_to_list(M),
	Seconds = integer_to_list(S),

	TimeStamp = general_helpers:list_merge_no_sort([Year, Hyphen, Month, Hyphen, Date, Hyphen, Hours, Hyphen, Minutes, Hyphen, Seconds]),

	{time, Time} = lists:keyfind(time, 1, erlang:system_info(os_system_time_source)),
	TimePrefix = integer_to_list(Time),
	%TimePrefix is the timestamp in milliseconds since Jan 1 1970. used in addition to Y-M-D-H-M-S timestamp to ensure uniqueness. 

	FileNameWithPrefix = general_helpers:list_merge_no_sort([TimeStamp, Hyphen, TimePrefix, Hyphen, Name]),
	FileNameWithPath = general_helpers:list_merge_no_sort([Location, Hours, Slash, FileNameWithPrefix]),

	{FileNameWithPrefix, FileNameWithPath}.

%%if filesize is okay, store file (in filesystem) and create response
file_storage(FileSize, MaxFileSize, MaxFileName, FileNameWithPath, FileData) when (FileSize < MaxFileSize) and (length(FileNameWithPath) < MaxFileName) ->
	{ok, WriteHandle} = file:open(FileNameWithPath, write),
	io:format(WriteHandle, "~s", [FileData]),
	% consider opening file in raw mode using file:write (faster)
	file:close(WriteHandle),
	Body = <<"File uploaded successfully">>,
	Status = 200,
	{Body, Status};

file_storage(FileSize, MaxFileSize, _MaxFileName, _FileNameWithPath, _FileData) when (FileSize > MaxFileSize) -> 
	Body = <<"File too large">>,
	Status = 500,
	{Body, Status};

file_storage(_FileSize, _MaxFileSize, MaxFileName, FileNameWithPath, _FileData) when  (length(FileNameWithPath) > MaxFileName) ->
	Body = <<"File name too long">>,
	Status = 500,
	{Body, Status}.

