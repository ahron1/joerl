%% upload new images from client to server/db
%%to do: abstract login checks into new module across different  handlers
-module(upload_handler).
-export([init/2]).

init(Req0, Opts) ->
	{CookieStatus, User} = entry_helpers:check_session_cookie(Req0),
	%check if user is logged in before processing uploaded file/info
	{ResponseBody_r, ResponseStatus_r} = upload_processor(CookieStatus, User, Req0),

	Req = cowboy_req:reply(ResponseStatus_r, #{
		<<"content-type">> => <<"text/html">>
		}, ResponseBody_r, Req0),
		{ok, Req, Opts}.

%% check loggedin status and process upload request
upload_processor(has_session_cookie, User, Req0) ->
	MaxFileSize = 4500000, %bytes
	{FileData, FileName, FileSize, Adj1, Adj2} = extract_file_and_adjs(Req0),
	{ok, FileMime} = emagic:from_buffer(FileData), % Mime based on magic numbers

	%erlang:display(FileData),
	erlang:display(FileName),
	erlang:display(Adj1),
	erlang:display(Adj2),
	erlang:display(FileMime),

	%% check for extension matching mime type, 
	case file_adj_validator(FileName, FileMime, is_mime_ok(FileMime), Adj1, Adj2) of
		file_valid ->
			FileHash = get_hash(crypto:hash(sha, FileData)),
			{FinalFileName, FileNameWithPath, TempFile, ImageUri} = create_file_name(FileName, FileHash),
			{ResponseBody, ResponseStatus} = file_storage(FileSize, MaxFileSize, FinalFileName, FileNameWithPath, TempFile, FileData, User, ImageUri, Adj1, Adj2),
			{ResponseBody, ResponseStatus};
		file_not_valid  ->
			ResponseBody = <<"Please upload an image of your choice. Accepted formats are jpeg, png, and gif.">>,
			ResponseStatus = 415,
			{ResponseBody, ResponseStatus};
		adj_not_valid  ->
			ResponseBody = <<"Please enter two adjectives of your choice">>,
			ResponseStatus = 415,
			{ResponseBody, ResponseStatus};
		other_error  ->
			ResponseBody = <<"There was an unknown error. Please try again">>,
			ResponseStatus = 415,
			{ResponseBody, ResponseStatus}

	end;

upload_processor(_, _, _) ->
	ResponseBody = <<"Not logged in">>,
	ResponseStatus = 400,
	{ResponseBody, ResponseStatus}.

%%check mime type and file extension and if they match
%%file_adj_validator(FileName, FileMime, is_mime_ok(FileMime), Adj1, Adj2) 
file_adj_validator(_, _, _, <<>>, _Adj2) ->
	erlang:display(adj1notvalid),
	adj_not_valid;
file_adj_validator(_, _, _, _Adj1, <<>>) ->
	erlang:display(adj2notvalid),
	adj_not_valid;
file_adj_validator(FileName, FileMime, mime_is_ok, _, _) ->
	[_, FileExt] = binary:split(filename:extension(FileName), <<".">>),
	[_, MimeExt] = binary:split(FileMime, <<"/">>),

	case extension_match(MimeExt, string:lowercase(FileExt)) of 
		true ->
			file_valid;
		false ->
			file_not_valid
	end;
file_adj_validator(_FileName, _FileMime, mime_not_ok, _, _) ->
	file_not_valid;
file_adj_validator(_, _, _, _Adj1, _Adj2) ->
	other_error.

%%check if the extension of filename matches that derived from magic mimetype 
extension_match(<<"jpeg">>, <<"jpg">>) ->
	true;
extension_match(MimeExt, FileExt) ->
	MimeExt == FileExt.

%% check if mime type of uploaded file is acceptable
is_mime_ok(Mime) ->
	AcceptableMimes = [<<"image/jpeg">>, <<"image/gif">>, <<"image/png">> ],
	case lists:member(Mime, AcceptableMimes) of
		true ->
			mime_is_ok;
		false ->
			mime_not_ok
	end.

%%extract the File data, file name and adjectives from incoming req
extract_file_and_adjs(Req) ->
	{ok, Headers, Req2} = cowboy_req:read_part(Req),
	
	%DataFile is the file data
	%erlang:display(cowboy_req:read_part_body(Req2)),
	{ok, DataFile, Req3} = cowboy_req:read_part_body(Req2),
	%io:format("DATA0 ~n ~p~n", [DataFile]),

	{file, <<"inputfile">>, FileName, FileSize} = case DataFile == <<"undefined">> of
		true ->
			{file, <<"inputfile">>, file_name, foo};
		false ->
			FileSize1 = byte_size(DataFile),
			%erlang:display(cow_multipart:form_data(Headers)),
			{file, <<"inputfile">>, FileName1, _} = cow_multipart:form_data(Headers),
			{file, <<"inputfile">>, FileName1, FileSize1}
	end,

%	FileSize = byte_size(DataFile),
%	erlang:display(cow_multipart:form_data(Headers)),
%	{file, <<"inputfile">>, FileName, _} = cow_multipart:form_data(Headers),
%	erlang:display(FileName),

%add checks to be sure Headers are what they should be. prevent tampered requests. 
	{ok, _Headers1, Req4} = cowboy_req:read_part(Req3),
	{ok, DataAdj1, Req5} = cowboy_req:read_part_body(Req4),

	{ok, _Headers2, Req6} = cowboy_req:read_part(Req5),
	{ok, DataAdj2, _Req7} = cowboy_req:read_part_body(Req6),

%	{ok, Headers3, Req8} = cowboy_req:read_part(Req7),
%	{ok, Data3, Req9} = cowboy_req:read_part_body(Req8),

	{DataFile, FileName, FileSize, DataAdj1, DataAdj2}.

%%get hash and convert it to printable ascii
%%https://gist.github.com/dch/bb33330a6d68b8149103
get_hash(<<CryptoHash:20/big-unsigned-integer-unit:8>>) ->
	lists:flatten(io_lib:format("~40.16.0b", [CryptoHash])).


%%prepend timestamp to the uploaded file name
create_file_name(FileName, FileHash) ->
	Name = binary:bin_to_list(FileName),
	FileExt = filename:extension(Name),
	% TODO: get suffix from mime type, not submitted extension. 
	FinalFileName = lists:concat([FileHash, FileExt]),

	ImageDir = "/usr/home/yc/vm_shared_dir/demo/jquery_mobile/test1/images",
	TempDir = "/usr/home/yc/vm_shared_dir/temp/uploaded/staging",
	UriDir= "/images",
	Slash = "/",
	{{YYYY, MM, DD},{_H,_M,_S}} = calendar:local_time(),
	Year = integer_to_list(YYYY),
	Month = integer_to_list(MM),
	Date = integer_to_list(DD),

	UriPath = general_helpers:list_merge_no_sort([UriDir, Slash, Year, Slash, Month, Slash, Date, Slash]),
	ImageUri = lists:concat([UriPath, FinalFileName]),
	FileDirPath = general_helpers:list_merge_no_sort([ImageDir, Slash, Year, Slash, Month, Slash, Date, Slash]),
	FileNameWithPath = lists:concat([FileDirPath, FinalFileName]),
	TempPath = general_helpers:list_merge_no_sort([TempDir, Slash, Date, Slash]),
	TempFile = lists:concat([TempPath, FinalFileName]),

	ok = filelib:ensure_dir(FileDirPath),
	ok = filelib:ensure_dir(TempPath),
	{FinalFileName, FileNameWithPath, TempFile, ImageUri}.

%%if filesize is okay, store file (in filesystem) and create response
file_storage(FileSize, MaxFileSize, FinalFileName, FileNameWithPath, TempFile,  FileData, User, ImageUri, Adj1, Adj2) when (FileSize < MaxFileSize)  ->
	{ok, WriteHandle} = file:open(TempFile, write),
	io:format(WriteHandle, "~s", [FileData]),
	% consider opening file in raw mode using file:write (faster)
	file:close(WriteHandle),
	{_NewImageId, _Adj1Text, _Adj2Text} = db_helpers:image_details_to_db(User, FinalFileName, ImageUri, FileNameWithPath, Adj1, Adj2),
	Body = <<"File uploaded successfully">>,
	Status = 200,
	{Body, Status};

file_storage(FileSize, MaxFileSize, _FinalFileName, _FileNameWithPath, _TempFile, _FileData, _User, _ImageUri, _Adj1, _Adj2) when (FileSize > MaxFileSize)  ->
	Body = <<"File too large">>,
	Status = 500,
	{Body, Status};

file_storage(_FileSize, _MaxFileSize, _FinalFileName, _FileNameWithPath, _TempFile, _FileData, _User, _ImageUri, _Adj1, _Adj2) ->
	Body = <<"Unknown error">>,
	Status = 500,
	{Body, Status}.

