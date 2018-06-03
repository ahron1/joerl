%%% Created : 02 May 2014 by Tristan Sloughter <tristan@heroku.com>
%%% updated with new functions by AN
%%%-------------------------------------------------------------------
-module(pp_db).

-export([open/1
        ,close/1
        ,get_connection/1
        ,return_connection/2
		,simple_query/1
		,param_query/2
        ,extended_query/2]).

-spec get_connection(atom()) -> {pgsql_connection, pid()} | {error, timeout}.
get_connection(Pool) ->    
    case episcina:get_connection(Pool) of
        {ok, Pid} ->
            {pgsql_connection, Pid};
        {error, timeout} ->
            {error, timeout}
    end.

-spec return_connection(atom(), {pgsql_connection, pid()}) -> ok.
return_connection(Pool, {pgsql_connection, Pid}) ->
    episcina:return_connection(Pool, Pid).

-spec open(list()) -> {ok, pid()}.
open(DBArgs) ->    
    {pgsql_connection, Pid} = pgsql_connection:open(DBArgs),
    {ok, Pid}.

-spec close(pid()) -> ok.
close(Pid) ->
    pgsql_connection:close({pgsql_connection, Pid}).

%-spec simple_query(string()) -> tuple().
simple_query(Query) ->
	X = pgo:query(Query),
	[Command, NumRows, Result] = maps:values(X),
	{{Command, NumRows}, Result}.

%    C = get_connection(primary),
%	erlang:display(C),
%    try
%        pgsql_connection:simple_query(Query, [], infinity, C)    
%    after
%        return_connection(primary, C)
%    end.
%
%-spec extended_query(string(), list()) -> tuple().
extended_query(Query, Param_list) ->
	X = pgo:query(Query, Param_list),
	[Command, NumRows, Result] = maps:values(X),
	{{Command, NumRows}, Result}.

%    C = get_connection(primary),
%    try
%        pgsql_connection:extended_query(Query, Param_list, C)    
%    after
%        return_connection(primary, C)
%    end.
%
-spec param_query(string(), list()) -> tuple().
param_query(Query, Param_list) ->
    C = get_connection(primary),
    try
        pgsql_connection:param_query(Query, Param_list, C)    
    after
        return_connection(primary, C)
    end.
