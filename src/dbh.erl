-module(dbh).

-export([token_sent_date/1]). 

token_sent_date(Id) -> 
	{{update, 1}, _} = pp_db:extended_query("
			update person_property_acquisition_organic 
			set date_token_sent = current_timestamp
			where property_of = $1
			returning id
		", [Id]).
