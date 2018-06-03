-module(general_helpers).
-export([escape_html/1, list_merge_no_sort/1]).

-define(QUOTE, $\").

%% html escaping function based on mochiweb_html.erl. extended by AN.
escape_html(B) when is_binary(B) ->
    escape_html(binary_to_list(B), []);
escape_html(A) when is_atom(A) ->
    escape_html(atom_to_list(A), []);
escape_html(S) when is_list(S) ->
    escape_html(S, []).

escape_html([], Acc) ->
    list_to_binary(lists:reverse(Acc));
escape_html("<" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&lt;", Acc));
escape_html(">" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&gt;", Acc));
escape_html("&" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&amp;", Acc));
escape_html("'" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#x27;", Acc));
escape_html("/" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#x2F;", Acc));
escape_html("`" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#96;", Acc));
escape_html("!" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#33;", Acc));
escape_html("@" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#64;", Acc));
escape_html("$" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#36;", Acc));
escape_html("%" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#37;", Acc));
escape_html("(" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#40;", Acc));
escape_html(")" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#41;", Acc));
escape_html("=" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#61;", Acc));
escape_html("+" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#43;", Acc));
escape_html("{" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#123;", Acc));
escape_html("}" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#125;", Acc));
escape_html("[" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#91;", Acc));
escape_html("]" ++ Rest, Acc) ->
    escape_html(Rest, lists:reverse("&#93;", Acc));
escape_html([?QUOTE | Rest], Acc) ->
    escape_html(Rest, lists:reverse("&quot;", Acc));
escape_html([C | Rest], Acc) ->
    escape_html(Rest, [C | Acc]).

%%to do: html attribute escaping function. based on above.
%%not done because all current attributes are predefined/hardcoded. 


%%custom merge fun to merge lists without sorting them
list_merge_no_sort([H|[H1|T]]) ->
	list_merge_no_sort([lists:append(H, H1)|T]);
list_merge_no_sort([H|T]) ->
	lists:append(H,T).
