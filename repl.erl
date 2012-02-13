-module(repl).
-import(eval).
-export([read/0,read/1,repl/0]).

dbg(Msg) -> io:format("~s~n", [Msg]).

read() -> read(io:get_line("> ")).

read(S) -> read(hd(string:tokens(S,"\r\n")), "", []).

read([], TokenAcc, ListAcc) -> lists:reverse(finish(TokenAcc, ListAcc));
read([$ |Rest], TokenAcc, ListAcc) -> read(Rest, "", finish(TokenAcc, ListAcc));
read([$(|Rest], TokenAcc, ListAcc) ->
    List = finish(TokenAcc, ListAcc),
    {Remainder, Sublist} = readsub(Rest, "", []),
    read(Remainder, "", [Sublist|List]);
read([C|Rest], TokenAcc, ListAcc) -> read(Rest, [C|TokenAcc], ListAcc).

readsub([], TokenAcc, ListAcc) -> erlang:error("Unmatched '(': ~p~n", {TokenAcc, ListAcc});
readsub([$)|Rest], TokenAcc, ListAcc) -> {Rest, lists:reverse(finish(TokenAcc, ListAcc))};
readsub([$ |Rest], TokenAcc, ListAcc) -> readsub(Rest, "", finish(TokenAcc, ListAcc));
readsub([$(|Rest], TokenAcc, ListAcc) ->
    List = finish(TokenAcc, ListAcc),
    {Remainder, Sublist} = readsub(Rest, "", []),
    readsub(Remainder, "", [Sublist|List]);
readsub([C|Rest], TokenAcc, ListAcc) -> readsub(Rest, [C|TokenAcc], ListAcc).


finish(TokenAcc, ListAcc) ->
    case TokenAcc of
	[] -> ListAcc;
	_ -> [lists:reverse(TokenAcc)|ListAcc]
    end.

sanitize(X) when is_list(X), is_list(hd(X)) ->
    lists:map(fun sanitize/1, X);
sanitize(X) ->
    F = string:to_float(X),
    case F of
	{error, no_float} ->
	    I = string:to_integer(X),
	    case I of
		{error, no_integer} ->
		    list_to_atom(X);
		_ -> {IRet, _} = I,
		     IRet
	    end;
	_ -> {FRet, _} = F,
	     FRet
    end.

evalloop([]) -> done;
evalloop(L) when is_list(L) ->
    [Expr|Rest] = L,
    case Expr of
	[] -> done;
	_ ->
	    Sanitized = lists:map(fun sanitize/1, Expr),
	    eval:evaluate(Sanitized),
	    evalloop(Rest)
    end.

readloop() ->
    Exprs = read(),
    evalloop(Exprs),
    readloop().

repl() ->
    io:format("Take this REPL, brother; may it serve you well.~n"),
    readloop().
    
