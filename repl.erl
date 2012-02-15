-module(repl).
-import(eval).
-export([read/0,read/1,repl/0]).

count_parens([]) -> 0;
count_parens([$(|Rest]) -> count_parens(Rest) + 1;
count_parens([$)|Rest]) -> count_parens(Rest) - 1;
count_parens([_|Rest]) -> count_parens(Rest).

getline_balanced(Prompt, Acc) ->
    Total = Acc ++ io:get_line(Prompt),
    Parens = count_parens(Total),
    if Parens > 0 -> getline_balanced(Prompt ++ "   ", Total);
       Parens < 0 -> erlang:error("Too many closing parentheses");
       Parens =:= 0 -> lists:map(fun(C) ->
					 case C of
					     $\n -> $ ;
					     _ -> C
					 end end, Total)
    end.
 
read() -> read(getline_balanced("> ", "")).

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
	{error, _} ->
	    I = string:to_integer(X),
	    case I of
		{error, _} ->
		    list_to_atom(X);
		_ -> {IRet, _} = I,
		     IRet
	    end;
	_ -> {FRet, _} = F,
	     FRet
    end.

evalloop([], Env) -> Env;
evalloop(L, Env) when is_list(L) ->
    [Expr|Rest] = L,
    case Expr of
	[] -> Env;
	_ ->
	    Sanitized = lists:map(fun sanitize/1, Expr),
	    {_, NewEnv} = eval:evaluate(Sanitized, Env),
	    evalloop(Rest, NewEnv)
    end.

readloop(Env) ->
    Exprs = read(),
    NewEnv = evalloop(Exprs, Env),
    readloop(NewEnv).

repl() ->
    io:format("Take this REPL, brother; may it serve you well.~n"),
    readloop(eval:env()).
    
