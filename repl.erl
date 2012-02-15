-module(repl).
-import(eval).
-export([read/0,read/1,repl/0]).

%% Counts the number of parens in a string.  If 0, there are no parens or the string is balanced
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

%% Public read entrypoint
read() -> read(getline_balanced("> ", "")).

%% Read the given string and return a list of tokens
read(S) -> read(hd(string:tokens(S,"\r\n")), "", []).

%% Tail-recursive version of read
read([], TokenAcc, ListAcc) -> lists:reverse(finish(TokenAcc, ListAcc));
read([$ |Rest], TokenAcc, ListAcc) -> read(Rest, "", finish(TokenAcc, ListAcc));
read([$(|Rest], TokenAcc, ListAcc) ->
    List = finish(TokenAcc, ListAcc),
    {Remainder, Sublist} = readsub(Rest, "", []),
    read(Remainder, "", [Sublist|List]);
read([C|Rest], TokenAcc, ListAcc) -> read(Rest, [C|TokenAcc], ListAcc).

%% Reads sub-expressions (in parenthesis).  This is a HACK
readsub([], TokenAcc, ListAcc) -> erlang:error("Unmatched '(': ~p~n", {TokenAcc, ListAcc});
readsub([$)|Rest], TokenAcc, ListAcc) -> {Rest, lists:reverse(finish(TokenAcc, ListAcc))};
readsub([$ |Rest], TokenAcc, ListAcc) -> readsub(Rest, "", finish(TokenAcc, ListAcc));
readsub([$(|Rest], TokenAcc, ListAcc) ->
    List = finish(TokenAcc, ListAcc),
    {Remainder, Sublist} = readsub(Rest, "", []),
    readsub(Remainder, "", [Sublist|List]);
readsub([C|Rest], TokenAcc, ListAcc) -> readsub(Rest, [C|TokenAcc], ListAcc).

%% Finish the current token
finish(TokenAcc, ListAcc) ->
    case TokenAcc of
	[] -> ListAcc;
	_ -> [lists:reverse(TokenAcc)|ListAcc]
    end.

%% Turn strings into atoms, ints, floats... whatever is appropriate
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

%% Evaluate the sanitized token list
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

%% Lather, rinse, repeat
readloop(Env) ->
    Exprs = read(),
    NewEnv = evalloop(Exprs, Env),
    readloop(NewEnv).

%% Start the REPL and be snarky about it
repl() ->
    io:format("Take this REPL, brother; may it serve you well.~n"),
    readloop(eval:env()).
    
