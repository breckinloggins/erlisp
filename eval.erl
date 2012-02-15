-module(eval).
-compile(export_all).
%%-export([evaluate/1]).

%% Built-in functions
function(car) -> fun(L) when is_list(L) -> hd(L) end;
function(cdr) -> fun(L) when is_list(L) -> tl(L) end;
function(add) -> fun(X,Y) -> X + Y end;
function(sub) -> fun(X,Y) -> X - Y end;
function(mul) -> fun(X,Y) -> X * Y end;
function(print) -> fun(X) -> io:format("~p~n", [X]) end;
function(_) -> undefined.

env() -> [[],[]].
env(Parent) -> [[], Parent].

env_insert(E, K, V) ->
    [Env, Parent] = E,
    [[{K,V}|Env],Parent].

env_lookup(E, K) ->
    case lists:keyfind(K, 1, hd(E)) of
	{_,V} -> V;
	false ->
	    case tl(E) =:= [[]] of
		true -> false;
		false -> env_lookup(hd(tl(E)),K)
	    end
    end.

replace([],_) -> [];
replace(X,[]) -> X;
replace(E,{S,D}) when not is_list(E) ->
    case E of
	S -> D;
	_ -> E
    end;
replace(E,[{S,D}|Rest]) when not is_list(e) ->
    Replaced = replace(E, {S,D}),
    replace(Replaced, Rest);
replace(L,{S,D}) when is_list(L) ->
    F = replace(hd(L), {S,D}),
    T = replace(tl(L), {S,D}),
    case T =:= [] of
	true -> [F];
	false -> [F|T]
    end.

%% Note that we do some special forms in evaluate.  This is because control-flow-like constructs
%% (quote, if, ...) can't be functions

%% t always evaluates to itself
evaluate(t, Env) -> {t, Env};

%% Evaluate primitives, including atom environment lookup
evaluate(X, Env) when is_float(X) -> {X, Env};
evaluate(X, Env) when is_integer(X) -> {X, Env};
evaluate(X, Env) when is_atom(X) ->
    case function(X) of
	undefined ->
	    case env_lookup(Env, X) of
		false -> erlang:error(string:concat("Symbol is unbound in this context: ", atom_to_list(X)));
		V -> {V, Env}
	    end;
	F -> {F, Env}
    end;

%% The empty list always evaluates to itself
evaluate([], Env) -> {[], Env};

%% Quote the given arguments unevaluated
evaluate([quote|T], Env) -> [R] = T, {R, Env};

%% Return t if the argument is an atom or the empty list, [] otherwise
evaluate([atom|[[]]], Env) -> {t, Env};
evaluate([atom|T], Env) ->
    [R] = T,
    case is_list(R) of
	true -> {[], Env};
	false -> {t, Env}
    end;

%% Return t if two atoms are the same or if the empty list is compared to itself, [] otherwise
evaluate([eq,A,A], Env) when not is_list(A) -> {t, Env};
evaluate([eq,[],[]], Env) -> {t, Env};
evaluate([eq|_], Env) -> {[], Env};

%% Concatenate the evaluated Head to the evaluated Tail.  Returned value may be a proper or
%% improper list
evaluate([cons,H,T], Env) -> 
    {EvalH, _} = evaluate(H, Env),
    {EvalT, _} = evaluate(T, Env),
    {[EvalH|EvalT], Env};

%% Encode lambda expressions
evaluate([lambda,Args,Def], Env) -> {{lambda,Args,Def}, Env};

%% Debug special form to print out the environment
evaluate([env], Env) -> io:format("~p~n", [Env]), {t, Env};

%% Performs a series of expressions in order (primarily for their side effects)
%% The return value is the value of the last expression
evaluate([do], Env) -> {[], Env};
evaluate([do,Expr|Exprs], Env) ->
    {Result, NewEnv} = evaluate(Expr, Env),
    case Exprs of
	[] -> {Result, NewEnv};
	_ -> evaluate([do|Exprs], NewEnv)
    end;

%% Set the given atom to the result of the expression in the current environment
evaluate([set,Atom,Expr], Env) ->
    case is_atom(Atom) of
	false -> erlang:error("Argument to set must be an atom");
	true ->
	    {ExprResult, _} = evaluate(Expr, Env),
	    {t, env_insert(Env, Atom, ExprResult)}
    end;

%% Define a function with the given name, args, and definition
evaluate([defun,Name,Args,Def], Env) ->
    case is_atom(Name) of
	false -> erlang:error("Function name in defun must be an atom");
	true -> evaluate([set,Name,[lambda,Args,Def]], Env)
    end;

%% For each (p_i e_i), evaluate p_i, if it returns t, evaluate e_i, else continue
%% to (p_i+1, e_i+1) or return [] if no more pairs
evaluate(['cond'], Env) -> {[], Env};
evaluate(['cond',Term], _) when is_atom(Term) ->
    erlang:error(string:concat("COND clause is not a list: ", atom_to_list(Term)));
evaluate(['cond',[t]], Env) -> {t, Env};
evaluate(['cond',[]], Env) -> {[], Env};
evaluate(['cond',[P,E]], Env) ->
    case evaluate(P, Env) of
	{t, _} -> evaluate(E, Env);
	_ -> {[], Env}
    end;
evaluate(['cond',[P,E]|XS], Env) ->
    io:format("Matched it! ~p~n", [XS]),
    case evaluate(P, Env) of
	{t, _} -> evaluate(E, Env);
	_ -> evaluate(['cond'|XS], Env)
    end;	

%% Evaluate functional application of the form (f p1 p2 ... pn).  f can be an atom
%% that resolves to a function or a lambda expression, or a bare lambda expression itself
evaluate([X|Args], Env) ->
    case evaluate(X, Env) of
	{{lambda,Params,Def}, Env} ->
	    EvaledArgs = lists:map(fun(Arg) -> evaluate(Arg, Env) end, Args),
	    NoEnvArgs = lists:map(fun({A,_}) -> A end, EvaledArgs),
	    Replacements = case NoEnvArgs of
			       [] -> [];
			       _ -> lists:zip(Params, NoEnvArgs)
			   end,
	    ToEval = replace(Def, Replacements),
	    {Result, _} = evaluate(ToEval, env(Env)),
	    {Result, Env};
	{F, Env} -> 
	    EvaledArgs = lists:map(fun(Arg) -> evaluate(Arg, Env) end, Args),
	    NoEnvArgs = lists:map(fun({A,_}) -> A end, EvaledArgs),
	    Result = erlang:apply(F, NoEnvArgs),
	    {Result, Env}
    end;

%% Catch unrecognized constructions
evaluate(_,_) ->
    erlang:error("Unknown expression type").






