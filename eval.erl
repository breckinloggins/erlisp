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
%% TODO: cond is more primitive than ifm.  Ifm can be a macro


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
evaluate([], Env) -> {[], Env};
evaluate([quote|T], Env) -> [R] = T, {R, Env};
evaluate([atom|[[]]], Env) -> {t, Env};
evaluate([atom|T], Env) ->
    [R] = T,
    case is_list(R) of
	true -> {[], Env};
	false -> {t, Env}
    end;
evaluate([eq,A,A], Env) when not is_list(A) -> {t, Env};
evaluate([eq,[],[]], Env) -> {t, Env};
evaluate([eq|_], Env) -> {[], Env};
evaluate([lambda,Args,Def], Env) -> {{lambda,Args,Def}, Env};
evaluate([env], Env) -> io:format("~p~n", [Env]), {t, Env};
evaluate([set,Atom,Expr], Env) ->
    case is_atom(Atom) of
	false -> erlang:error("Argument to set must be an atom");
	true ->
	    {ExprResult, _} = evaluate(Expr, Env),
	    {t, env_insert(Env, Atom, ExprResult)}
    end;
evaluate([defun,Name,Args,Def], Env) ->
    case is_atom(Name) of
	false -> erlang:error("Function name in defun must be an atom");
	true -> evaluate([set,Name,[lambda,Args,Def]], Env)
    end;
evaluate([ifm,Test,If,Else], Env) ->
    case evaluate(Test, Env) of
	{t, NewEnv} -> evaluate(If, NewEnv);
	{[], NewEnv} -> evaluate(Else, NewEnv)
    end;
evaluate([ifm,Test,If], Env) ->
    case evaluate(Test, Env) of
	{t, NewEnv} -> evaluate(If, NewEnv);
	{[], NewEnv} -> {[], NewEnv}
    end;
evaluate([X|Args], Env) ->
    case evaluate(X, Env) of
	{{lambda,Params,Def}, NewEnv} ->
	    EvaledArgs = lists:map(fun(Arg) -> evaluate(Arg, NewEnv) end, Args),
	    NoEnvArgs = lists:map(fun({A,_}) -> A end, EvaledArgs),
	    Replacements = lists:zip(Params, NoEnvArgs),
	    ToEval = replace(Def, Replacements),
	    evaluate(ToEval, NewEnv);
	{F, NewEnv} -> 
	    EvaledArgs = lists:map(fun(Arg) -> evaluate(Arg, NewEnv) end, Args),
	    NoEnvArgs = lists:map(fun({A,_}) -> A end, EvaledArgs),
	    Result = erlang:apply(F, NoEnvArgs),
	    {Result, NewEnv}
    end;
evaluate(_,_) ->
    erlang:error("Unknown expression type").






