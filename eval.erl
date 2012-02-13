-module(eval).
-export([evaluate/1]).

%% Built-in functions
function(car) -> fun(L) when is_list(L) -> hd(L) end;
function(cdr) -> fun(L) when is_list(L) -> tl(L) end;
function(add) -> fun(X,Y) -> X + Y end;
function(sub) -> fun(X,Y) -> X - Y end;
function(mul) -> fun(X,Y) -> X * Y end;
function(print) -> fun(X) -> io:format("~p~n", [X]) end;
function(_) -> undefined.

%% Note that we do some special forms in evaluate.  This is because control-flow-like constructs
%% (quote, if, ...) can't be functions
%% TODO: cond is more primitive than ifm.  Ifm can be a macro
evaluate([]) ->
    [];
evaluate([quote|T]) -> [R] = T, R;
evaluate([atom|[[]]]) -> t;
evaluate([atom|T]) ->
    [R] = T,
    case is_list(R) of
	true -> [];
	false -> t
    end;
evaluate([ifm,Test,If,Else]) ->
    case evaluate(Test) of
	true -> evaluate(If);
	false -> evaluate(Else)
    end;
evaluate([ifm,Test,If]) ->
    case evaluate(Test) of
	true -> evaluate(If);
	false -> []
    end;
evaluate([F|Args]) -> erlang:apply(evaluate(F), lists:map(fun evaluate/1, Args));
evaluate(X) when is_float(X) -> X;
evaluate(X) when is_integer(X) -> X;
evaluate(X) when is_atom(X) ->
    case function(X) of
	undefined -> X;
	F -> F
    end;
evaluate(X) -> X.
