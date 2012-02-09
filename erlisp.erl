-module(erlisp).
-compile(export_all).

lexer(Receiver, Input, Accumulator) ->
    receive
	next_token_plz ->
	    [Ch|Rest] = Input,
	    case Ch of
		32 ->
		    Receiver ! {can_haz_token, whitespace};
		$\t ->
		    Receiver ! {can_haz_token, whitespace};
		$\n ->
		    Receiver ! {can_haz_token, eol};
		_ ->
		    Receiver ! {can_haz_token, token, Ch}
	    end,
	    lexer(Receiver, Rest, Accumulator);
	kthxbye ->
	    okay
    end.
		    

builder(Receiver) ->
    Input = io:get_line([]),
    Lexer = spawn(erlisp, lexer, [self(), Input, []]),
    builder(Input, Lexer, Receiver).

builder(Input, Lexer, Receiver) ->
    receive
	expression_plz ->
	    Lexer ! next_token_plz,
	    builder(Input, Lexer, Receiver);
	{can_haz_token, whitespace} ->
	    self() ! expression_plz,
	    builder(Input, Lexer, Receiver);
	{can_haz_token, eol} ->
	    Receiver ! {evaluate_plz, {eol}},
	    self() ! kthxbye,
	    builder(Input, Lexer, Receiver);
	{can_haz_token, token, Ch} ->
	    Receiver ! {evaluate_plz, {symbol, Ch}},
	    builder(Input, Lexer, Receiver);
	kthxbye ->
	    Lexer ! kthxbye,
	    okay
    end.

evaluator() ->
    Builder = spawn(erlisp, builder, [self()]),
    evaluator(Builder).

evaluator(Builder) ->
    Builder ! expression_plz,
    receive
	{evaluate_plz, {symbol, X}} ->
	    io:format("Would evaluate ~s~n", [[X]]),
	    evaluator(Builder);
	{evaluate_plz, {eol}} ->
	    self() ! kthxbye,
	    evaluator(Builder);
	kthxbye ->
	    Builder ! kthxbye,
	    okay
    end.

eval() ->
    Result = evaluator(),
    case Result of
	% TODO: A special end signal would be nice
	_ -> eval()
    end.

