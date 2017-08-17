-module(esugar_pipe_transform).
-export([parse_transform/2]).


parse_transform(AST, _Options) ->
    walk_ast(AST).


walk_ast({call, Line, {atom, _, pipe@}, [{op, _, '!', First, Second}]}) ->
    do_transform(Line, First, Second);
walk_ast({call, _Line, {atom, _, pipe@}, [First]}) ->
    First;
walk_ast(AST) when is_tuple(AST) ->
    erlang:list_to_tuple(walk_ast(erlang:tuple_to_list(AST)));
walk_ast(AST) when is_list(AST) ->
    [walk_ast(E) || E <- AST];
walk_ast(AST) -> AST.


do_transform(Line, First, {op, _, '!', {call, _, Func, Args}, Second}) ->
    do_transform(Line, {call, Line, Func, Args ++ [First]}, Second);
do_transform(Line, First, {op, _, '!', Func, Second}) ->
    do_transform(Line, {call, Line, Func, [First]}, Second);
do_transform(Line, First, {call, _, Func, Args}) ->
    {call, Line, Func, Args ++ [First]};
do_transform(Line, First, Func) ->
    {call, Line, Func, [First]}.

