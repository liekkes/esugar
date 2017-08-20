-module(esugar_bind_transform).
-export([parse_transform/2]).


parse_transform(AST, _Options) ->
    walk_ast(AST, []).


walk_ast({call, Line, {atom, _, do}, [{lc, _, Monad, Qs}]}, MonadStack) ->
    {call, Line, {'fun', Line, {clauses, [{clause, Line, [], []
                        , do_transform(Qs, [Monad | MonadStack])}]}}, []};
walk_ast({call, Line, {atom, _, return} = Return, As}, [Monad | _] = MonadStack) ->
    {call, Line, {remote, Line, Monad, Return}, walk_ast(As, MonadStack)};
walk_ast({call, Line, {atom, _, fail} = Fail, As}, [Monad | _] = MonadStack) ->
    {call, Line, {remote, Line, Monad, Fail}, walk_ast(As, MonadStack)};
walk_ast(AST, MonadStack) when is_tuple(AST) ->
    erlang:list_to_tuple(walk_ast(erlang:tuple_to_list(AST), MonadStack));
walk_ast(AST, MonadStack) when is_list(AST) ->
    [walk_ast(E, MonadStack) || E <- AST];
walk_ast(AST, _MonadStack) -> AST.


do_transform([{generate, Line, {var, _, _} = Pattern, E} | Es], [Monad | _] = MonadStack) ->
    [{call, Line, {remote, Line, Monad, {atom, Line, '>>='}}, [walk_ast(E, MonadStack),
        {'fun', Line, {clauses, [{clause, Line, [Pattern], [], do_transform(Es, MonadStack)}]}}]}];
do_transform([{generate, Line, Pattern, E} | Es], [Monad | _] = MonadStack) ->
    [{call, Line, {remote, Line, Monad, {atom, Line, '>>='}}, [walk_ast(E, MonadStack),
        {'fun', Line, {clauses, [
            {clause, Line, [Pattern], [], do_transform(Es, MonadStack)},
            {clause, Line, [{var, Line, '_'}], [], [{call, Line,
                {remote, Line, Monad, {atom, Line, fail}}, [{atom, Line, monad_badmatch}]}]}]}}]}];
do_transform([E], MonadStack) ->
    [walk_ast(E, MonadStack)];
do_transform([{match, _, _, _} = E | Es], MonadStack) ->
    [walk_ast(E, MonadStack) | do_transform(Es, MonadStack)];
do_transform([E | Es], [Monad | _] = MonadStack) ->
    Line = erlang:element(2, E),
    [{call, Line, {remote, Line, Monad, {atom, Line, '>>='}}, [walk_ast(E, MonadStack),
        {'fun', Line, {clauses, [
            {clause, Line, [{var, Line, '_'}], [], do_transform(Es, MonadStack)}]}}]}].

