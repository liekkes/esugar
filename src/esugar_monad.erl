-module(esugar_monad).
-compile({parse_transform, esugar_do_transform}).
-export([join/2, sequence/2]).


-export_type([monad/0, monadic/2]).
-type monad() :: module() | {module(), monad()}.
-type monadic(_M, _A) :: any().


-callback '>>='(esugar_monad:monadic(M, A), fun((A) -> esugar_monad:monadic(M, B))) ->
    esugar_monad:monadic(M, B) when M :: esugar_monad:monad().
-callback return(A) ->
    esugar_monad:monadic(M, A) when M :: esugar_monad:monad().
-callback fail(any()) ->
    esugar_monad:monadic(M, _A) when M :: esugar_monad:monad().


-spec join(M, esugar_monad:monadic(M, esugar_monad:monadic(M, A))) ->
    esugar_monad:monadic(M, A).
join(Monad, X) -> do([Monad || Y <- X, Y]).


-spec sequence(M, [esugar_monad:monadic(M, A)]) ->
    esugar_monad:monadic(M, [A]).
sequence(Monad, Xs) -> sequence(Monad, Xs, []).

sequence(Monad, [], Acc) ->
    do([Monad || return(lists:reverse(Acc))]);
sequence(Monad, [X | Xs], Acc) ->
    do([Monad || E <- X, sequence(Monad, Xs, [E | Acc])]).

