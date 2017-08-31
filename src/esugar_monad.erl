-module(esugar_monad).
-export_type([monad/0, monadic/2]).
-export([join/2, sequence/2]).
-define(ID, fun id/1).


-type monad() :: module() | {module(), monad()}.
-type monadic(_M, _A) :: any().


-callback '>>='(esugar_monad:monadic(M, A), fun((A) -> esugar_monad:monadic(M, B))) ->
    esugar_monad:monadic(M, B) when M :: esugar_monad:monad().
-callback return(A) ->
    esugar_monad:monadic(M, A) when M :: esugar_monad:monad().
-callback fail(any()) ->
    esugar_monad:monadic(M, _A) when M :: esugar_monad:monad().


-spec join(M, esugar_monad:monadic(M, esugar_monad:monadic(M, A))) ->
    esugar_monad:monadic(M, A) when M :: esugar_monad:monad().
join(Monad, X) -> Monad:'>>='(X, ?ID).


-spec sequence(M, [esugar_monad:monadic(M, A)]) ->
    esugar_monad:monadic(M, [A]) when M :: esugar_monad:monad().
sequence(Monad, Xs) ->
    Monad:return([Monad:'>>='(X, ?ID) || X <- Xs]).


-spec id(A) -> A.
id(X) -> X.

