-module(esugar_monad).
-extends(esugar_applicative).
-export_type([monad/0, monad/2]).
-export([join/2
	, sequence/2]).
-define(ID, fun id/1).


-type monad() :: module().
-type monad(_M, _A) :: any().


-callback '>>='(monad(M, A), fun((A) -> monad(M, B))) ->
    monad(M, B) when M :: monad().
-callback return(A) ->
    monad(M, A) when M :: monad().
-callback fail(any()) ->
    monad(M, _A) when M :: monad().


-spec join(M, monad(M, monad(M, A))) ->
    monad(M, A) when M :: monad().
join(Monad, X) -> Monad:'>>='(X, ?ID).


-spec sequence(M, [monad(M, A)]) ->
    monad(M, [A]) when M :: monad().
sequence(Monad, Xs) ->
    Monad:return([Monad:'>>='(X, ?ID) || X <- Xs]).


-spec id(A) -> A.
id(X) -> X.

