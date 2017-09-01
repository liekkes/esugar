-module(esugar_monad).
-compile({parse_transform, esugar_do_transform}).
-extends(esugar_applicative).
-export_type([monad/0, monad/2]).
-export([join/2
    , sequence/2]).
-define(ID, fun id/1).


-type monad() :: module() | {module(), monad()}.
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
% join(Monad, X) -> do([Monad || Y <- X, Y]).


-spec sequence(M, [monad(M, A)]) ->
    monad(M, [A]) when M :: monad().
sequence(Monad, Xs) ->
    lists:foldr(fun(X, Rs) ->
        do([Monad || C <- X, Cs <- Rs, return([C | Cs])])
    end, Monad:return([]), Xs).


-spec id(A) -> A.
id(X) -> X.

