-module(esugar_monad_plus).
-compile({parse_transform, esugar_do_transform}).
-export([guard/2, msum/2, mfilter/3]).


-callback mzero() ->
    esugar_monad:monadic(_M, _A).
-callback mplus(esugar_monad:monadic(M, A), esugar_monad:monadic(M, A)) ->
    esugar_monad:monadic(M, A).


-spec guard(M, boolean()) ->
    esugar_monad:monadic(M, ok).
guard(Monad, true)  -> Monad:return(ok);
guard(Monad, false) -> Monad:fail("").


-spec msum(M, [esugar_monad:monadic(M, A)]) ->
    esugar_monad:monadic(M, A).
msum(Monad, List) ->
    lists:foldr(fun Monad:mplus/2, Monad:mzero(), List).


-spec mfilter(M, fun((A) -> boolean()), esugar_monad:monadic(M, A)) ->
    esugar_monad:monadic(M, A).
mfilter(Monad, Pred, X) ->
    do([Monad || A <- X,
        case Pred(A) of
        true  -> return(A);
        false -> Monad:mzero()
        end]).
