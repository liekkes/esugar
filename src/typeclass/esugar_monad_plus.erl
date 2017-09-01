-module(esugar_monad_plus).
-extends(esugar_monad).
-export_type([monad_plus/0, monad_plus/2]).
-compile({parse_transform, esugar_do_transform}).
-export([guard/2, msum/2, mfilter/3]).


-type monad_plus() :: module() | {module(), monad_plus()}.
-type monad_plus(_M, _A) :: any().


-callback mzero() ->
    monad_plus(M, _A) when M :: monad_plus().
-callback mplus(monad_plus(M, A), monad_plus(M, A)) ->
    monad_plus(M, A) when M :: monad_plus().


-spec guard(M, boolean()) ->
    monad_plus(M, ok).
guard(Monad, true)  -> Monad:return(ok);
guard(Monad, false) -> Monad:fail("").


-spec msum(M, [monad_plus(M, A)]) ->
    monad_plus(M, A).
msum(Monad, List) ->
    lists:foldr(fun Monad:mplus/2, Monad:mzero(), List).


-spec mfilter(M, fun((A) -> boolean()), monad_plus(M, A)) ->
    monad_plus(M, A).
mfilter(Monad, Pred, X) ->
    do([Monad || A <- X,
        case Pred(A) of
        true  -> return(A);
        false -> Monad:mzero()
        end]).
