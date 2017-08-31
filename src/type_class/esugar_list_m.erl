-module(esugar_list_m).
-behaviour(esugar_monad).
-export(['>>='/2, return/1, fail/1]).
-behaviour(esugar_monad_plus).
-export([mzero/0, mplus/2]).


-spec '>>='([A], fun((A) -> [B])) -> [B].
'>>='(X, Fun) -> lists:append([Fun(E) || E <- X]).


-spec return(A) -> [A].
return(X) -> [X].


-spec fail(any()) -> [_A].
fail(_) -> [].


-spec mzero() -> [_A].
mzero() -> [].


-spec mplus([A], [A]) -> [A].
mplus(X, Y) ->
    lists:append(X, Y).

