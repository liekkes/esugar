-module(esugar_omega_m).
-behaviour(esugar_monad).
-export(['>>='/2, return/1, fail/1]).
-behaviour(esugar_monad_plus).
-export([mzero/0, mplus/2]).
-export([diagonal/1]).


-spec '>>='([A], fun((A) -> [B])) -> [B].
'>>='(X, Fun) ->
    diagonal([Fun(E) || E <- X]).


-spec return(A) -> [A].
return(X) -> [X].


-spec fail(any()) -> [_A].
fail(_) -> [].


-spec mzero() -> [_A].
mzero() -> [].


-spec mplus([A], [A]) -> [A].
mplus(X, Y) ->
    lists:append(X, Y).


%% [[a, b, c, d],
%%  [e, f, g, h],
%%  [i, j, k, l],
%%  [m, n, o, p]].
%%
%% diagonal traverses diagonally from north-west corner, heading east
%% then south-west. I.e.
%% [a, b, e, c, f, i, d, g, j, m, h, k, n, l, o, p]
-spec diagonal([[A]]) -> [A].
diagonal(LoL) -> lists:append(stripe(LoL)).

stripe([])           -> [];
stripe([[]|Xss])     -> stripe(Xss);
stripe([[X|Xs]|Xss]) -> [[X] | zip_cons(Xs, stripe(Xss))].

zip_cons([], Ys)         -> Ys;
zip_cons(Xs, [])         -> [[X] || X <- Xs];
zip_cons([X|Xs], [Y|Ys]) -> [[X|Y] | zip_cons(Xs, Ys)].
