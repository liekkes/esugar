-module(esugar_identity_m).
-behaviour(esugar_monad).
-export(['>>='/2, return/1, fail/1]).


-export_type([identity_m/1]).
-type identity_m(A) :: A.


-spec '>>='(identity_m(A), fun((A) -> identity_m(B))) -> identity_m(B).
'>>='(X, Fun) -> Fun(X).


-spec return(A) -> identity_m(A).
return(X) -> X.


-spec fail(any()) -> identity_m(_A).
fail(E) ->
    throw({error, E}).
