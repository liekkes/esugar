-module(esugar_error).
-behaviour(esugar_monad).
-export(['>>='/2, return/1, fail/1]).


-export_type([error_m/1]).
-type error_m(A) :: ok | {ok, A} | {error, any()}.


-spec '>>='(error_m(A), fun((A) -> error_m(B))) -> error_m(B).
'>>='({error, _Err} = Error, _Fun) -> Error;
'>>='({ok, Result},           Fun) -> Fun(Result);
'>>='(ok,                     Fun) -> Fun(ok).


-spec return(A) -> error_m(A).
return(ok) -> ok;
return(X ) -> {ok, X}.


-spec fail(any()) -> error_m(_A).
fail(X) -> {error, X}.

