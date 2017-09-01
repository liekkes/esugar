-module(esugar_maybe).
-behaviour(esugar_monad).
-export(['>>='/2, return/1, fail/1]).
-behaviour(esugar_monad_plus).
-export([mzero/0, mplus/2]).


-export_type([maybe/1]).
-type maybe(A) :: {just, A} | nothing.


-spec '>>='(maybe(A), fun((A) -> maybe(B))) -> maybe(B).
'>>='({just, X}, Fun) -> Fun(X);
'>>='(nothing,  _Fun) -> nothing.


-spec return(A) -> maybe(A).
return(X) -> {just, X}.


-spec fail(any()) -> maybe(_A).
fail(_) -> nothing.


-spec mzero() -> maybe(_A).
mzero() -> nothing.


-spec mplus(maybe(A), maybe(A)) -> maybe(A).
mplus(nothing, Y) -> Y;
mplus(X,      _Y) -> X.
