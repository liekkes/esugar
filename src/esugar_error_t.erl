-module(esugar_error_t).
-compile({parse_transform, esugar_do_transform}).
-behaviour(esugar_monad_trans).
-export([new/1, '>>='/3, return/2, fail/2, run/2, lift/2]).


-export_type([error_t/2]).
-opaque error_t(M, A) :: esugar_monad:monadic(M, ok | {ok, A} | {error, any()}).


-spec new(M) -> TM when TM :: esugar_monad:monad(), M :: esugar_monad:monad().
new(M) ->
    {?MODULE, M}.


-spec '>>='(error_t(M, A), fun( (A) -> error_t(M, B) ), M) -> error_t(M, B).
'>>='(X, Fun, {?MODULE, M}) ->
    do([M || R <- X,
             case R of
                 {error, _Err} = Error -> return(Error);
                 {ok,  Result}         -> Fun(Result);
                 ok                    -> Fun(ok)
             end
       ]).


-spec return(A, M) -> error_t(M, A).
return(ok, {?MODULE, M}) -> M:return(ok);
return(X , {?MODULE, M}) -> M:return({ok, X}).

%% This is the equivalent of
%%     fail msg = ErrorT $ return (Left (strMsg msg))
%% from the instance (Monad m, Error e) => Monad (ErrorT e m)
%%
%% http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/doc/html/src/Control-Monad-Error.html#ErrorT
%%
%% I.e. note that calling fail on the outer monad is not a failure of
%% the inner esugar_monad: it is success of the inner monad, but the failure
%% is encapsulated.
-spec fail(any(), M) -> error_t(M, _A).
fail(E, {?MODULE, M}) ->
    M:return({error, E}).


-spec run(error_t(M, A), M) -> esugar_monad:monadic(M, ok | {ok, A} | {error, any()}).
run(EM, _M) -> EM.


-spec lift(esugar_monad:monadic(M, A), M) -> error_t(M, A).
lift(X, {?MODULE, M}) ->
    do([M || A <- X,
             return({ok, A})]).
