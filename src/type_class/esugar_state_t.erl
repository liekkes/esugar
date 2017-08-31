-module(esugar_state_t).
-compile({parse_transform, esugar_do_transform}).
-behaviour(esugar_monad_trans).
-export([new/1, '>>='/3, return/2, fail/2]).
-export([get/1, put/2, eval/3, exec/3, run/3,
         modify/2, modify_and_return/2, lift/2]).


-export_type([state_t/3]).
-opaque state_t(S, M, A) :: fun( (S) -> esugar_monad:monadic(M, {A, S}) ).


-spec new(M) -> TM when TM :: esugar_monad:monad(), M :: esugar_monad:monad().
new(M) ->
    {?MODULE, M}.


-spec '>>='(state_t(S, M, A), fun( (A) -> state_t(S, M, B) ), M) -> state_t(S, M, B).
'>>='(X, Fun, {?MODULE, M}) ->
    fun (S) ->
            do([M || {A, S1} <- X(S),
                     (Fun(A))(S1)])
    end.


-spec return(A, M) -> state_t(_S, M, A).
return(A, {?MODULE, M}) ->
    fun (S) ->
            M:return({A, S})
    end.


-spec fail(any(), M) -> state_t(_S, M, _A).
fail(E, {?MODULE, M}) ->
    fun (_) ->
            M:fail(E)
    end.


-spec get(M) -> state_t(S, M, S).
get({?MODULE, M}) ->
    fun (S) ->
            M:return({S, S})
    end.


-spec put(S, M) -> state_t(S, M, ok).
put(S, {?MODULE, M}) ->
    fun (_) ->
            M:return({ok, S})
    end.


-spec eval(state_t(S, M, A), S, M) -> esugar_monad:monadic(M, A).
eval(SM, S, {?MODULE, M}) ->
    do([M || {A, _S1} <- SM(S),
             return(A)]).


-spec exec(state_t(S, M, _A), S, M) -> esugar_monad:monadic(M, S).
exec(SM, S, {?MODULE, M}) ->
    do([M || {_A, S1} <- SM(S),
             return(S1)]).


-spec run(state_t(S, M, A), S, M) -> esugar_monad:monadic(M, {A, S}).
run(SM, S, _M) -> SM(S).


-spec modify(fun( (S) -> S ), M) -> state_t(S, M, ok).
modify(Fun, {?MODULE, M}) ->
    fun (S) ->
            M:return({ok, Fun(S)})
    end.


-spec modify_and_return(fun( (S) -> {A, S} ), M) -> state_t(S, M, A).
modify_and_return(Fun, {?MODULE, M}) ->
    fun (S) ->
            M:return(Fun(S))
    end.


-spec lift(esugar_monad:monadic(M, A), M) -> state_t(_S, M, A).
lift(X, {?MODULE, M}) ->
    fun (S) ->
            do([M || A <- X,
                     return({A, S})])
    end.
