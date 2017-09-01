-module(esugar_do_transform_tests).
-compile([{parse_transform, esugar_do_transform}]).
-include_lib("eunit/include/eunit.hrl").


esugar_do_transform_test_() ->
    {foreach,
        fun() -> error_logger:tty(false) end,
        fun(_) -> error_logger:tty(true) end,
        [
              {<<"test_sequence">>, fun test_sequence/0}
            , {<<"test_join">>, fun test_join/0}
            , {<<"test_maybe">>, fun test_maybe/0}
            , {<<"test_fib">>, fun test_fib/0}
            , {<<"test_list">>, fun test_list/0}
            , {<<"test_omega">>, fun test_omega/0}
            , {<<"test_error_t_list">>, fun test_error_t_list/0}
            , {<<"test_let_match">>, fun test_let_match/0}
            , {<<"test_let_first">>, fun test_let_first/0}
            , {<<"test_let_escapes">>, fun test_let_escapes/0}
            , {<<"test_named_fun">>, fun test_named_fun/0}
            , {<<"test_maps">>, fun test_maps/0}
        ]
    }.


test_sequence() ->
    List = lists:seq(1, 5),
    ListM = [do([esugar_maybe || return(N)]) || N <- List],
    {just, List} = esugar_monad:sequence(esugar_maybe, ListM).


test_join() ->
    {just, 5} = esugar_monad:join(esugar_maybe,
                           esugar_maybe:return(esugar_maybe:return(5))),
    {just, 5} = esugar_monad:join(esugar_maybe,
                           do([esugar_maybe || return(esugar_maybe:return(5))])),
    {just, 5} = esugar_monad:join(esugar_maybe,
                           do([esugar_maybe || return(do([esugar_maybe || return(5)]))])).

test_maybe() ->
    nothing = maybe(atom),
    {just, 9} = maybe(3).

maybe(Arg) ->
    do([esugar_maybe
        || esugar_monad_plus:guard(esugar_maybe, is_number(Arg)),
           return(Arg*Arg)]).

test_fib() ->
    true = lists:all(fun ({X, Y}) -> X =:= Y end,
                     [{fib_m(N), fib_rec(N)} || N <- lists:seq(0, 20)]).

%% Classic monadic implementation of fibonnaci
fib_m(N) ->
    StateT = esugar_state_t:new(esugar_identity_m),
    {_, R} = StateT:exec(
               esugar_monad:sequence(StateT,
                              lists:duplicate(N, fib_m_step(StateT))), {0, 1}),
    R.

fib_m_step(StateT) -> StateT:modify(fun ({X, Y}) -> {Y, X+Y} end).

%% Classic recursive implementation of fibonnaci
fib_rec(N) when N >= 0 -> fib_rec(N, 0, 1).
fib_rec(0, _X, Y) -> Y;
fib_rec(N,  X, Y) -> fib_rec(N-1, Y, X+Y).

test_list() ->
    %% Demonstrate equivalence of list comprehensions and list monad
    A = [{X,Y} || X <- "abcd",
                  Y <- [1,2]],
    A = do([esugar_list || X <- "abcd",
                      Y <- [1,2],
                      return({X,Y})]),
    %% Classic pythagorean triples
    P = [{X, Y, Z} || Z <- lists:seq(1,20),
                      X <- lists:seq(1,Z),
                      Y <- lists:seq(X,Z),
                      math:pow(X,2) + math:pow(Y,2) == math:pow(Z,2)],
    P = do([esugar_list || Z <- lists:seq(1,20),
                      X <- lists:seq(1,Z),
                      Y <- lists:seq(X,Z),
                      esugar_monad_plus:guard(
                        esugar_list, math:pow(X,2) + math:pow(Y,2) == math:pow(Z,2)),
                      return({X,Y,Z})]).

test_omega() ->
    A = [{X,Y,Z} || X <- "abcd",
                    Y <- lists:seq(1,5),
                    Z <- lists:seq(11,15)],
    B = do([esugar_omega_m || X <- "abcd",
                       Y <- lists:seq(1,5),
                       Z <- lists:seq(11,15),
                       return({X,Y,Z})]),
    true = A =/= B,
    true = A =:= lists:usort(B).

test_error_t_list() ->
    M = esugar_error_t:new(esugar_list),
    R = M:run(do([M || E1 <- M:lift([1, 2, 3]),
                       E2 <- M:lift([4, 5, 6]),
                       case (E1 * E2) rem 2 of
                           0 -> return({E1, E2});
                           _ -> fail(not_even_product)
                       end])),
    R = [{ok, {1, 4}}, {error, not_even_product}, {ok, {1, 6}},
         {ok, {2, 4}}, {ok, {2, 5}},              {ok, {2, 6}},
         {ok, {3, 4}}, {error, not_even_product}, {ok, {3, 6}}],

    %% Compare with the non-error_t version, which will remove failures:
    S = do([esugar_list || E1 <- [1, 2, 3],
                      E2 <- [4, 5, 6],
                      case (E1 * E2) rem 2 of
                          0 -> return({E1, E2});
                          _ -> fail(not_even_product)
                      end]),
    S = [{1, 4}, {1, 6}, {2, 4}, {2, 5}, {2, 6}, {3, 4}, {3, 6}].

%% Tests for 'let-match binding' (a-la 'let' in Haskell's 'do'
%% expression) But instead of 'let' here we use 'match' (=) expression
%% in 'do([])':
test_let_match() ->
    T1 = do([esugar_maybe || R <- return(2),
                        R2 = R*R,
                        return(R2*R2)]),
    T1 = do([esugar_maybe || R <- return(2),
                        return(R*R*R*R)]),
    %% Failure test
    T2 = do([esugar_error || A <- return(42),
                        {B,C} <- fail(test),
                        BC = B*C,
                        return(BC+A)]),
    T2 = do([esugar_error || A <- return(42),
                        {B,C} <- fail(test),
                        return(B*C+A)]),

    Fun = fun({X,Y}) -> {Y,X} end, %% Mysterious function
    T3 = do([esugar_error || R <- return({1,42}),
                        {R1,R2} = Fun(R),
                        return(R1+R2)]),
    T3 = do([esugar_error || R <- return({1,42}),
                        %% No better way without 'let'?
                        %% Well, only via extra 'return'
                        return(element(1,Fun(R)) + element(2,Fun(R)))]),

    DivRem = fun(N,M) -> {N div M,N rem M} end,
    T4 = do([esugar_error || {N,M} <- return({42,3}),
                        {D,R} = DivRem(N,M),
                        E <- T3,
                        S = D+R+E,
                        return({D,R,S})]),
    T4 = do([esugar_error || {N,M} <- return({42,3}),
                        %% Can hack it with extra 'return' (and '>>='
                        %% as result)
                        {D,R} <- return(DivRem(N,M)),
                        E <- T3,
                        return({D,R,D+R+E})]),

    T5 = do([esugar_list || X <- [1,2,3],
                       X2 = X*X,
                       Y <- lists:seq(1,X2),
                       Y2 = {Y,X2},
                       Z = Y + X2,
                       return({X2,Y,Y2,Z})]),
    T5 = do([esugar_list || X <- [1,2,3],
                       Y <- lists:seq(1,X*X),
                       return({X*X,Y,{Y,X*X},Y+X*X})]).

test_let_first() ->
    M = do([esugar_list || A = 3,
                      X <- [1,2,A],
                      Y <- [A,A+1],
                      return({X,Y})]),
    M = fun() ->
                A = 3,
                do([esugar_list || X <- [1,2,A],
                              Y <- [A,A+1],
                              return({X,Y})])
        end().

test_let_escapes() ->
    M1 = do([esugar_maybe || A = 5,
                        return(A)]),
    M2 = do([esugar_maybe || A = 6,
                        return(A)]),
    M1 = do([esugar_maybe || return(5)]),
    M2 = do([esugar_maybe || return(6)]),

    %% Demonstrate that bindings do not escape.
    M3 = do([esugar_maybe || return(_A = 5)]),
    M3 = do([esugar_maybe || return((_A = 7) - 2)]),
    _A = 6.

test_named_fun() ->
    Fib  = fun Self (0) -> esugar_identity_m:return(1);
               Self (1) -> esugar_identity_m:return(1);
               Self (N) -> do([esugar_identity_m || M <- Self(N-1),
                                             return(N+M)])
           end,
    true = Fib(10) =:= 55.

test_maps() ->
    M1 = do([esugar_maybe || A = #{ a => b },
                        X <- return(A),
                        Y <- return(X#{ a := c, b => d }),
                        return(Y)
            ]),
    {just, #{ a := c, b := d }} = M1.


