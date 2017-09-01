-module(esugar_applicative).
-extends(esugar_functor).
-export_type([applicative/0, applicative/2]).


-type applicative() :: module() | {module(), applicative()}.
-type applicative(_M, _A) :: any().


-callback pure(A) ->
    applicative(M, A) when M :: applicative().
-callback '<*>'(applicative(M, fun((A) -> B)), applicative(M, A)) ->
    applicative(M, B) when M :: applicative().

