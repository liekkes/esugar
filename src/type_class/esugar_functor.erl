-module(esugar_functor).
-export_type([functor/0, functor/2]).


-type functor() :: module().
-type functor(_M, _A) :: any().


-callback fmap(fun((A) -> B), functor(M, A)) ->
    functor(M, B) when M :: functor().
-callback '<$'(B, functor(M, _A)) ->
    functor(M, B) when M :: functor().

