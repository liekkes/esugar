-module(esugar_functor).
-export_type([functor/1]).


-type functor(_F) :: any().


-callback fmap(fun((A) -> B), functor(A)) ->
    functor(B).
-callback '<$'(B, functor(_A)) ->
    functor(B).

