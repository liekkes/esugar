-module(esugar_monoid).
-export_type([monoid/0, monoid/1]).


-type monoid() :: monoid(_).
-type monoid(_A) :: any().


-callback mempty() ->
    _A.
-callback mappend(A, A) ->
    A.
-callback mconcat([A]) ->
    A.

