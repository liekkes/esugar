-module(esugar_monoid).
-export_type([monoid/0, monoid/2]).


-type monoid() :: module().
-type monoid(_M, _A) :: any().


-callback mempty() ->
	_A.
-callback mappend(A, A) ->
	A.
-callback mconcat([A]) ->
	A.

