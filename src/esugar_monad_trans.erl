-module(esugar_monad_trans).
-compile({parse_transform, esugar_do_transform}).


-callback '>>='(esugar_monad:monadic(TM, A),
		fun((A) -> esugar_monad:monadic(TM, B)), M) -> esugar_monad:monadic(TM, B)
	when TM :: esugar_monad:monad(), M :: esugar_monad:monad().
-callback return(A, M) ->
	esugar_monad:monadic(TM, A)
	when TM :: esugar_monad:monad(), M :: esugar_monad:monad().
-callback fail(any(), M) ->
	esugar_monad:monadic(TM, _A)
	when TM :: esugar_monad:monad(), M :: esugar_monad:monad().


-callback lift(esugar_monad:monadic(M, A), M) ->
	esugar_monad:monadic(TM, A)
	when TM :: esugar_monad:monad(), M :: esugar_monad:monad().

