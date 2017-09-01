-module(esugar_monad_trans).
-extends(esugar_monad).
-export_type([monad_trans/0, monad_trans/2]).


-type monad_trans() :: module() | {module(), monad_trans()}.
-type monad_trans(_T, _A) :: any().


-callback lift(esugar_monad:monad(M, A), M) ->
    monad_trans(T, esugar_monad:monad(M, A)) when
    M :: esugar_monad:monad(), T :: monad_trans().

