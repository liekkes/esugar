-ifndef(ESUGAR_TYPECLASS_HRL).
-define(ESUGAR_TYPECLASS_HRL, true).


-define(CAR(Cons), element(1, Cons)).
-define(CDR(Cons), element(2, Cons)).
-define(CONS(Car, Cdr), {Car, Cdr}).

-define(TYPE(Term), ?CAR(Term)).
-define(DATA(Term), ?CDR(Term)).
-define(TERM(Type, Data), ?CONS(Type, Data)).

-define(list, list).
-define(maybe, maybe).
-define(error, error).

-define(WM(Type, Func), esugar_typeclass:which_module(Type, Func)).

-define(ID, fun(X) -> X end).


-endif.
