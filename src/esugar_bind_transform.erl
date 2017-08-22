-module(esugar_bind_transform).
-export([format_error/1]).
-export([parse_transform/2]).

-define(get_scope_stack(), erlang:get(scope_stack)).
-define(set_scope_stack(L), erlang:put(scope_stack, L)).
-define(del_scope_stack(), erlang:erase(scope_stack)).
-define(add_scope(), ?set_scope_stack([#{} | ?get_scope_stack()])).
-define(del_scope(), ?set_scope_stack(erlang:tl(?get_scope_stack()))).
-define(get_scope(), erlang:hd(?get_scope_stack())).
-define(set_scope(S), ?set_scope_stack([S | tl(?get_scope_stack())])).
-define(get_pattern(), erlang:get(pattern)).
-define(set_pattern(P), erlang:put(pattern, P)).
-define(clr_pattern(), begin ?set_scope(maps:merge(?get_scope()
                , ?get_pattern())), erlang:erase(pattern) end).


format_error({Format, Args}) ->
    lists:flatten(io_lib:format(Format, Args));
format_error(AnyThing) -> AnyThing.


parse_transform(AST, _Options) ->
    walk_ast(AST).


walk_ast([{function, _Line, _Name, _Arity, _Clauses} = Function | T]) ->
    try [walk_function(Function) | walk_ast(T)] catch
    throw:{ErrLine, ErrArgument} ->
        [{error, {ErrLine, ?MODULE, ErrArgument}} | walk_ast(T)];
    ErrType:ErrReason ->
        erlang:raise(ErrType, ErrReason, erlang:get_stacktrace())
    end;
walk_ast([H | T]) -> [H | walk_ast(T)]; walk_ast([]) -> [].


walk_function({function, Line, Name, Arity, Clauses}) ->
    ?set_scope_stack([]),
    Clauses2 = walk_function_clauses(Clauses),
    ?del_scope_stack(),
    {function, Line, Name, Arity, Clauses2}.


walk_function_clauses([Clause | T]) ->
    ?add_scope(),
    Clause2 = walk_clause(Clause),
    ?del_scope(),
    [Clause2 | walk_function_clauses(T)];
walk_function_clauses([]) -> [].


walk_clause({clause, Line, Head, Guard, Body}) ->
    ?set_pattern(#{}),
    Head2 = walk_patterns(Head),
    ?clr_pattern(),
    Guard2 = guard(Guard),
    Body2 = exprs(Body),
    {clause, Line, Head2, Guard2, Body2}.


walk_patterns([Pattern | T]) ->
    [walk_pattern(Pattern) | walk_patterns(T)];
walk_patterns([]) -> [].


walk_pattern({var, Line, Var}) ->
    {var, Line, left_var(Line, Var)};
walk_pattern({match, Line, L, R}) ->
    {match, Line, walk_pattern(L), walk_pattern(R)};
walk_pattern({cons, Line, H, T}) ->
    {cons, Line, walk_pattern(H), walk_pattern(T)};
walk_pattern({tuple, Line, Patterns}) ->
    {tuple, Line, walk_patterns(Patterns)};
walk_pattern({map, Line, Patterns}) ->
    {map, Line, walk_patterns(Patterns)};
walk_pattern({map_field_exact,Line,K,V}) ->
    Ke = expr(K),
    Ve = walk_pattern(V),
    {map_field_exact,Line,Ke,Ve};
walk_pattern({record,Line,Name,Pfs0}) ->
    Pfs1 = pattern_fields(Pfs0),
    {record,Line,Name,Pfs1};
walk_pattern({record_index,Line,Name,Field0}) ->
    Field1 = walk_pattern(Field0),
    {record_index,Line,Name,Field1};
walk_pattern({record_field,Line,Rec0,Name,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Name,Field1};
walk_pattern({record_field,Line,Rec0,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Field1};
walk_pattern({bin,Line,Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin,Line,Fs2};
walk_pattern(Pattern) -> Pattern.

pattern_grp([{bin_element,L1,E1,S1,T1} | Fs]) ->
    S2 = case S1 of
         default ->
         default;
         _ ->
         expr(S1)
     end,
    T2 = case T1 of
         default ->
         default;
         _ ->
         bit_types(T1)
     end,
    [{bin_element,L1,walk_pattern(E1),S2,T2} | pattern_grp(Fs)];
pattern_grp([]) ->
    [].

expr_grp([{bin_element,L1,E1,S1,T1} | Fs]) ->
    S2 = case S1 of
         default ->
         default;
         _ ->
         expr(S1)
     end,
    T2 = case T1 of
         default ->
         default;
         _ ->
         bit_types(T1)
     end,
    [{bin_element,L1,expr(E1),S2,T2} | expr_grp(Fs)];
expr_grp([]) ->
    [].

bit_types([]) ->
    [];
bit_types([Atom | Rest]) when is_atom(Atom) ->
    [Atom | bit_types(Rest)];
bit_types([{Atom, Integer} | Rest]) when is_atom(Atom), is_integer(Integer) ->
    [{Atom, Integer} | bit_types(Rest)].


%% -type pattern_fields([Field]) -> [Field].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

pattern_fields([{record_field,Lf,{atom,La,F},P0}|Pfs]) ->
    P1 = walk_pattern(P0),
    [{record_field,Lf,{atom,La,F},P1}|pattern_fields(Pfs)];
pattern_fields([{record_field,Lf,{var,La,'_'},P0}|Pfs]) ->
    P1 = walk_pattern(P0),
    [{record_field,Lf,{var,La,'_'},P1}|pattern_fields(Pfs)];
pattern_fields([]) -> [].

%% -type guard([GuardTest]) -> [GuardTest].

guard([G0|Gs]) when is_list(G0) ->
    [guard0(G0) | guard(Gs)];
guard(L) ->
    guard0(L).

guard0([G0|Gs]) ->
    G1 =  guard_test(G0),
    [G1|guard0(Gs)];
guard0([]) -> [].

guard_test(Expr={call,Line,{atom,La,F},As0}) ->
    case erl_internal:type_test(F, length(As0)) of
    true -> 
        As1 = gexpr_list(As0),
        {call,Line,{atom,La,F},As1};
    _ ->
        gexpr(Expr)
    end;
guard_test(Any) ->
    gexpr(Any).

%% Before R9, there were special rules regarding the expressions on
%% top level in guards. Those limitations are now lifted - therefore
%% there is no need for a special clause for the toplevel expressions.
%% -type gexpr(GuardExpr) -> GuardExpr.

gexpr({var,Line,Var}) ->
    Var2 = right_var(Line, Var),
    {var,Line,Var2};
gexpr({integer,Line,I}) -> {integer,Line,I};
gexpr({char,Line,C}) -> {char,Line,C};
gexpr({float,Line,F}) -> {float,Line,F};
gexpr({atom,Line,A}) -> {atom,Line,A};
gexpr({string,Line,S}) -> {string,Line,S};
gexpr({nil,Line}) -> {nil,Line};
gexpr({map,Line,Map0,Es0}) ->
    [Map1|Es1] = gexpr_list([Map0|Es0]),
    {map,Line,Map1,Es1};
gexpr({map,Line,Es0}) ->
    Es1 = gexpr_list(Es0),
    {map,Line,Es1};
gexpr({map_field_assoc,Line,K,V}) ->
    Ke = gexpr(K),
    Ve = gexpr(V),
    {map_field_assoc,Line,Ke,Ve};
gexpr({map_field_exact,Line,K,V}) ->
    Ke = gexpr(K),
    Ve = gexpr(V),
    {map_field_exact,Line,Ke,Ve};
gexpr({cons,Line,H0,T0}) ->
    H1 = gexpr(H0),
    T1 = gexpr(T0),             %They see the same variables
    {cons,Line,H1,T1};
gexpr({tuple,Line,Es0}) ->
    Es1 = gexpr_list(Es0),
    {tuple,Line,Es1};
gexpr({record_index,Line,Name,Field0}) ->
    Field1 = gexpr(Field0),
    {record_index,Line,Name,Field1};
gexpr({record_field,Line,Rec0,Name,Field0}) ->
    Rec1 = gexpr(Rec0),
    Field1 = gexpr(Field0),
    {record_field,Line,Rec1,Name,Field1};
gexpr({record,Line,Name,Inits0}) ->
    Inits1 = grecord_inits(Inits0),
    {record,Line,Name,Inits1};
gexpr({call,Line,{atom,La,F},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) of
    true -> As1 = gexpr_list(As0),
        {call,Line,{atom,La,F},As1}
    end;
% Guard bif's can be remote, but only in the module erlang...
gexpr({call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,F}},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) or
     erl_internal:arith_op(F, length(As0)) or 
     erl_internal:comp_op(F, length(As0)) or
     erl_internal:bool_op(F, length(As0)) of
    true -> As1 = gexpr_list(As0),
        {call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,F}},As1}
    end;
gexpr({bin,Line,Fs}) ->
    Fs2 = expr_grp(Fs),
    {bin,Line,Fs2};
gexpr({op,Line,Op,A0}) ->
    case erl_internal:arith_op(Op, 1) or 
     erl_internal:bool_op(Op, 1) of
    true -> A1 = gexpr(A0),
        {op,Line,Op,A1}
    end;
gexpr({op,Line,Op,L0,R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
    %% R11B: andalso/orelse are now allowed in guards.
    L1 = gexpr(L0),
    R1 = gexpr(R0),         %They see the same variables
    {op,Line,Op,L1,R1};
gexpr({op,Line,Op,L0,R0}) ->
    case erl_internal:arith_op(Op, 2) or
      erl_internal:bool_op(Op, 2) or 
      erl_internal:comp_op(Op, 2) of
    true ->
        L1 = gexpr(L0),
        R1 = gexpr(R0),         %They see the same variables
        {op,Line,Op,L1,R1}
    end.

%% -type gexpr_list([GuardExpr]) -> [GuardExpr].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

gexpr_list([E0|Es]) ->
    E1 = gexpr(E0),
    [E1|gexpr_list(Es)];
gexpr_list([]) -> [].

grecord_inits([{record_field,Lf,{atom,La,F},Val0}|Is]) ->
    Val1 = gexpr(Val0),
    [{record_field,Lf,{atom,La,F},Val1}|grecord_inits(Is)];
grecord_inits([{record_field,Lf,{var,La,'_'},Val0}|Is]) ->
    Val1 = gexpr(Val0),
    [{record_field,Lf,{var,La,'_'},Val1}|grecord_inits(Is)];
grecord_inits([]) -> [].

%% -type exprs([Expression]) -> [Expression].
%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs([E0|Es]) ->
    E1 = expr(E0),
    [E1|exprs(Es)];
exprs([]) -> [].

%% -type expr(Expression) -> Expression.

expr({var,Line,Var}) ->
    Var2 = right_var(Line, Var),
    {var,Line,Var2};
expr({integer,Line,I}) -> {integer,Line,I};
expr({float,Line,F}) -> {float,Line,F};
expr({atom,Line,A}) -> {atom,Line,A};
expr({string,Line,S}) -> {string,Line,S};
expr({char,Line,C}) -> {char,Line,C};
expr({nil,Line}) -> {nil,Line};
expr({cons,Line,H0,T0}) ->
    H1 = expr(H0),
    T1 = expr(T0),              %They see the same variables
    {cons,Line,H1,T1};
expr({lc,Line,E0,Qs0}) ->
    ?add_scope(),
    Qs1 = lc_bc_quals(Qs0),
    E1 = expr(E0),
    ?del_scope(),
    {lc,Line,E1,Qs1};
expr({bc,Line,E0,Qs0}) ->
    ?add_scope(),
    Qs1 = lc_bc_quals(Qs0),
    E1 = expr(E0),
    ?del_scope(),
    {bc,Line,E1,Qs1};
expr({tuple,Line,Es0}) ->
    Es1 = expr_list(Es0),
    {tuple,Line,Es1};
expr({map,Line,Map0,Es0}) ->
    [Map1|Es1] = exprs([Map0|Es0]),
    {map,Line,Map1,Es1};
expr({map,Line,Es0}) ->
    Es1 = exprs(Es0),
    {map,Line,Es1};
expr({map_field_assoc,Line,K,V}) ->
    Ke = expr(K),
    Ve = expr(V),
    {map_field_assoc,Line,Ke,Ve};
expr({map_field_exact,Line,K,V}) ->
    Ke = expr(K),
    Ve = expr(V),
    {map_field_exact,Line,Ke,Ve};
%%expr({struct,Line,Tag,Es0}) ->
%%    Es1 = walk_patterns(Es0),
%%    {struct,Line,Tag,Es1};
expr({record_index,Line,Name,Field0}) ->
    Field1 = expr(Field0),
    {record_index,Line,Name,Field1};
expr({record,Line,Name,Inits0}) ->
    Inits1 = record_inits(Inits0),
    {record,Line,Name,Inits1};
expr({record_field,Line,Rec0,Name,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Name,Field1};
expr({record,Line,Rec0,Name,Upds0}) ->
    Rec1 = expr(Rec0),
    Upds1 = record_updates(Upds0),
    {record,Line,Rec1,Name,Upds1};
expr({record_field,Line,Rec0,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Field1};
expr({block,Line,Es0}) ->
    %% Unfold block into a sequence.
    Es1 = exprs(Es0),
    {block,Line,Es1};
expr({'if',Line,Cs0}) ->
    Scope = ?get_scope(),
    Cs1 = icr_clauses(Cs0, Scope, Scope),
    {'if',Line,Cs1};
expr({'case',Line,E0,Cs0}) ->
    E1 = expr(E0),
    Scope = ?get_scope(),
    Cs1 = icr_clauses(Cs0, Scope, Scope),
    {'case',Line,E1,Cs1};
expr({'receive',Line,Cs0}) ->
    Scope = ?get_scope(),
    Cs1 = icr_clauses(Cs0, Scope, Scope),
    {'receive',Line,Cs1};
expr({'receive',Line,Cs0,To0,ToEs0}) ->
    To1 = expr(To0),
    ToEs1 = exprs(ToEs0),
    Scope = ?get_scope(),
    Cs1 = icr_clauses(Cs0, Scope, Scope),
    {'receive',Line,Cs1,To1,ToEs1};
expr({'try',Line,Es0,Scs0,Ccs0,As0}) ->
    Es1 = exprs(Es0),
    Scope = ?get_scope(),
    Scs1 = icr_clauses(Scs0, Scope, Scope),
    Scope2 = ?get_scope(),
    Ccs1 = icr_clauses(Ccs0, Scope2, Scope2),
    As1 = exprs(As0),
    {'try',Line,Es1,Scs1,Ccs1,As1};
expr({'fun',Line,Body}) ->
    case Body of
    {clauses,Cs0} ->
        Cs1 = fun_clauses(Cs0),
        {'fun',Line,{clauses,Cs1}};
    {function,F,A} ->
        {'fun',Line,{function,F,A}};
    {function,M,F,A} when is_atom(M), is_atom(F), is_integer(A) ->
        %% R10B-6: fun M:F/A. (Backward compatibility)
        {'fun',Line,{function,M,F,A}};
    {function,M0,F0,A0} ->
        %% R15: fun M:F/A with variables.
        M = expr(M0),
        F = expr(F0),
        A = expr(A0),
        {'fun',Line,{function,M,F,A}}
    end;
expr({named_fun,Loc,Name,Cs}) ->
    ?add_scope(),
    ?set_pattern(#{}),
    Name2 = left_var(Loc, Name),
    ?clr_pattern(),
    Cs2 = fun_clauses(Cs),
    ?del_scope(),
    {named_fun,Loc,Name2,Cs2};
expr({call,Line,F0,As0}) ->
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    F1 = expr(F0),
    As1 = expr_list(As0),
    {call,Line,F1,As1};
expr({'catch',Line,E0}) ->
    %% No new variables added.
    E1 = expr(E0),
    {'catch',Line,E1};
expr({match,Line,P0,E0}) ->
    E1 = expr(E0),
    ?set_pattern(#{}),
    P1 = walk_pattern(P0),
    ?clr_pattern(),
    {match,Line,P1,E1};
expr({bin,Line,Fs}) ->
    Fs2 = expr_grp(Fs),
    {bin,Line,Fs2};
expr({op,Line,Op,A0}) ->
    A1 = expr(A0),
    {op,Line,Op,A1};
expr({op,Line,Op,L0,R0}) ->
    L1 = expr(L0),
    R1 = expr(R0),              %They see the same variables
    {op,Line,Op,L1,R1};
%% The following are not allowed to occur anywhere!
expr({remote,Line,M0,F0}) ->
    M1 = expr(M0),
    F1 = expr(F0),
    {remote,Line,M1,F1};
expr(Expr) -> Expr.

%% -type expr_list([Expression]) -> [Expression].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

expr_list([E0|Es]) ->
    E1 = expr(E0),
    [E1|expr_list(Es)];
expr_list([]) -> [].

%% -type record_inits([RecordInit]) -> [RecordInit].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_inits([{record_field,Lf,{atom,La,F},Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field,Lf,{atom,La,F},Val1}|record_inits(Is)];
record_inits([{record_field,Lf,{var,La,'_'},Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field,Lf,{var,La,'_'},Val1}|record_inits(Is)];
record_inits([]) -> [].

%% -type record_updates([RecordUpd]) -> [RecordUpd].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_updates([{record_field,Lf,{atom,La,F},Val0}|Us]) ->
    Val1 = expr(Val0),
    [{record_field,Lf,{atom,La,F},Val1}|record_updates(Us)];
record_updates([]) -> [].

%% -type icr_clauses([Clause]) -> [Clause].

icr_clauses([C0|Cs], AccScope, OldScope) ->
    ?set_scope(OldScope),
    C1 = walk_clause(C0),
    IcrScope = ?get_scope(),
    AccScope2 = merge_scope(AccScope, IcrScope),
    [C1|icr_clauses(Cs, AccScope2, OldScope)];
icr_clauses([], AccScope, _OldScope) ->
    ?set_scope(AccScope),
    [].

%% -type lc_bc_quals([Qualifier]) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

lc_bc_quals([{generate,Line,P0,E0}|Qs]) ->
    E1 = expr(E0),
    ?set_pattern(#{}),
    P1 = walk_pattern(P0),
    ?clr_pattern(),
    [{generate,Line,P1,E1}|lc_bc_quals(Qs)];
lc_bc_quals([{b_generate,Line,P0,E0}|Qs]) ->
    E1 = expr(E0),
    ?set_pattern(#{}),
    P1 = walk_pattern(P0),
    ?clr_pattern(),
    [{b_generate,Line,P1,E1}|lc_bc_quals(Qs)];
lc_bc_quals([E0|Qs]) ->
    E1 = expr(E0),
    [E1|lc_bc_quals(Qs)];
lc_bc_quals([]) -> [].

%% -type fun_clauses([Clause]) -> [Clause].

fun_clauses([C0|Cs]) ->
    ?add_scope(),
    C1 = walk_clause(C0),
    ?del_scope(),
    [C1|fun_clauses(Cs)];
fun_clauses([]) -> [].





merge_scope(ScopeA, ScopeB) ->
    maps:fold(fun(K, VB, Acc) ->
        case Acc of
        #{K := VA} when VA > VB ->
            Acc;
        _ ->
            Acc#{K => VB}
        end
    end, ScopeA, ScopeB).


-define(INIT_NO, 1).

concat(Var, No) ->
    erlang:list_to_atom(
           erlang:atom_to_list(Var)
        ++ "@"
        ++ erlang:integer_to_list(No)
    ).

left_var(Line, '_') -> '_';
left_var(Line, VarOrVarAt) ->
    case lists:reverse(erlang:atom_to_list(VarOrVarAt)) of
    "@" ++ RevInit ->
        Var = erlang:list_to_atom(lists:reverse(RevInit)),
        LastCharIsAt = true;
    _ ->
        Var = VarOrVarAt,
        LastCharIsAt = false
    end,
    Pattern = ?get_pattern(),
    case Pattern of
    #{Var := No} ->
        concat(Var, No);
    _ ->
        ScopeStack = ?get_scope_stack(),
        case left_no(Var, ScopeStack) of
        No when is_integer(No) ->
            case LastCharIsAt of
            false ->
                No2 = No + 1;
            _ ->
                No2 = No
            end,
            ?set_pattern(Pattern#{Var => No2}),
            concat(Var, No2);
        _ ->
            case LastCharIsAt of
            false ->
                No = ?INIT_NO,
                ?set_pattern(Pattern#{Var => No}),
                concat(Var, No);
            _ ->
                erlang:throw({111, {"~p", [Var]}})
            end
        end
    end.

left_no(Var, [Scope | T]) ->
    case Scope of
    #{Var := No} -> No;
    _ -> left_no(Var, T)
    end;
left_no(_Var, _ScopeStack) ->
    undefined.



right_var(Line, Var) ->
    case lists:reverse(erlang:atom_to_list(Var)) of
    "@" ++ _Init -> erlang:throw({error, Line, Var}); _ -> next
    end,
    ScopeStack = ?get_scope_stack(),
    case right_no(Var, ScopeStack) of
    No when is_integer(No) ->
        concat(Var, No);
    _ -> erlang:throw({Line, {"~p", [Var]}})
    end.

right_no(Var, [Scope | T]) ->
    case Scope of
    #{Var := No} -> No;
    _ -> right_no(Var, T)
    end;
right_no(_Var, _ScopeStack) ->
    undefined.

