-module(esugar_typeclass).
-include("esugar_typeclass.hrl").
-export([which_module/2]).


which_module(?list, mempty) -> esugar_list;
which_module(?list, mappend) -> esugar_list;
which_module(?list, mconcat) -> esugar_list;
which_module(?maybe, mempty) -> esugar_maybe;
which_module(?maybe, mappend) -> esugar_maybe;
which_module(?maybe, mconcat) -> esugar_maybe;
which_module(_Type, _Func) -> undefined.



