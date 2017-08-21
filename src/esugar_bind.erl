-module(esugar_bind).
-compile([{parse_transform, esugar_bind_transform}]).
-export([foo/0]).


foo() ->
	X = 1,
	X = fun(X) ->
		X + 1
	end,
	X(8).






