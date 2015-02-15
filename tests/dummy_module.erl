-module(dummy_module).

-compile({parse_transform, erldocs_pt}).

-export([f1/0, f2/1]).

-doc "This docstring should be skipped".
-doc "This docstring shold be askipped, as well".
-doc "This is the documentation for f1".
-spec f1() -> 'f1'.
f1() -> f1.

-doc "This is the documentation for f2".
-spec f2(X :: integer()) -> boolean().
f2(N) ->
    (N rem 2) == 0.

-doc "This docstring should also be skipped".
