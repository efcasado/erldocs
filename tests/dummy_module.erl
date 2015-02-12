-module(dummy_module).

-compile({parse_transform, erldocs_pt}).

-export([f1/0]).

-doc "This docstring should be skipped".
-doc "This docstring shold be askipped, as well".
-doc "This is the documentation for f1".
f1() -> f1.

-doc "This docstring should also be skipped".
