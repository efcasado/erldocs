-module(erldocs_pt).

-compile({parse_transform, forms_pt}).

-export([parse_transform/2]).

%% ========================================================================
%%  Macro definitions
%% ========================================================================

-define(DOC_ATTR(Var), {attribute, _, doc, Var}).
-define(FUNCTION(Var1, Var2), {function, _, Var1, Var2, _}).


%% ========================================================================
%%  Parse transform
%% ========================================================================

parse_transform(Forms, _Opts) ->
    Docs = parse(Forms),
    HelpFunction = help_gen(Docs),
    Forms1 = meta:add_function(HelpFunction, true, Forms),
    delete_doc_attrs(Forms1).

help_gen(Docs) ->
    Bindings = [ [{'F', F}, {'A', A}, {'D', D}]  || {F, A, D} <- Docs ],
    forms:function(help, [{fun(F, A) -> D end, Bindings}]).

parse(Forms) ->
    parse(Forms, []).

parse([],  Acc) ->
    Acc;
parse([_], Acc) ->
    Acc;
parse([?DOC_ATTR(DocString), ?FUNCTION(Name, Arity)| Forms], Acc) ->
    parse(Forms, [{Name, Arity, DocString}| Acc]);
parse([_| Forms], Acc) ->
    parse(Forms, Acc).

delete_doc_attrs(Forms) ->
    lists:foldr(fun({attribute, _, doc, _}, Acc) ->
                        Acc;
                   (Other, Acc) ->
                        [Other| Acc]
                end,
                [],
                Forms).
