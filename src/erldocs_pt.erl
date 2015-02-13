%%%========================================================================
%%% File: erldocs_pt.erl
%%%
%%% An opinionated parse transform that promotes documentation to a
%%% first-class citizen in the Erlang society.
%%%
%%% Conventions:
%%%   - Documentation is written using the doc attribute (i.e., `-doc`)
%%%   - Function specifications (i.e., spec attributes), if any, must go
%%%     between the doc attribute and the function definition
%%%
%%%
%%% Assuming a `lists2` module including the following code:
%%%
%%%     -doc "Returns `true` if `Elem` matches some element of `Lists`, "
%%%          "otherwise `false`."
%%%     -spec member(Elem :: any(), List :: list(any())) -> boolean().
%%%     member(Elem, List) ->
%%%         lists:member(Elem, Lists).
%%%
%%% The documentation of the `lists2:member/2` function will be available
%%% as `lists2:help(member, 2)`. That is, a call to
%%% `lists2:help(member, 2)` will output:
%%%
%%%     "<h3>member/2</h3>"
%%%     "<pre><code>"
%%%     "member(Elem :: any(), List :: list(any())) -> boolean()"
%%%     "<pre><code>"
%%%     "<p>Returns <code>true</code> if <code>Elem</code> matches some"
%%%     "element of <code>Lists<code>, otherwise, <code>false</code>.</p>"
%%%
%%%
%%% Author: Enrique Fernandez <enrique.fernandez@gmail.com>
%%% Date:   January, 2015
%%%
%%%-- LICENSE -------------------------------------------------------------
%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2015 Enrique Fernandez
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining
%%% a copy of this software and associated documentation files (the
%%% "Software"), to deal in the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software,
%%% and to permit persons to whom the Software is furnished to do so,
%%% subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%========================================================================
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
