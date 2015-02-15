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

-compile(export_all).
-export([parse_transform/2]).

%% ========================================================================
%%  Macro definitions
%% ========================================================================

-define(DOC(Var), {attribute, _, doc, Var}).
-define(FUN(Var1, Var2), {function, _, Var1, Var2, _}).
-define(SPEC(Var1, Var2, Var3),
        {attribute, _, spec,
         {{Var1, _},
          [{type, _, 'fun',
            [{type, _, product, Var2}, Var3]}]}}).

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
parse([?DOC(Doc), ?FUN(Name, Arity)| Forms], Acc) ->
    parse(Forms, [{Name, Arity, doc_string(Name, Arity, Doc)}| Acc]);
parse([?DOC(Doc), ?SPEC(Name, Args, Return), ?FUN(Name, Arity)| Forms], Acc) ->
    Spec = printable_spec(Name, Args, Return),
    parse(Forms, [{Name, Arity, doc_string(Name, Arity, Spec, Doc)}| Acc]);
parse([_| Forms], Acc) ->
    parse(Forms, Acc).

doc_string(Name, Arity, Doc) ->
    lists:flatten(
      string:join([
                   io_lib:format("### ~p/~p", [Name, Arity]),
                   Doc
                  ],
                  "\n")).

doc_string(Name, Arity, Spec, Doc) ->
    lists:flatten(
      string:join([
                   io_lib:format("### ~p/~p", [Name, Arity]),
                   Spec,
                   Doc
                  ],
                  "\n")).

%% TODO: Investigate why forms:from_abstract/1 does not work well with
%%       specs.
printable_spec(Name, Args, Return) ->
    io_lib:format("    ~p(~s) -> ~s",
                  [
                   Name,
                   printable_args(Args),
                   printable_type(Return)
                  ]).

printable_args(Args) ->
    Args1 = [ printable_arg(A) || A <- Args ],
    string:join(Args1, ", ").

printable_arg({ann_type, _, [{var, _, Var}, Type]}) ->
    io_lib:format("~p :: ~s", [Var, printable_type(Type)]);
printable_arg(Type) ->
    printable_type(Type).

printable_type({type, _, union, Types}) ->
    Types1 = [ printable_type(T) || T <- Types ],
    string:join(Types1, " | ");
printable_type({type, _, Name, Types}) ->
    Types1 = [ printable_type(T) || T <- Types ],
    Types2 = string:join(Types1, ", "),
    io_lib:format("~p(~s)", [Name, Types2]);
printable_type({_, _, T}) ->
    io_lib:format("~p", [T]).

delete_doc_attrs(Forms) ->
    lists:foldr(fun({attribute, _, doc, _}, Acc) ->
                        Acc;
                   (Other, Acc) ->
                        [Other| Acc]
                end,
                [],
                Forms).
