:- module(
  rdf_proof_export,
  [
    export_proof/2, % +File, +Proof
    view_pdf/1,     % +File
    view_proof/1    % +Proof
  ]
).

/** <module> Exports RDF proof trees using the GraphViz DOT format

# Installation

This requires GraphViz command `dot' to be available from PATH.

```sh
sudo apt install graphviz # Debian, Ubuntu
sudo dnf install graphviz # Red Hat, Fedora
```

# Debug info

In order to display the export output, run `?- debug(gv).'

---

@author Wouter Beek
@version 2018
*/

:- use_module(library(debug)).
:- use_module(library(md5)).
:- use_module(library(process)).

:- use_module(rdf_proof_print, []).





%! export_proof(+File:atom, +Proof:compound) is det.

export_proof(File, Tree) :-
  setup_call_cleanup(
    open(File, write, Out, [type(binary)]),
    setup_call_cleanup(
      (
        process_create(path(dot), ['-Tpdf'], [stdin(pipe(ProcIn)),stdout(pipe(ProcOut))]),
        set_stream(ProcOut, type(binary))
      ),
      (
        call_cleanup(
          export_proof_stream(ProcIn, Tree),
          close(ProcIn)
        ),
        copy_stream_data(ProcOut, Out)
      ),
      close(ProcOut)
    ),
    close(Out)
  ).



%! view_pdf(+File:atom) is det.

view_pdf(File) :-
  process_create(path(evince), [file(File)], []).



%! view_proof(+Proof:compound) is det.

view_proof(Tree) :-
  setup_call_cleanup(
    process_create(path(dot), ['-Tgtk'], [stdin(pipe(ProcIn))]),
    export_proof_stream(ProcIn, Tree),
    close(ProcIn)
  ).





% GENERICS %

export_proof_stream(Out, Tree) :-
  format_debug(gv, Out, "digraph g {", []),
  export_tree(Out, Tree),
  format_debug(gv, Out, "}", []).

export_tree(Out, t(Rule,Concl,Prems)) :-
  gv_node_id(Concl, X),
  with_output_to(string(XLabel), rdf_proof_print:pp_tp(Concl)),
  gv_node(Out, X, [label(XLabel)]),
  gv_node_id([Rule,Concl,Prems], Y),
  with_output_to(string(XYLabel), rdf_proof_print:pp_rule(Rule)),
  gv_node(Out, Y, [label(XYLabel)]),
  gv_edge(Out, X, Y),
  maplist(export_subtree(Out, Y), Prems).
  
export_subtree(Out, Y, t(Rule,Concl,Prems)) :-
  gv_node_id(Concl, Z),
  with_output_to(string(ZLabel), rdf_proof_print:pp_tp(Concl)),
  gv_node(Out, Z, [label(ZLabel)]),
  gv_edge(Out, Y, Z),
  export_tree(Out, t(Rule,Concl,Prems)).

gv_attribute(Attr, Str) :-
  Attr =.. [Name,Value],
  gv_attribute(Name, Value, Str).

gv_attribute(label, Values, Str) :-
  is_list(Values), !,
  maplist(gv_html_replace, Values, Strs),
  atomics_to_string(Strs, "<BR/>", Str0),
  format(string(Str), "label=<~s>", [Str0]).
gv_attribute(label, Value0, Str) :- !,
  gv_html_replace(Value0, Value),
  format(string(Str), "label=<~s>", [Value]).

gv_attributes(Attrs, Str) :-
  maplist(gv_attribute, Attrs, Strs),
  atomics_to_string(Strs, ",", Str).

gv_edge(Out, FromId, ToId) :-
  format_debug(gv, Out, "  ~a -> ~a;", [FromId,ToId]).

gv_node(Out, Id, Attrs) :-
  gv_attributes(Attrs, Str),
  format_debug(gv, Out, "  ~a [~s];", [Id,Str]).

gv_node_id(Term, Id) :-
  term_to_atom(Term, Atom),
  md5_hash(Atom, Hash, []),
  atomic_concat(n, Hash, Id).

gv_html_replace(Str1, Str2) :-
  string_codes(Str1, Cs1),
  phrase(html_replace, Cs1, Cs2),
  string_codes(Str2, Cs2).

html_replace, "&lt;" --> "<", !, html_replace.
html_replace, "&gt;" --> ">", !, html_replace.
html_replace, "&amp;" --> "&", !, html_replace.
html_replace, [C] --> [C], !, html_replace.
html_replace --> "".





% HELPERS %

format_debug(Flag, Out, Pattern) :-
  format_debug(Flag, Out, Pattern, []).


format_debug(Flag, Out, Pattern, Args) :-
  string_concat(Pattern, "\n", PatternNewline),
  format(Out, PatternNewline, Args),
  debug(Flag, Pattern, Args).
