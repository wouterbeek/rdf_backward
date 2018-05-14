:- module(
  rdf_proof_export,
  [
    export_proof/2, % +File, +Proof
    export_proof/3, % +File, +Proof, +Options
    view_proof/1    % +Proof
  ]
).

/** <module> Exports RDF proof trees using the GraphViz DOT format

@author Wouter Beek
@version 2018
*/

:- use_module(library(option)).
:- use_module(library(yall)).

:- use_module(library(graph/graph_export)).
:- use_module(library(sw/rdf_proof_print), []).





%! export_proof(+File:atom, +Proof:compound) is det.
%! export_proof(+File:atom, +Proof:compound, +Options:list(compound)) is det.

export_proof(File, Proof) :-
  export_proof(File, Proof, []).

export_proof(File, Proof, Options1) :-
  merge_options([directed(true)], Options1, Options2),
  export_graph(File, {Proof}/[Out]>>export_proof_stream(Out, Proof), Options2).



%! view_proof(+Proof:compound) is det.

view_proof(Proof) :-
  view_graph({Proof}/[Out]>>export_proof_stream(Out, Proof), [directed(true)]).





% GENERICS %

%! export_proof_stream(+Out:stream, +Proof:compound) is det.

export_proof_stream(Out, t(Rule,Concl,Prems)) :-
  with_output_to(string(ConclLabel), rdf_proof_print:pp_tp(Concl)),
  dot_node(Out, Concl, [label(ConclLabel)]),
  with_output_to(string(RuleLabel), rdf_proof_print:pp_rule(Rule)),
  dot_node(Out, t(Rule,Concl,Prems), [label(RuleLabel)]),
  dot_arc(Out, Concl, t(Rule,Concl,Prems)),
  maplist(export_subproof_stream(Out, t(Rule,Concl,Prems)), Prems).

export_subproof_stream(Out, Node, t(Rule,Concl,Prems)) :-
  with_output_to(string(ConclLabel), rdf_proof_print:pp_tp(Concl)),
  dot_node(Out, Concl, [label(ConclLabel)]),
  dot_arc(Out, Node, Concl),
  export_proof_stream(Out, t(Rule,Concl,Prems)).
