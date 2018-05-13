:- module(
  rdf_proof_export,
  [
    rdf_proof_view/1 % +Proof
  ]
).

/** <module> Exports RDF proof trees using the GraphViz DOT format

@author Wouter Beek
@version 2018
*/

:- use_module(library(graph/graph_export)).
:- use_module(library(yall)).

:- use_module(rdf_proof_print, []).



%! rdf_proof_export(+Proof:compound) is det.

rdf_proof_view(Proof) :-
  view_graph({Proof}/[Out]>>export_proof(Out, Proof), [directed(true)]).

export_proof(Out, t(Rule,Concl,Prems)) :-
  with_output_to(string(ConclLabel), rdf_proof_print:pp_tp(Concl)),
  dot_node(Out, Concl, [label(ConclLabel)]),
  with_output_to(string(RuleLabel), rdf_proof_print:pp_rule(Rule)),
  dot_node(Out, t(Rule,Concl,Prems), [label(RuleLabel)]),
  dot_arc(Out, Concl, t(Rule,Concl,Prems)),
  maplist(export_subproof(Out, t(Rule,Concl,Prems)), Prems).
  
export_subproof(Out, Node, t(Rule,Concl,Prems)) :-
  with_output_to(string(ConclLabel), rdf_proof_print:pp_tp(Concl)),
  dot_node(Out, Concl, [label(ConclLabel)]),
  dot_arc(Out, Node, Concl),
  export_proof(Out, t(Rule,Concl,Prems)).
