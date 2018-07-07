:- module(
  rdf_proof_export,
  [
    export_proof/2, % +File, +Proof
    view_proof/1    % +Proof
  ]
).

/** <module> Exports RDF proof trees using the GraphViz DOT format

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(yall)).

:- use_module(library(graph/graph_export)).
:- use_module(library(semweb/rdf_proof_print)).





%! export_proof(+File:atom, +Proof:compound) is det.

export_proof(File, Tree) :-
  export_graph(File, {Tree}/[Out]>>write_tree(Out, Tree), [directed(true)]).



%! view_proof(+Proof:compound) is det.

view_proof(Tree) :-
  view_graph({Tree}/[Out]>>write_tree(Out, Tree), [directed(true)]).





% GENERICS %

write_tree(Out, t(Rule,Concl,Prems)) :-
  dot_node_id(Concl, X),
  with_output_to(string(XLabel), rdf_proof_print:pp_tp(Concl)),
  dot_node(Out, X, [label(XLabel)]),
  dot_node_id([Rule,Concl,Prems], Y),
  with_output_to(string(XYLabel), rdf_proof_print:pp_rule(Rule)),
  dot_node(Out, Y, [label(XYLabel)]),
  dot_arc(Out, X, Y),
  maplist(write_subtree(Out, Y), Prems).

write_subtree(Out, Y, t(Rule,Concl,Prems)) :-
  dot_node_id(Concl, Z),
  with_output_to(string(ZLabel), rdf_proof_print:pp_tp(Concl)),
  dot_node(Out, Z, [label(ZLabel)]),
  dot_arc(Out, Y, Z),
  write_tree(Out, t(Rule,Concl,Prems)).
