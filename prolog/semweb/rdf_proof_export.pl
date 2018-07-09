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
  export_graph(File, {Tree}/[Out]>>export_proof_(Out, Tree), [directed(true)]).



%! view_proof(+Proof:compound) is det.

view_proof(Tree) :-
  view_graph({Tree}/[Out]>>export_proof_(Out, Tree), [directed(true)]).





% GENERICS %

export_proof_(Out, Tree) :-
  Tree = t(Rule,Concl,SubTrees),
  % conclusion node
  with_output_to(string(ConclLabel), rdf_proof_print:pp_tp(Concl)),
  dot_node(Out, Concl, [label(ConclLabel)]),
  % rule application node
  with_output_to(string(RuleLabel), rdf_proof_print:pp_rule(Rule)),
  dot_node(Out, Tree, [label(RuleLabel)]),
  % arc from conclusion to rule application node
  dot_arc(Out, Concl, Tree),
  maplist(export_subproof_(Out, Tree), SubTrees).

export_subproof_(Out, Tree, SubTree) :-
  SubTree = t(_,Prem,_),
  with_output_to(string(PremLabel), rdf_proof_print:pp_tp(Prem)),
  dot_node(Out, Prem, [label(PremLabel)]),
  dot_arc(Out, Tree, Prem),
  export_proof_(Out, SubTree).
