:- encoding(utf8).
:- module(
  rdf_proof,
  [
    rdf_export_proof/2, % +File, +Proof
    rdf_view_proof/1    % +Proof
  ]
).

/** <module> Support predicates for RDF proofs

*/

:- use_module(library(apply)).
:- use_module(library(yall)).

:- use_module(library(dcg)).
:- use_module(library(dot)).
:- use_module(library(gv)).
:- use_module(library(semweb/rdf_print)).



%! rdf_export_proof(+File:atom, +Proof:tree) is det.

rdf_export_proof(File, Proof) :-
  gv_export(File, {Proof}/[Out]>>export_proof(Out, Proof), options{directed: true}).



%! rdf_view_proof(+Proof:tree) is det.

rdf_view_proof(Proof) :-
  gv_view({Proof}/[Out]>>export_proof(Out, Proof), options{directed: true}).



% GENERICS %

%! export_proof(+Out:ostream, +Proof:tree) is det

export_proof(Out, Proof) :-
  Proof = tree(Rule,Concl,Subproofs),
  % conclusion node
  dcg_with_output_to(string(ConclLabel), rdf_dcg_triple(Concl, options{pp: true})),
  dot_node(Out, Concl, options{label: ConclLabel}),
  % rule application node
  format(string(RuleLabel), "[~w]", [Rule]),
  dot_node(Out, Proof, options{label: RuleLabel}),
  % arc from conclusion to rule application node
  dot_arc(Out, Concl, Proof),
  maplist(export_subproof(Out, Proof), Subproofs).

export_subproof(Out, Proof, Subproof) :-
  Subproof = tree(_,Prem,_),
  dcg_with_output_to(string(PremLabel), rdf_dcg_triple(Prem)),
  dot_node(Out, Prem, options{label: PremLabel}),
  dot_arc(Out, Proof, Prem),
  export_proof(Out, Subproof).
