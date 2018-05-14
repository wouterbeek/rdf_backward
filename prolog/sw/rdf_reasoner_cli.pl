:- module(
  rdf_reasoner_cli,
  [
  % SETUP
    add_axiom/2,                   % +Vocab, +Statement
    add_recognized_datatype_iri/1, % +D
    add_rule/3,                    % +Rule, +Conclusion, +Premises
    add_statement/1,               % +Statement
    add_statement/2,               % +Statement, +G
  % REASONING
    prove/1,                       % ?Conclusion
    prove_tree/1,                  % ?Conclusion
    prove_tree/2                   % ?Conclusion, -Proof
  ]
).

/** <module> RDF(S) reasoner CLI

@author Wouter Beek
@version 2018
*/

:- use_module(library(settings)).

:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_proof_export)).
:- use_module(library(sw/rdf_reasoner)).
:- use_module(library(sw/rdf_term)).

:- dynamic
    axiom/2,
    recognized_datatype_iri/1,
    rule/3.

:- initialization
   rdf_assert_prefixes.

:- multifile
    axiom/2,
    recognized_datatype_iri/1,
    rule/3.

:- rdf_meta
   add_axiom(+, t),
   add_recognized_datatype_iri(r),
   add_rule(+, t, t),
   add_statement(t),
   add_statement(t, r),
   prove(t),
   prove_tree(t),
   prove_tree(t, -).

:- setting(stage, oneof([setup,reasoning]), setup,
           "The stage in which the reasoner is currently.").





/* SETUP STAGE

Implementation of the RDF(S) reasoner's setup stage.

This stage allows axioms, recognized datatype IRIs, rules, are database
statements to be added.  Once everything has been added, the setup
stage is ended by invoking one of the reasoning predicates.
*/

%! add_axiom(+Vocab:atom, +Triple:compound) is det.

add_axiom(Vocab, Triple) :-
  assume_setup_stage(add_axiom/2),
  (axiom(Vocab, Triple) -> true ; assertz(axiom(Vocab, Triple))).



%! add_recognized_datatype_iri(+D:atom) is det.

add_recognized_datatype_iri(D) :-
  assume_setup_stage(add_recognized_datatype_iri/1),
  (recognized_datatype_iri(D) -> true ; assertz(recognized_datatype_iri(D))).



%! add_rule(+Rule:compound, +Conclusion:compound, +Premises:list(compound)) is det.

add_rule(Rule, Concl, Prems) :-
  assume_setup_stage(add_rule/1),
  (rule(Rule, _, _) -> true ; assertz(rule(Rule,Concl,Prems))).



%! add_statement(+Statement:compound) is det.

add_statement(rdf(S,P,O)) :-
  assume_setup_stage(add_statement/1),
  rdf_assert_triple(S, P, O).


%! add_statement(+Statement:compound, +G:atom) is det.

add_statement(rdf(S,P,O), G) :-
  assume_setup_stage(add_statement/2),
  rdf_assert_triple(S, P, O, G).



%! add_container_memberchip_property_axioms is det.

add_container_memberchip_property_axioms :-
  forall(
    rdf_container_membership_property(P),
    (
      add_axiom(rdf, rdf(P, rdf:type, rdf:'Property')),
      add_axiom(rdfs, rdf(P, rdf:type, rdfs:'ContainerMembershipProperty')),
      add_axiom(rdfs, rdf(P, rdfs:domain, rdfs:'Resource')),
      add_axiom(rdfs, rdf(P, rdfs:range, rdfs:'Resource'))
    )
  ).





/* REASONING STAGE

Implementation of the reasoner itself.  Invoking one of the prove
predicates automatically end setup stage and enters reasoning stage.
*/

prove(Concl) :-
  set_stage(reasoning),
  rdf_prove(Concl).



%! prove_tree(++Conclusion:compound) is semidet.
%! prove_tree(+Conclusion:compound) is nondet.
%! prove_tree(-Conclusion:compound) is multi.

prove_tree(Concl) :-
  rdf_prove_tree(Concl, Proof),
  view_proof(Proof).


%! prove_tree(++Conclusion:compound, -Proof:compound) is semidet.
%! prove_tree(+Conclusion:compound, -Proof:compound) is nondet.
%! prove_tree(-Conclusion:compound, -Proof:compound) is multi.

prove_tree(Concl, Proof) :-
  set_stage(reasoning),
  rdf_prove_tree(Concl, Proof).





% HELPERS %

%! assume_setup_stage(+Predicate:compound) is det.
%
% @throws error(stage(reasoning),Context)

assume_setup_stage(Pred/Arity) :-
  setting(stage, reasoning), !,
  throw(error(stage(reasoning),Pred/Arity)).
assume_setup_stage(_).

prolog:message(error(stage(Stage),Pred/Arity)) -->
  ["Call to ‘~a/~d’ not allowed in ~a stage."-[Pred,Arity,Stage]].



%! set_stage(+Stage:oneof([reasoning])) is det.

set_stage(reasoning) :-
  setting(stage, reasoning), !.
set_stage(reasoning) :-
  add_container_memberchip_property_axioms,
  set_setting(stage, reasoning).
