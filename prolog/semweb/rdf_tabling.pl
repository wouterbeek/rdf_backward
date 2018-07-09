:- module(
  rdf_tabling,
  [
  % SETUP
    add_axiom/2,                   % +Vocab, +Statement
    add_recognized_datatype_iri/1, % +D
    add_rule/3,                    % +Rule, +Conclusion, +Premises
    add_statement/1,               % +Statement
    add_statement/2,               % +Statement, +G
  % REASONING
    rdf_proof/1,                   % ?Conclusion
    rdf_proof_tree/1,              % ?Conclusion
    rdf_proof_tree/2               % ?Conclusion, -Proof
  ]
).

/** <module> RDF(S) entailment through mode-directed tabling

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(settings)).

:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_proof_export)).
:- use_module(library(semweb/rdf_term)).

:- dynamic
    axiom/2,
    recognized_datatype_iri/1,
    rule/3.

:- multifile
    axiom/2,
    recognized_datatype_iri/1,
    rule/3,
    prolog:message//1.

:- rdf_meta
   add_axiom(+, t),
   add_recognized_datatype_iri(r),
   add_rule(+, t, t),
   add_statement(t),
   add_statement(t, r),
   axiom(?, t),
   rdf_proof(t),
   rdf_proof_tree(t),
   rdf_proof_tree(t, -),
   recognized_datatype_iri(r),
   rule(?, t, t).

:- setting(stage, oneof([setup,reasoning]), setup,
           "The stage in which the reasoner is currently.").

:- table
   rdf_proof_/1,
   rdf_proof_(_,lattice(shortest_proof)).





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



%! axiom(?Vocab:term, ?Triple:compound) is nondet.

axiom(rdf,  rdf(rdf:type,           rdf:type,           rdf:'Property'  )).
axiom(rdf,  rdf(rdf:subject,        rdf:type,           rdf:'Property'  )).
axiom(rdf,  rdf(rdf:predicate,      rdf:type,           rdf:'Property'  )).
axiom(rdf,  rdf(rdf:object,         rdf:type,           rdf:'Property'  )).
axiom(rdf,  rdf(rdf:first,          rdf:type,           rdf:'Property'  )).
axiom(rdf,  rdf(rdf:rest,           rdf:type,           rdf:'Property'  )).
axiom(rdf,  rdf(rdf:value,          rdf:type,           rdf:'Property'  )).
axiom(rdf,  rdf(rdf:nil,            rdf:type,           rdf:'List'      )).
axiom(rdf,  rdf(P,                  rdf:type,           rdf:'Property'  )) :-
  rdf_check_predicate(P),
  rdf_container_membership_property(P).
axiom(rdfs, rdf(rdf:type,           rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, rdf(rdfs:domain,        rdfs:domain,        rdf:'Property'  )).
axiom(rdfs, rdf(rdfs:range,         rdfs:domain,        rdf:'Property'  )).
axiom(rdfs, rdf(rdfs:subPropertyOf, rdfs:domain,        rdf:'Property'  )).
axiom(rdfs, rdf(rdfs:subClassOf,    rdfs:domain,        rdfs:'Class'    )).
axiom(rdfs, rdf(rdf:subject,        rdfs:domain,        rdf:'Statement' )).
axiom(rdfs, rdf(rdf:predicate,      rdfs:domain,        rdf:'Statement' )).
axiom(rdfs, rdf(rdf:object,         rdfs:domain,        rdf:'Statement' )).
axiom(rdfs, rdf(rdfs:member,        rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, rdf(rdf:first,          rdfs:domain,        rdf:'List'      )).
axiom(rdfs, rdf(rdf:rest,           rdfs:domain,        rdf:'List'      )).
axiom(rdfs, rdf(rdfs:seeAlso,       rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, rdf(rdfs:isDefinedBy,   rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, rdf(rdfs:comment,       rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, rdf(rdfs:label,         rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, rdf(rdf:value,          rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, rdf(rdf:type,           rdfs:range,         rdfs:'Class'    )).
axiom(rdfs, rdf(rdfs:domain,        rdfs:range,         rdfs:'Class'    )).
axiom(rdfs, rdf(rdfs:range,         rdfs:range,         rdfs:'Class'    )).
axiom(rdfs, rdf(rdfs:subPropertyOf, rdfs:range,         rdf:'Property'  )).
axiom(rdfs, rdf(rdfs:subClassOf,    rdfs:range,         rdfs:'Class'    )).
axiom(rdfs, rdf(rdf:subject,        rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, rdf(rdf:predicate,      rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, rdf(rdf:object,         rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, rdf(rdfs:member,        rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, rdf(rdf:first,          rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, rdf(rdf:rest,           rdfs:range,         rdf:'List'      )).
axiom(rdfs, rdf(rdfs:seeAlso,       rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, rdf(rdfs:isDefinedBy,   rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, rdf(rdfs:comment,       rdfs:range,         rdfs:'Literal'  )).
axiom(rdfs, rdf(rdfs:label,         rdfs:range,         rdfs:'Literal'  )).
axiom(rdfs, rdf(rdf:value,          rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, rdf(rdf:'Alt',          rdfs:subClassOf,    rdfs:'Container')).
axiom(rdfs, rdf(rdf:'Bag',          rdfs:subClassOf,    rdfs:'Container')).
axiom(rdfs, rdf(rdf:'Seq',          rdfs:subClassOf,    rdfs:'Container')).
axiom(rdfs, rdf(rdfs:'ContainerMembershipProperty', rdfs:subClassOf, rdf:'Property')).
axiom(rdfs, rdf(rdfs:isDefinedBy,   rdfs:subPropertyOf, rdfs:seeAlso    )).
axiom(rdfs, rdf(rdfs:'Datatype',    rdfs:subClassOf,    rdfs:'Class'    )).
axiom(rdfs, rdf(P,                  rdf:type,           rdfs:'ContainerMembershipProperty')) :-
  rdf_check_predicate(P),
  rdf_container_membership_property(P).
axiom(rdfs, rdf(P,                  rdfs:domain,        rdfs:'Resource' )) :-
  rdf_check_predicate(P),
  rdf_container_membership_property(P).
axiom(rdfs, rdf(P,                  rdfs:range,         rdfs:'Resource' )) :-
  rdf_check_predicate(P),
  rdf_container_membership_property(P).



%! recognized_datatype_iri(+Iri:atom) is semidet.
%! recognized_datatype_iri(-Iri:atom) is multi.
%
% The recognized datatype IRIs supported by this reasoner.
%
% Datatype IRI `rdf:langString' is recognized, but is not asserted,
% since language-tagged strings are implemented by a different Prolog
% syntax.
%
% By default, the set of recognized datatype IRIs includes only
% `rdf:langString' and `xsd:string'.  This set can be extended by
% asserting Prolog terms of the form `recognized_datatype_iri(+atom)'.

recognized_datatype_iri(xsd:string).



%! rule(?Rule:compound, ?Conclusion:compound, -Premises:list(compound)) is nondet.

rule(db(G),        rdf(S,P,O),                             []) :-
  rdf_check_subject(S),
  rdf_check_predicate(P),
  rdf_triple(S, P, O, G).
rule(axiom(Vocab), Concl,                                  []) :-
  axiom(Vocab, Concl).
rule(rdf(1),       rdf(literal(lan(LTag,Lex)),rdf:type,rdf:langString), [rdf(_S,_P,literal(lang(LTag,Lex)))]) :-
  ground(LTag-Lex).
rule(rdf(1),       rdf(literal(type(D,Val)),rdf:type,D),   [rdf(_S,_P,literal(type(D,Val)))]) :-
  ground(D-Val),
  recognized_datatype_iri(D).
rule(rdf(2),       rdf(P,rdf:type,rdf:'Property'),         [rdf(_S,P,_O)]).
rule(rdfs(1),      rdf(D,rdf:type,rdfs:'Datatype'),        []) :-
  (rdf_equal(D, rdf:langString) ; recognized_datatype_iri(D)).
rule(rdfs(2),      rdf(I,rdf:type,C),                      [rdf(P,rdfs:domain,C),rdf(I,P,_)]).
rule(rdfs(3),      rdf(I,rdf:type,C),                      [rdf(P,rdfs:range,C),rdf(_,P,I)]).
rule(rdfs('4a'),   rdf(I,rdf:type,rdfs:'Resource'),        [rdf(I,_,_)]).
rule(rdfs('4b'),   rdf(I,rdf:type,rdfs:'Resource'),        [rdf(_,_,I)]).
rule(rdfs(5),      rdf(P, rdfs:subPropertyOf, R),          [rdf(P,rdfs:subPropertyOf,Q),rdf(Q,rdfs:subPropertyOf,R)]).
rule(rdfs(6),      rdf(P, rdfs:subPropertyOf, P),          [rdf(P,rdf:type,rdf:'Property')]).
rule(rdfs(7),      rdf(X,Q,Y),                             [rdf(P,rdfs:subPropertyOf,Q),rdf(X,P,Y)]).
rule(rdfs(8),      rdf(C,rdfs:subClassOf,rdfs:'Resource'), [rdf(C,rdf:type,rdfs:'Class')]).
rule(rdfs(9),      rdf(I,rdf:type,D),                      [rdf(C,rdfs:subClassOf,D),rdf(I,rdf:type,C)]).
rule(rdfs(10),     rdf(C,rdfs:subClassOf,C),               [rdf(C,rdf:type,rdfs:'Class')]).
rule(rdfs(11),     rdf(C,rdfs:subClassOf,E),               [rdf(C,rdfs:subClassOf,D),rdf(D,rdfs:subClassOf,E)]).
rule(rdfs(12),     rdf(P,rdfs:subPropertyOf,rdfs:member),  [rdf(P,rdf:type,rdfs:'ContainerMembershipProperty')]).
rule(rdfs(13),     rdf(C,rdfs:subClassOf,rdfs:'Literal'),  [rdf(C,rdf:type,rdfs:'Datatype')]).





/* REASONING STAGE

Implementation of the reasoner itself.  Invoking one of the prove
predicates automatically end setup stage and enters reasoning stage.
*/

%! rdf_proof(++Conclusion:compound) is semidet.
%! rdf_proof(+Conclusion:compound) is nondet.
%! rdf_proof(-Conclusion:compound) is multi.

rdf_proof(Concl) :-
  set_stage(reasoning),
  rdf_proof_(Concl).

rdf_proof_(Concl) :-
  rule(_Rule, Concl, Prems),
  maplist(rdf_proof_, Prems).



%! rdf_proof_tree(++Conclusion:compound) is semidet.
%! rdf_proof_tree(+Conclusion:compound) is nondet.
%! rdf_proof_tree(-Conclusion:compound) is multi.

rdf_proof_tree(Concl) :-
  rdf_proof_tree(Concl, Proof),
  view_proof(Proof).


%! rdf_proof_tree(++Conclusion:compound, -Proof:compound) is semidet.
%! rdf_proof_tree(+Conclusion:compound, -Proof:compound) is nondet.
%! rdf_proof_tree(-Conclusion:compound, -Proof:compound) is multi.

rdf_proof_tree(Concl, Proof) :-
  set_stage(reasoning),
  rdf_proof_(Concl, Proof).

rdf_proof_(Concl, t(Rule,Concl,Trees)) :-
  rule(Rule, Concl, Prems),
  maplist(rdf_proof_, Prems, Trees).

shortest_proof(Tree1, Tree2, Tree) :-
  maplist(tree_depth, [Tree1,Tree2], [Depth1,Depth2]),
  (Depth2 < Depth1 -> Tree = Tree2 ; Tree = Tree1).





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



%! rdf_check_predicate(?P:atom) is semidet.

rdf_check_predicate(P) :-
  var(P), !.
rdf_check_predicate(P) :-
  rdf_is_predicate(P).



%! rdf_check_subject(?S:atom) is semidet.

rdf_check_subject(S) :-
  var(S), !.
rdf_check_subject(S) :-
  rdf_is_subject(S).



%! set_stage(+Stage:oneof([reasoning])) is det.

set_stage(Stage) :-
  setting(stage, Stage), !.
set_stage(reasoning) :-
  set_setting(stage, reasoning).
set_stage(setup) :-
  abolish_all_tables,
  setting(stage, reasoning), !.



%! tree_depth(+Tree:compound, -Depth:nonneg) is det.

tree_depth(t(_,_,[]), 0) :- !.
tree_depth(t(_,_,Trees), Depth) :-
  maplist(tree_depth, Trees, Depths),
  min_list(Depths, Depth0),
  Depth is Depth0 + 1.
