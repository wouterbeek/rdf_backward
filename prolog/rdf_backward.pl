:- encoding(utf8).
:- module(
  rdf_backward,
  [
    rdf_proof/1, % ?Conclusion
    rdf_proof/2, % +Dataset, ?Conclusion
    rdf_proof/3, % +Dataset, ?Conclusion, -Proof
    rdf_prove/1, % ?Conclusion
    rdf_prove/2  % +Dataset, ?Conclusion
  ]
).
:- reexport(library(rdf_api)).

/** <module> RDF(S) entailment through mode-directed tabling

*/

:- use_module(library(apply)).
:- use_module(library(solution_sequences)).
:- use_module(library(when)).

:- use_module(library(proof)). % Used in lattice/1.
:- use_module(library(rdf_prefix)).
:- use_module(library(rdf_proof)).
:- use_module(library(rdf_term)).

:- dynamic
    recognized_datatype_iri/1.

:- rdf_meta
   axiom(?, t),
   rdf_proof(t),
   rdf_proof(+, t),
   rdf_proof(+, t, -),
   rdf_prove(t),
   rdf_prove(+, t),
   recognized_datatype_iri(r),
   rule(+, ?, t, t).

:- table
   rdf_proof(_,_,lattice(shortest_proof)).





%! rdf_proof(++Conclusion:compound) is semidet.
%! rdf_proof(+Conclusion:compound) is nondet.
%! rdf_proof(-Conclusion:compound) is multi.

rdf_proof(Concl) :-
  create_dataset([], [], Dataset),
  rdf_proof(Dataset, Concl).


%! rdf_proof(+Dataset:compound, ++Conclusion:compound) is semidet.
%! rdf_proof(+Dataset:compound, +Conclusion:compound) is nondet.
%! rdf_proof(+Dataset:compound, -Conclusion:compound) is multi.

rdf_proof(Dataset, Concl) :-
  rdf_proof(Dataset, Concl, Proof),
  rdf_view_proof(Proof).


%! rdf_proof(+Dataset:compound, ++Conclusion:compound, -Proof:compound) is semidet.
%! rdf_proof(+Dataset:compound, +Conclusion:compound, -Proof:compound) is nondet.
%! rdf_proof(+Dataset:compound, -Conclusion:compound, -Proof:compound) is multi.

rdf_proof(Dataset, Concl, p(Rule,Concl,Proofs)) :-
  rule(Dataset, Rule, Concl, Prems),
  maplist(rdf_proof(Dataset), Prems, Proofs).



%! rdf_prove(++Conclusion:compound) is semidet.
%! rdf_prove(+Conclusion:compound) is nondet.

rdf_prove(Concl) :-
  create_dataset([], [], Dataset),
  rdf_prove(Dataset, Concl).


%! rdf_prove(+Dataset:compound, ++Conclusion:compound) is semidet.
%! rdf_prove(+Dataset:compound, +Conclusion:compound) is nondet.

rdf_prove(Dataset, Concl) :-
  distinct(Concl, rdf_proof(Dataset, Concl, _)).



%! axiom(?Vocab:term, ?Triple:compound) is nondet.

axiom(rdf,  tp(rdf:type,           rdf:type,           rdf:'Property'  )).
axiom(rdf,  tp(rdf:subject,        rdf:type,           rdf:'Property'  )).
axiom(rdf,  tp(rdf:predicate,      rdf:type,           rdf:'Property'  )).
axiom(rdf,  tp(rdf:object,         rdf:type,           rdf:'Property'  )).
axiom(rdf,  tp(rdf:first,          rdf:type,           rdf:'Property'  )).
axiom(rdf,  tp(rdf:rest,           rdf:type,           rdf:'Property'  )).
axiom(rdf,  tp(rdf:value,          rdf:type,           rdf:'Property'  )).
axiom(rdf,  tp(rdf:nil,            rdf:type,           rdf:'List'      )).
axiom(rdf,  tp(P,                  rdf:type,           rdf:'Property'  )) :-
  when(ground(P), rdf_is_predicate(P)),
  rdf_container_membership_property(P).
axiom(rdfs, tp(rdf:type,           rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, tp(rdfs:domain,        rdfs:domain,        rdf:'Property'  )).
axiom(rdfs, tp(rdfs:range,         rdfs:domain,        rdf:'Property'  )).
axiom(rdfs, tp(rdfs:subPropertyOf, rdfs:domain,        rdf:'Property'  )).
axiom(rdfs, tp(rdfs:subClassOf,    rdfs:domain,        rdfs:'Class'    )).
axiom(rdfs, tp(rdf:subject,        rdfs:domain,        rdf:'Statement' )).
axiom(rdfs, tp(rdf:predicate,      rdfs:domain,        rdf:'Statement' )).
axiom(rdfs, tp(rdf:object,         rdfs:domain,        rdf:'Statement' )).
axiom(rdfs, tp(rdfs:member,        rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, tp(rdf:first,          rdfs:domain,        rdf:'List'      )).
axiom(rdfs, tp(rdf:rest,           rdfs:domain,        rdf:'List'      )).
axiom(rdfs, tp(rdfs:seeAlso,       rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, tp(rdfs:isDefinedBy,   rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, tp(rdfs:comment,       rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, tp(rdfs:label,         rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, tp(rdf:value,          rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, tp(rdf:type,           rdfs:range,         rdfs:'Class'    )).
axiom(rdfs, tp(rdfs:domain,        rdfs:range,         rdfs:'Class'    )).
axiom(rdfs, tp(rdfs:range,         rdfs:range,         rdfs:'Class'    )).
axiom(rdfs, tp(rdfs:subPropertyOf, rdfs:range,         rdf:'Property'  )).
axiom(rdfs, tp(rdfs:subClassOf,    rdfs:range,         rdfs:'Class'    )).
axiom(rdfs, tp(rdf:subject,        rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdf:predicate,      rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdf:object,         rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdfs:member,        rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdf:first,          rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdf:rest,           rdfs:range,         rdf:'List'      )).
axiom(rdfs, tp(rdfs:seeAlso,       rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdfs:isDefinedBy,   rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdfs:comment,       rdfs:range,         rdfs:'Literal'  )).
axiom(rdfs, tp(rdfs:label,         rdfs:range,         rdfs:'Literal'  )).
axiom(rdfs, tp(rdf:value,          rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdf:'Alt',          rdfs:subClassOf,    rdfs:'Container')).
axiom(rdfs, tp(rdf:'Bag',          rdfs:subClassOf,    rdfs:'Container')).
axiom(rdfs, tp(rdf:'Seq',          rdfs:subClassOf,    rdfs:'Container')).
axiom(rdfs, tp(rdfs:'ContainerMembershipProperty', rdfs:subClassOf, rdf:'Property')).
axiom(rdfs, tp(rdfs:isDefinedBy,   rdfs:subPropertyOf, rdfs:seeAlso    )).
axiom(rdfs, tp(rdfs:'Datatype',    rdfs:subClassOf,    rdfs:'Class'    )).
axiom(rdfs, tp(P,                  rdf:type,           rdfs:'ContainerMembershipProperty')) :-
  when(ground(P), rdf_is_predicate(P)),
  rdf_container_membership_property(P).
axiom(rdfs, tp(P,                  rdfs:domain,        rdfs:'Resource' )) :-
  when(ground(P), rdf_is_predicate(P)),
  rdf_container_membership_property(P).
axiom(rdfs, tp(P,                  rdfs:range,         rdfs:'Resource' )) :-
  when(ground(P), rdf_is_predicate(P)),
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



%! rule(+Dataset:compound,
%!      ?Rule:compound,
%!      ?Conclusion:compound,
%!      -Premises:list(compound)) is nondet.

rule(Dataset, db, tp(S,P,O), []) :-
  when(ground(S), rdf_is_subject(S)),
  when(ground(P), rdf_is_predicate(P)),
  tp(S, P, O, Dataset).
rule(_, axiom(Vocab), Concl, []) :-
  axiom(Vocab, Concl).
rule(_, rdf(1),     tp(literal(lang(LTag,Lex)),rdf:type,rdf:langString), [tp(_,_,literal(lang(LTag,Lex)))]) :-
  ground(LTag-Lex).
rule(_, rdf(1),     tp(literal(type(D,Val)),rdf:type,D),   [tp(_,_,literal(type(D,Val)))]) :-
  ground(D-Val),
  recognized_datatype_iri(D).
rule(_, rdf(2),     tp(P,rdf:type,rdf:'Property'),         [tp(_,P,_)]).
rule(_, rdfs(1),    tp(D,rdf:type,rdfs:'Datatype'),        []) :-
  (rdf_equal(D, rdf:langString) ; recognized_datatype_iri(D)).
rule(_, rdfs(2),    tp(I,rdf:type,C),                      [tp(P,rdfs:domain,C),tp(I,P,_)]).
rule(_, rdfs(3),    tp(I,rdf:type,C),                      [tp(P,rdfs:range,C),tp(_,P,I)]).
rule(_, rdfs('4a'), tp(I,rdf:type,rdfs:'Resource'),        [tp(I,_,_)]).
rule(_, rdfs('4b'), tp(I,rdf:type,rdfs:'Resource'),        [tp(_,_,I)]).
rule(_, rdfs(5),    tp(P, rdfs:subPropertyOf, R),          [tp(P,rdfs:subPropertyOf,Q),tp(Q,rdfs:subPropertyOf,R)]).
rule(_, rdfs(6),    tp(P, rdfs:subPropertyOf, P),          [tp(P,rdf:type,rdf:'Property')]).
rule(_, rdfs(7),    tp(X,Q,Y),                             [tp(P,rdfs:subPropertyOf,Q),tp(X,P,Y)]).
rule(_, rdfs(8),    tp(C,rdfs:subClassOf,rdfs:'Resource'), [tp(C,rdf:type,rdfs:'Class')]).
rule(_, rdfs(9),    tp(I,rdf:type,D),                      [tp(C,rdfs:subClassOf,D),tp(I,rdf:type,C)]).
rule(_, rdfs(10),   tp(C,rdfs:subClassOf,C),               [tp(C,rdf:type,rdfs:'Class')]).
rule(_, rdfs(11),   tp(C,rdfs:subClassOf,E),               [tp(C,rdfs:subClassOf,D),tp(D,rdfs:subClassOf,E)]).
rule(_, rdfs(12),   tp(P,rdfs:subPropertyOf,rdfs:member),  [tp(P,rdf:type,rdfs:'ContainerMembershipProperty')]).
rule(_, rdfs(13),   tp(C,rdfs:subClassOf,rdfs:'Literal'),  [tp(C,rdf:type,rdfs:'Datatype')]).
