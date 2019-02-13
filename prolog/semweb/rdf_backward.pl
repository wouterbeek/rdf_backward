:- encoding(utf8).
:- module(
  rdf_backward,
  [
    rdf_proof/1, % ?Conclusion
    rdf_proof/2, % ?Conclusion, -Proof
    rdf_prove/1  % ?Conclusion
  ]
).

/** <module> RDF(S) entailment through mode-directed tabling

@author Wouter Beek
@version 2018-2019
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(solution_sequences)).
:- use_module(library(when)).

:- use_module(library(proof)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_proof)).
:- use_module(library(semweb/rdf_term)).

:- rdf_meta
   axiom(?, t),
   rdf_proof(t),
   rdf_proof(t, -),
   rdf_prove(t),
   recognized_datatype_iri(r),
   rule(?, t, t).

:- table
   rdf_proof(_,lattice(shortest_proof)).





%! rdf_prove(++Conclusion:compound) is semidet.
%! rdf_prove(+Conclusion:compound) is nondet.
%! rdf_proof(++Conclusion:compound) is semidet.
%! rdf_proof(+Conclusion:compound) is nondet.
%! rdf_proof(-Conclusion:compound) is multi.

rdf_proof(Concl) :-
  rdf_proof(Concl, Proof),
  rdf_view_proof(Proof).


%! rdf_proof(++Conclusion:compound, -Proof:compound) is semidet.
%! rdf_proof(+Conclusion:compound, -Proof:compound) is nondet.
%! rdf_proof(-Conclusion:compound, -Proof:compound) is multi.

rdf_proof(Concl, t(Rule,Concl,Proofs)) :-
  rule(Rule, Concl, Prems),
  maplist(rdf_proof, Prems, Proofs).



%! rdf_prove(-Conclusion:compound) is multi.

rdf_prove(Concl) :-
  distinct(Concl, rdf_proof(Concl, _)).



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
  when(ground(P), rdf_is_predicate(P)),
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
  when(ground(P), rdf_is_predicate(P)),
  rdf_container_membership_property(P).
axiom(rdfs, rdf(P,                  rdfs:domain,        rdfs:'Resource' )) :-
  when(ground(P), rdf_is_predicate(P)),
  rdf_container_membership_property(P).
axiom(rdfs, rdf(P,                  rdfs:range,         rdfs:'Resource' )) :-
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



%! rule(?Rule:compound, ?Conclusion:compound, -Premises:list(compound)) is nondet.

rule(B,            rdf(S,P,O),                             []) :-
  when(ground(S), rdf_is_subject(S)),
  when(ground(P), rdf_is_predicate(P)),
  tp(B, S, P, O).
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
