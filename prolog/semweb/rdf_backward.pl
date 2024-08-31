:- encoding(utf8).
:- module(
  rdf_backward,
  [
    rdf_assert_recognized_datatype_iri/1, % +Datatype
    rdf_proof/1,                          % ?Conclusion
    rdf_proof/2,                          % +Dataset, ?Conclusion
    rdf_proof/3,                          % +Dataset, ?Conclusion, -Proof
    rdf_prove/1,                          % ?Conclusion
    rdf_prove/2                           % +Dataset, ?Conclusion
  ]
).

/** <module> RDF(S) entailment through mode-directed tabling

*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(when)).
:- use_module(library(yall)).

:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_proof)).
:- use_module(library(tree)). % Used in lattice/1.

:- dynamic
   rdf_recognized_datatype_iri_/1.

:- initialization
   init_.

:- rdf_meta
   axiom_(?, t),
   rdf_assert_recognized_datatype_iri(r),
   rdf_proof(t),
   rdf_proof(+, t),
   rdf_proof(+, t, -),
   rdf_prove(t),
   rdf_prove(+, t),
   rule_(+, ?, t, t).

shortest_proof(Proof1, Proof2, Shortest) :-
  maplist(must_be(tree), [Proof1,Proof2]),
  shortest([Proof1,Proof2], Shortest).

:- table
   rdf_proof(_,_,lattice(shortest_proof)).



%! rdf_assert_recognized_datatype_iri(+Datatype:iri) is semidet.
%
% Extend the datatype IRIs that are recognized by this reasoner.
%
% Datatype IRI `rdf:langString' is recognized, but is not asserted,
% since language-tagged strings are implemented by a different Prolog
% syntax.
%
% By default, the set of recognized datatype IRIs includes only
% `rdf:langString' and `xsd:string'.  This set can be extended by
% asserting Prolog terms of the form `recognized_datatype_iri(+atom)'.

:- det(rdf_assert_recognized_datatype_iri/1).
rdf_assert_recognized_datatype_iri(Datatype) :-
  assert(rdf_recognized_datatype_iri_(Datatype)).



%! rdf_proof(++Conclusion:rdf_triple_pattern) is semidet.
%! rdf_proof(+Conclusion:rdf_triple_pattern) is nondet.
%! rdf_proof(-Conclusion:rdf_triple_pattern) is multi.

rdf_proof(Conclusion) :-
  rdf_dataset_create(default, [], Dataset),
  rdf_proof(Dataset, Conclusion).


%! rdf_proof(+Dataset:rdf_dataset, ++Conclusion:rdf_triple_pattern) is semidet.
%! rdf_proof(+Dataset:rdf_dataset, +Conclusion:rdf_triple_pattern) is nondet.
%! rdf_proof(+Dataset:rdf_dataset, -Conclusion:rdf_triple_pattern) is multi.

rdf_proof(Dataset, Conclusion) :-
  rdf_proof(Dataset, Conclusion, Proof),
  rdf_view_proof(Proof).


%! rdf_proof(+Dataset:rdf_dataset, ++Conclusion:rdf_triple_pattern, -Proof:tree) is semidet.
%! rdf_proof(+Dataset:rdf_dataset, +Conclusion:rdf_triple_pattern, -Proof:tree) is nondet.
%! rdf_proof(+Dataset:rdf_dataset, -Conclusion:rdf_triple_pattern, -Proof:tree) is multi.

rdf_proof(Dataset, Conclusion, tree(Rule,Conclusion,Proofs)) :-
  rule_(Dataset, Rule, Conclusion, Prems),
  maplist(rdf_proof(Dataset), Prems, Proofs).



%! rdf_prove(++Conclusion:rdf_triple_pattern) is semidet.
%! rdf_prove(+Conclusion:rdf_triple_pattern) is nondet.

rdf_prove(Conclusion) :-
  rdf_dataset_create(default, [], Dataset),
  rdf_prove(Dataset, Conclusion).


%! rdf_prove(+Dataset:rdf_dataset, ++Conclusion:rdf_triple_pattern) is semidet.
%! rdf_prove(+Dataset:rdf_dataset, +Conclusion:rdf_triple_pattern) is nondet.

rdf_prove(Dataset, Conclusion) :-
  distinct(Conclusion, rdf_proof(Dataset, Conclusion, _)).



% AXIOMS %

%! axiom_(?Vocab:any, ?Triple:rdf_triple_pattern) is nondet.

axiom_(rdf,  tp(rdf:type,           rdf:type,           rdf:'Property'  )).
axiom_(rdf,  tp(rdf:subject,        rdf:type,           rdf:'Property'  )).
axiom_(rdf,  tp(rdf:predicate,      rdf:type,           rdf:'Property'  )).
axiom_(rdf,  tp(rdf:object,         rdf:type,           rdf:'Property'  )).
axiom_(rdf,  tp(rdf:first,          rdf:type,           rdf:'Property'  )).
axiom_(rdf,  tp(rdf:rest,           rdf:type,           rdf:'Property'  )).
axiom_(rdf,  tp(rdf:value,          rdf:type,           rdf:'Property'  )).
axiom_(rdf,  tp(rdf:nil,            rdf:type,           rdf:'List'      )).
axiom_(rdf,  tp(P,                  rdf:type,           rdf:'Property'  )) :-
  when(ground(P), rdf_is_predicate(P)),
  rdf_container_membership_property(P).
axiom_(rdfs, tp(rdf:type,           rdfs:domain,        rdfs:'Resource' )).
axiom_(rdfs, tp(rdfs:domain,        rdfs:domain,        rdf:'Property'  )).
axiom_(rdfs, tp(rdfs:range,         rdfs:domain,        rdf:'Property'  )).
axiom_(rdfs, tp(rdfs:subPropertyOf, rdfs:domain,        rdf:'Property'  )).
axiom_(rdfs, tp(rdfs:subClassOf,    rdfs:domain,        rdfs:'Class'    )).
axiom_(rdfs, tp(rdf:subject,        rdfs:domain,        rdf:'Statement' )).
axiom_(rdfs, tp(rdf:predicate,      rdfs:domain,        rdf:'Statement' )).
axiom_(rdfs, tp(rdf:object,         rdfs:domain,        rdf:'Statement' )).
axiom_(rdfs, tp(rdfs:member,        rdfs:domain,        rdfs:'Resource' )).
axiom_(rdfs, tp(rdf:first,          rdfs:domain,        rdf:'List'      )).
axiom_(rdfs, tp(rdf:rest,           rdfs:domain,        rdf:'List'      )).
axiom_(rdfs, tp(rdfs:seeAlso,       rdfs:domain,        rdfs:'Resource' )).
axiom_(rdfs, tp(rdfs:isDefinedBy,   rdfs:domain,        rdfs:'Resource' )).
axiom_(rdfs, tp(rdfs:comment,       rdfs:domain,        rdfs:'Resource' )).
axiom_(rdfs, tp(rdfs:label,         rdfs:domain,        rdfs:'Resource' )).
axiom_(rdfs, tp(rdf:value,          rdfs:domain,        rdfs:'Resource' )).
axiom_(rdfs, tp(rdf:type,           rdfs:range,         rdfs:'Class'    )).
axiom_(rdfs, tp(rdfs:domain,        rdfs:range,         rdfs:'Class'    )).
axiom_(rdfs, tp(rdfs:range,         rdfs:range,         rdfs:'Class'    )).
axiom_(rdfs, tp(rdfs:subPropertyOf, rdfs:range,         rdf:'Property'  )).
axiom_(rdfs, tp(rdfs:subClassOf,    rdfs:range,         rdfs:'Class'    )).
axiom_(rdfs, tp(rdf:subject,        rdfs:range,         rdfs:'Resource' )).
axiom_(rdfs, tp(rdf:predicate,      rdfs:range,         rdfs:'Resource' )).
axiom_(rdfs, tp(rdf:object,         rdfs:range,         rdfs:'Resource' )).
axiom_(rdfs, tp(rdfs:member,        rdfs:range,         rdfs:'Resource' )).
axiom_(rdfs, tp(rdf:first,          rdfs:range,         rdfs:'Resource' )).
axiom_(rdfs, tp(rdf:rest,           rdfs:range,         rdf:'List'      )).
axiom_(rdfs, tp(rdfs:seeAlso,       rdfs:range,         rdfs:'Resource' )).
axiom_(rdfs, tp(rdfs:isDefinedBy,   rdfs:range,         rdfs:'Resource' )).
axiom_(rdfs, tp(rdfs:comment,       rdfs:range,         rdfs:'Literal'  )).
axiom_(rdfs, tp(rdfs:label,         rdfs:range,         rdfs:'Literal'  )).
axiom_(rdfs, tp(rdf:value,          rdfs:range,         rdfs:'Resource' )).
axiom_(rdfs, tp(rdf:'Alt',          rdfs:subClassOf,    rdfs:'Container')).
axiom_(rdfs, tp(rdf:'Bag',          rdfs:subClassOf,    rdfs:'Container')).
axiom_(rdfs, tp(rdf:'Seq',          rdfs:subClassOf,    rdfs:'Container')).
axiom_(rdfs, tp(rdfs:'ContainerMembershipProperty', rdfs:subClassOf, rdf:'Property')).
axiom_(rdfs, tp(rdfs:isDefinedBy,   rdfs:subPropertyOf, rdfs:seeAlso    )).
axiom_(rdfs, tp(rdfs:'Datatype',    rdfs:subClassOf,    rdfs:'Class'    )).
axiom_(rdfs, tp(P,                  rdf:type,           rdfs:'ContainerMembershipProperty')) :-
  when(ground(P), rdf_is_predicate(P)),
  rdf_container_membership_property(P).
axiom_(rdfs, tp(P,                  rdfs:domain,        rdfs:'Resource' )) :-
  when(ground(P), rdf_is_predicate(P)),
  rdf_container_membership_property(P).
axiom_(rdfs, tp(P,                  rdfs:range,         rdfs:'Resource' )) :-
  when(ground(P), rdf_is_predicate(P)),
  rdf_container_membership_property(P).



% RULES %

%! rule_(+Dataset:rdf_dataset,
%!       ?Rule:any,
%!       ?Conclusion:rdf_triple_pattern,
%!       -Premises:list(rdf_triple_pattern)) is nondet.

rule_(Dataset, db,   tp(S,P,O), []) :-
  when(ground(S), rdf_is_subject(S)),
  when(ground(P), rdf_is_predicate(P)),
  rdf_triple_pattern(Dataset, tp(S,P,O)).
rule_(_, axiom(Vocab), Conclusion, []) :-
  axiom_(Vocab, Conclusion).
rule_(_, tp(1),      tp(literal(lang(LTag,Lex)),rdf:type,rdf:langString), [tp(_,_,literal(lang(LTag,Lex)))]) :-
  ground(LTag-Lex).
rule_(_, tp(1),      tp(literal(type(D,Val)),rdf:type,D),   [tp(_,_,literal(type(D,Val)))]) :-
  ground(D-Val),
  rdf_recognized_datatype_iri_(D).
rule_(_, tp(2),      tp(P,rdf:type,rdf:'Property'),         [tp(_,P,_)]).
rule_(_, rdfs(1),    tp(D,rdf:type,rdfs:'Datatype'),        []) :-
  (rdf_equal(D, rdf:langString) ; rdf_recognized_datatype_iri_(D)).
rule_(_, rdfs(2),    tp(I,rdf:type,C),                      [tp(P,rdfs:domain,C),tp(I,P,_)]).
rule_(_, rdfs(3),    tp(I,rdf:type,C),                      [tp(P,rdfs:range,C),tp(_,P,I)]).
rule_(_, rdfs('4a'), tp(I,rdf:type,rdfs:'Resource'),        [tp(I,_,_)]).
rule_(_, rdfs('4b'), tp(I,rdf:type,rdfs:'Resource'),        [tp(_,_,I)]).
rule_(_, rdfs(5),    tp(P, rdfs:subPropertyOf, R),          [tp(P,rdfs:subPropertyOf,Q),tp(Q,rdfs:subPropertyOf,R)]).
rule_(_, rdfs(6),    tp(P, rdfs:subPropertyOf, P),          [tp(P,rdf:type,rdf:'Property')]).
rule_(_, rdfs(7),    tp(X,Q,Y),                             [tp(P,rdfs:subPropertyOf,Q),tp(X,P,Y)]).
rule_(_, rdfs(8),    tp(C,rdfs:subClassOf,rdfs:'Resource'), [tp(C,rdf:type,rdfs:'Class')]).
rule_(_, rdfs(9),    tp(I,rdf:type,D),                      [tp(C,rdfs:subClassOf,D),tp(I,rdf:type,C)]).
rule_(_, rdfs(10),   tp(C,rdfs:subClassOf,C),               [tp(C,rdf:type,rdfs:'Class')]).
rule_(_, rdfs(11),   tp(C,rdfs:subClassOf,E),               [tp(C,rdfs:subClassOf,D),tp(D,rdfs:subClassOf,E)]).
rule_(_, rdfs(12),   tp(P,rdfs:subPropertyOf,rdfs:member),  [tp(P,rdf:type,rdfs:'ContainerMembershipProperty')]).
rule_(_, rdfs(13),   tp(C,rdfs:subClassOf,rdfs:'Literal'),  [tp(C,rdf:type,rdfs:'Datatype')]).



% INITIALIZATION %

:- det(init_/0).
init_ :-
  rdf_assert_recognized_datatype_iri(rdf:langString),
  rdf_assert_recognized_datatype_iri(xsd:string).
