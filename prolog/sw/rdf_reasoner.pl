:- module(
  rdf_reasoner,
  [
    rdf_prove/1,     % ?Conclusion
    rdf_prove_tree/2 % ?Conclusion, -Proof
  ]
).
/** <module> A simple backward-chaining reasoner for RDF(S)

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(lists)).

:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_term)).

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
   axiom(?, t),
   rdf_prove(t),
   rdf_prove_tree(t, -),
   recognized_datatype_iri(r),
   rule(?, t, t).

:- table
   rdf_prove/1,
   rdf_prove_tree(_,lattice(shortest_proof)).





%! rdf_prove(++Conclusion:compound) is semidet.
%! rdf_prove(+Conclusion:compound) is nondet.
%! rdf_prove(-Conclusion:compound) is multi.

rdf_prove(Concl) :-
  rule(_, Concl, Prems),
  maplist(rdf_prove, Prems).



%! rdf_prove_tree(++Conclusion:compound, -Proof:compound) is semidet.
%! rdf_prove_tree(+Conclusion:compound, -Proof:compound) is nondet.
%! rdf_prove_tree(-Conclusion:compound, -Proof:compound) is multi.

rdf_prove_tree(Concl, t(Rule,Concl,Trees)) :-
  rule(Rule, Concl, Prems),
  maplist(rdf_prove_tree, Prems, Trees).

shortest_proof(Tree1, Tree2, Tree) :-
  maplist(tree_depth, [Tree1,Tree2], [Depth1,Depth2]),
  (Depth2 < Depth1 -> Tree = Tree2 ; Tree = Tree1).





% AXIOMS, RECOGNIZED DATATYPE IRIS, RULES %

%! axiom(?Vocab:term, ?Triple:compound) is nondet.

axiom(rdf,  rdf(rdf:type,           rdf:type,           rdf:'Property'  )).
axiom(rdf,  rdf(rdf:subject,        rdf:type,           rdf:'Property'  )).
axiom(rdf,  rdf(rdf:predicate,      rdf:type,           rdf:'Property'  )).
axiom(rdf,  rdf(rdf:object,         rdf:type,           rdf:'Property'  )).
axiom(rdf,  rdf(rdf:first,          rdf:type,           rdf:'Property'  )).
axiom(rdf,  rdf(rdf:rest,           rdf:type,           rdf:'Property'  )).
axiom(rdf,  rdf(rdf:value,          rdf:type,           rdf:'Property'  )).
axiom(rdf,  rdf(rdf:nil,            rdf:type,           rdf:'List'      )).
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
rule(rdf(1), rdf(literal(lang(LTag,Lex)),rdf:type,rdf:langString), [rdf(_,_,literal(lang(LTag,Lex)))]) :-
  ground(LTag-Lex).
rule(rdf(1), rdf(literal(type(D,Lex)),rdf:type,D), [rdf(_,_,literal(type(D,Lex)))]) :-
  ground(D-Lex),
  recognized_datatype_iri(D).
rule(rdf(2),       rdf(P,rdf:type,rdf:'Property'),         [rdf(_S,P,_O)]).
rule(rdfs(1),      rdf(D,rdf:type,rdfs:'Datatype'),        []) :-
  recognized_datatype_iri(D).  
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





% HELPERS %

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



%! tree_depth(+Tree:compound, -Depth:nonneg) is det.

tree_depth(t(_,_,[]), 0) :- !.
tree_depth(t(_,_,Trees), Depth) :-
  maplist(tree_depth, Trees, Depths),
  min_list(Depths, Depth0),
  Depth is Depth0 + 1.
