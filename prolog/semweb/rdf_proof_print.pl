:- module(
  rdf_proof_print,
  [
    rdf_proof_print/1 % +Proof
  ]
).

/** <module> A simple pretty-printer for RDF proof trees

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).



%! rdf_proof_print(+Proof:compound) is det.

rdf_proof_print(Tree) :-
  pp_tree(0, Tree).

pp_tree(N1, t(Concl,Rule,Trees)) :-
  tab(N1),
  pp_rule(Rule),
  format(" "),
  pp_tp(Concl), nl,
  N2 is N1 + 4,
  maplist(pp_tree(N2), Trees).

pp_rule(Rule) :-
  format("[~w]", [Rule]).

pp_bindings([]) :- !.
pp_bindings([H|T]) :-
  pp_binding(H),
  (T == [] -> true ; format(",")),
  pp_bindings(T).

pp_binding(X=Y) :-
  pp_term(X),
  format("="),
  pp_term(Y).

pp_tp(rdf(S,P,O)) :-
  pp_term(S),
  format(" "),
  pp_term(P),
  format(" "),
  pp_term(O).

pp_term(Var) :-
  var(Var), !,
  format("~w", [Var]).
pp_term(literal(lang(LTag,Lex))) :- !,
  format('"~a"@~a', [Lex,LTag]).
pp_term(literal(type(D,Lex))) :- !,
  format('"~a"^^', [Lex]),
  pp_iri(D).
pp_term(Iri) :-
  pp_iri(Iri).

pp_iri(Iri) :-
  rdf_current_prefix(Alias, Prefix),
  atom_concat(Prefix, Rest, Iri), !,
  format("~a:~a", [Alias,Rest]).
pp_iri(Iri) :-
  format("~a", [Iri]).
