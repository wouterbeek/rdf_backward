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

:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).

:- rdf_meta
   rdf_proof_print(t).





%! rdf_proof_print(+Proof:compound) is det.

rdf_proof_print(Tree) :-
  pp_tree(0, Tree).

pp_tree(N1, t(Rule,Concl,Trees)) :-
  tab(N1),
  pp_rule(Rule),
  format(" "),
  pp_tp(Concl), nl,
  N2 is N1 + 4,
  maplist(pp_tree(N2), Trees).

pp_rule(Rule) :-
  format("[~w]", [Rule]).

pp_tp(rdf(S,P,O)) :-
  pp_term(S),
  format(" "),
  pp_term(P),
  format(" "),
  pp_term(O).

pp_term(Var) :-
  var(Var), !,
  format("~w", [Var]).
pp_term(Term) :-
  rdf_atom_term(Atom, Term),
  format("~a", [Atom]).
