:- use_module(library(sw/hdt_db)).
:- use_module(library(sw/hdt_graph)).
:- use_module(library(sw/rdf_reasoner_cli)).
:- use_module(library(sw/rdf_term)).

:- initialization
   hdt_init('/scratch/wbeek/data/LOD-a-lot/data.hdt').

setup :-
  rdf_equal(Prefix, rdf:'_'),
  hdt(Hdt),
  forall(
    hdt_term_prefix(Hdt, predicate, Prefix, P),
    (
      add_axiom(rdf,  rdf(P, rdf:type, rdf:'Property')),
      add_axiom(rdfs, rdf(P, rdf:type, rdfs:'ContainerMembershipProperty')),
      add_axiom(rdfs, rdf(P, rdfs:domain, rdfs:'Resource')),
      add_axiom(rdfs, rdf(P, rdfs:range, rdfs:'Resource'))
    )
  ).
