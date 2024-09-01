:- begin_tests(rdf_backward).

:- use_module(library(semweb/rdf11)).

:- use_module(library(semweb/rdf_backward)).

term_expansion(
  (test(Name, Options1) :- Body),
  (test(Name, Options2) :- Body)
) :-
  rdf_global_term(Options1, Options2).

test(recognized_datatype_iris, set(Datatype == [rdf:langString,xsd:string])) :-
  rdf_prove(tp(Datatype,rdf:type,rdfs:'Datatype')).
test(recognized_datatype_iris, [setup(assert(recognized_datatype_iri(xsd:date))),
                                set(Datatype == [rdf:langString,xsd:date,xsd:string]),
                                cleanup(retract(recognized_datatype_iri(xsd:date)))]) :-
  rdf_prove(tp(Datatype,rdf:type,rdfs:'Datatype')).

:- end_tests(rdf_backward).
