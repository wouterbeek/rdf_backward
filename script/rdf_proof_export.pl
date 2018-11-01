:- module(
  rdf_proof_export,
  [
    rdf_proof_export/1 % +File
  ]
).

/** <module> Exports all entailments to file

@author Wouter Beek
@version 2018
*/

:- use_module(library(file_ext)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_tabling)).





%! rdf_proof_export(+File:atom) is det.

rdf_proof_export(File) :-
  write_to_file(
    File,
    [Out]>>forall(rdf_proof(Triple), rdf_write_triple(Out, Triple))
  ).
