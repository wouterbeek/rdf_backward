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

:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_tabling)).
:- use_module(library(stream_ext)).





%! rdf_proof_export(+File:atom) is det.

rdf_proof_export(File) :-
  call_stream_file(
    File,
    [Out]>>forall(rdf_proof(Triple), rdf_write_triple(Out, Triple))
  ).
