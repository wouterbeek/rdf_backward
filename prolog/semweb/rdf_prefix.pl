:- module(rdf_prefix, [init_rdf_prefixes/0]).

:- use_module(library(semweb/rdf_db)).

init_rdf_prefixes :-
  forall(
    prefix_(Alias, Prefix),
    rdf_register_prefix(Alias, Prefix)
  ).

prefix_(Alias, Prefix) :-
  rdf_db:ns(Alias, Prefix).
prefix_(bag, 'http://bag.basisregistraties.overheid.nl/def/bag#').
prefix_(bibframe, 'http://id.loc.gov/ontologies/bibframe/').
prefix_(bibo, 'http://purl.org/ontology/bibo/').
prefix_(cms, 'http://SemanticCMS.cc/vocab/').
prefix_(crs, 'http://www.opengis.net/def/crs/OGC/1.3/').
prefix_(csvw, 'http://www.w3.org/ns/csvw#').
prefix_(cyc, 'http://sw.opencyc.org/concept/').
prefix_(dawgt, 'http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#').
prefix_(dbc, 'http://dbpedia.org/resource/Category:').
prefix_(dbd, 'http://dbpedia.org/datatype/').
prefix_(dbo, 'http://dbpedia.org/ontology/').
prefix_(dbp, 'http://dbpedia.org/property/').
prefix_(dbr, 'http://dbpedia.org/resource/').
prefix_(dby, 'http://dbpedia.org/class/yago/').
prefix_(dcat, 'http://www.w3.org/ns/dcat#').
prefix_(dce, 'http://purl.org/dc/elements/1.1/').
prefix_(dcterm, 'http://purl.org/dc/terms/').
prefix_(dctype, 'http://purl.org/dc/dcmitype/').
prefix_(dqv, 'http://www.w3.org/ns/dqv#').
prefix_(earl, 'http://www.w3.org/ns/earl#').
prefix_(ex, 'https://example.org/').
prefix_(fabio, 'http://purl.org/spar/fabio/').
prefix_(fb, 'http://ogp.me/ns/fb#').
prefix_(freebase, 'http://rdf.freebase.com/ns/').
prefix_(fn, 'http://www.w3.org/2005/xpath-functions#').
prefix_(formats, 'http://www.w3.org/ns/formats/').
prefix_(geo, 'http://www.opengis.net/ont/geosparql#').
prefix_(geof, 'http://www.opengis.net/def/function/geosparql/').
prefix_(geonames, 'http://sws.geonames.org/').
prefix_(geor, 'http://www.opengis.net/def/rule/geosparql/').
prefix_(gg, 'http://www.gemeentegeschiedenis.nl/gg-schema#').
prefix_(gml, 'http://www.opengis.net/ont/gml#').
prefix_(gr, 'http://purl.org/goodrelations/v1#').
prefix_(grddl, 'http://www.w3.org/2003/g/data-view#').
prefix_(http, 'http://www.w3.org/2011/http#').
prefix_(hydra, 'http://www.w3.org/ns/hydra/core#').
prefix_(ical, 'http://www.w3.org/2002/12/cal/icaltzd#').
prefix_(lexvo, 'http://lexvo.org/ontology#').
prefix_(ma, 'http://www.w3.org/ns/ma-ont#').
prefix_(mf, 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#').
prefix_(nyt, 'http://data.nytimes.com/').
prefix_(odp, 'http://www.ontologydesignpatterns.org/').
prefix_(openlinks, 'http://www.openlinksw.com/schemas/virtrdf#').
prefix_(orcid, 'http://orcid.org/').
prefix_(org, 'http://www.w3.org/ns/org#').
prefix_(prov, 'http://www.w3.org/ns/prov#').
prefix_(qb, 'http://purl.org/linked-data/cube#').
prefix_(qt, 'http://www.w3.org/2001/sw/DataAccess/tests/test-query#').
prefix_(rdfa, 'http://www.w3.org/ns/rdfa#').
prefix_(rdft, 'http://www.w3.org/ns/rdftest#').
prefix_(rel, 'http://purl.org/vocab/relationship/').
prefix_(rif, 'http://www.w3.org/2007/rif#').
prefix_(role, 'http://www.w3.org/1999/xhtml/vocab#role').
prefix_(rr, 'http://www.w3.org/ns/r2rml#').
prefix_(schema, 'http://schema.org/').
prefix_(sd, 'http://www.w3.org/ns/sparql-service-description#').
prefix_(sf, 'http://www.opengis.net/ont/sf#').
prefix_(sfn, ' http://www.w3.org/ns/sparql#').
prefix_(sh, 'http://www.w3.org/ns/shacl#').
prefix_(sioc, 'http://rdfs.org/sioc/ns#').
prefix_(skosxl, 'http://www.w3.org/2008/05/skos-xl#').
prefix_('sparql-results', 'http://www.w3.org/2005/sparql-results#').
prefix_(umbel, 'http://umbel.org/umbel#').
prefix_(uom, 'http://www.opengis.net/def/uom/OGC/1.0/').
prefix_(vcard, 'http://www.w3.org/2006/vcard/ns#').
prefix_(wdr, 'http://www.w3.org/2007/05/powder#').
prefix_(wdrs, 'http://www.w3.org/2007/05/powder-s#').
prefix_(wdt, 'http://www.wikidata.org/prop/direct/').
prefix_(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').
prefix_(wv, 'http://vocab.org/waiver/terms/norms').
prefix_(xhv, 'http://www.w3.org/1999/xhtml/vocab#').
prefix_(xml, 'http://www.w3.org/XML/1998/namespace').
prefix_(yago, 'http://yago-knowledge.org/resource/').
