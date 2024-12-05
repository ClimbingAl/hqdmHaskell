# Functions to join MagmaCore-generated data with the purely functional representation of HQDM.

This is experimental but has worked on some test files (they are not provided).  Some basic transforms of the RDF triples are required before using with the functions employed in `Main.hs`.  This is a work in progress and will be updated soon to allow conversion of any MagmaCore generated RDF data to this purely functional form.

Note: an additional relation has been added called `element_of_type`.  Documentation of this will be provided soon along with the rationale for creating it (it essentially does away with the need for the formally ambiguous `rdf:type` predicate (not all `rdf:type` statements get converted to `element_of_type` for this reason).
