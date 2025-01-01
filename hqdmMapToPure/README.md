# hqdmMapToPure

A command line tool to convert Magma Core data exported as triples (in a CSV file format) from Fuseki to a pure form that can be handled by the hqdmHaskell applications.

## General mapping process

- Read csv file in
- Remove header if present
- Remove inverted commas, if present
- Remove IRI URL parts, leaving only basic uuids and strings
- Covert predicates to relation Ids

- Possibly list predicates that couldn't be converted?
- Save results in a specified file name, as a csv
