# HQDM Relation Path Up command line tool

A command line tool to query the functional specification files of HQDM for the specification of a provided relation id, to output the relation set path to the Top relation set and the specification of the relation itself.

## Example

relationPathUp -a <PureHqdmRelations_v5.csv> <HqdmAllAsDataFormal2.csv> 6e23c714-9241-4132-aa7b-82391c6a60b7


## Command pattern

relationPathUp -a <Relation_file.csv> <EntityType_file.csv> <uuid_of_relation>

## Switches

`-a` ASCII output
`-m` Mermaid output
`-g` GraphViz output