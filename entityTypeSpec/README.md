# HQDM Entity Type Spec command line tool

A command line tool to query the functional specification files of HQDM for the specification of a provided entity type id (or name), to output the entity type path to Thing and the specification of the entity type itself.

## Example

entityTypeSpec -a <PureHqdmRelations_v5.csv> <HqdmAllAsDataFormal2.csv> spatio_temporal_extent


## Command pattern

entityTypeSpec -a <Relation_file.csv> <EntityType_file.csv> <uuid_or_name_of_entity_type>

## Switches

`-a` ASCII output
`-m` Mermaid output
`-g` GraphViz output