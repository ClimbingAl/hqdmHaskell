# HQDM Relation Path Up command line tool

A command line tool to query the functional specification files of HQDM for the inheritance path of a provided relation id, to output the relation set path to the Top relation set and the specification of the relation itself. The relations can only be queried using ids and may not correspond directly to the relationships shown here [wiki](https://github.com/hqdmTop/hqdmFramework/wiki/).  In general, all the relationships originally specified in EXPRESS for HQDM and displayed in the wiki are included, but additional relation sets have been derived by re-computing the relation set inheritance.  This is more verbose than the original documentation for HQDM but addresses missing relationships in the original EXPRESS and handles them as Binary Relation Sets, almost as described here [Binary Relation](https://en.wikipedia.org/wiki/Binary_relation) but treating them as first class relation objects in data (this is markedly different from how other specifications like EXPRESS and RDFS/OWL treat relationships (or predicates). The Relation Specifications outputs all use entity and relation ids but the hierarchy diagrams displays the enity names and relation names with their ids in brackets.  The relation names are NOT unique and so the only way to query for relation information is by the id.  The entity tyoe heirarchy can be queried with the [`entityTypeSpec`](https://github.com/ClimbingAl/hqdmHaskell/tree/main/entityTypeSpec) tool.

## Example

`relationPathUp -a PureHqdmRelations_v5.csv HqdmAllAsDataFormal4Short.csv 6e23c714-9241-4132-aa7b-82391c6a60b7`

## General Command pattern

relationPathUp -a <Relation_file.csv> <EntityType_file.csv> <uuid_of_relation>

It is recommended to use the Relation File and Entity Type Files provided in this folder unless you are confident you have a reason for not using them.

## Running the command
This runs on any Linux command line as:

`./relationPathUp -a PureHqdmRelations_v5.csv HqdmAllAsDataFormal4Short.csv 6e23c714-9241-4132-aa7b-82391c6a60b7`

It may run on macOS (could be straightforward) and Windows (likely non-trivial), I haven't tried it.  The binary for this is included in the repo so that youy don't have to build it.  If you don't like trusting binaries then you can rebuild it by installing the Haskell GHC and Cabal build system. The command `stack build` will then build this command line tool and all of its dependencies.  The tool can then be used using `stack` itself like this:

`stack exec -- relationPathUp -a PureHqdmRelations_v8.csv HqdmAllAsDataFormal4Short.csv 6e23c714-9241-4132-aa7b-82391c6a60b7`

If you wish to use it without `stack` then you can build it with th `--copy-bins` switch. 

`stack build --copy-bins .`

This will copy the binary to the local bin path (you can then use it locally or copy it top where you need it) and as shown above. If you are using the Haskell dev container included in the vscode workspace [dev container file](https://github.com/ClimbingAl/hqdmHaskell/blob/main/.devcontainer/devcontainer.json) for this repo then the path to the built binary will be here: `/home/vscode/.local/bin/`.  You can move it to whereever you want it to be accessible.  If you do this, ensure that it has executable permissions (e.g. `-rwxr-xr-x`).

## Switches

`-a` ASCII output as a type hierarchy (supertype path followed by a subtype tree) and list of the relations for the specified HQDM Entity Type.

`-g` GraphViz output - Not yet implemented

`-m` Mermaid output - Initial implementation - waiting for Mermaid Euler chart feature ([#5932](https://github.com/mermaid-js/mermaid/pull/5932))

## Example output

```
ASCII Relation Inheritance Path To Universal Binary Relation Set (6e23c714-9241-4132-aa7b-82391c6a60b7):


                                                              [thing] universal_relation_set(85e78ac0-ec72-478f-9aac-cacb520290a0) [thing]
                                                                                                   ^
                                                                                                  /|\
                                                                                                   |
                                                            [thing] universal_set_relation_set(2db5490e-01d0-491e-bd64-67ac616f65a0) [thing]
                                                                                                   ^
                                                                                                  /|\
                                                                                                   |
                                               [spatio_temporal_extent] member_of(e052b90c-13c3-4fea-8289-0c995656ed8f) [class_of_spatio_temporal_extent]
                                                                                                   ^
                                                                                                  /|\
                                                                                                   |
                                                                [state] member_of(65553ab0-e43d-4f6c-8f1d-76dad4610a54) [class_of_state]
                                                                                                   ^
                                                                                                  /|\
                                                                                                   |
                                                         [individual] member_of_kind(6e23c714-9241-4132-aa7b-82391c6a60b7) [kind_of_individual]


RELATION SPECIFICATION:
        Domain: 58107227-267a-4f2e-a44a-25bb61c6a455
        Relation UUID: 6e23c714-9241-4132-aa7b-82391c6a60b7
        Original Relation Name: member_of_kind
        Range: 872000dc-f6b0-41db-a564-eb8f82f7a97c
        Min Cardinality: 0
        Max Cardinality: -1
```
