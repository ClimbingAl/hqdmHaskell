# HQDM Mapped Dataset Cardinality Checker

A command line tool that takes in a converted (mapped to pure) hqdm dataset and checks for cardinality and range violations.  The output is in a text-based output on the standard output stream (and can therfore be directed to a file too).  Takes flags to allow cardinality violations to be fitered out (i.e. ignored).  Listed errors have the type or error and the associated Binary Relation Set properties listed.  Error counts are also given for cardinality and range type violations.

## Example

`hqdmCardinalityChecker -sw ../PureHqdmRelations_v9.csv ../HqdmTypes_v4.csv mappedHqdmDataset.csv`

## General Command pattern

Running the following command, `./hqdmCardinalityChecker --help`, will display the switches available and the command arguments:

```
Usage: hqdmCardinalityChecker -psoerw <hqdmRelations.csv> <hqdmEntityTypes.csv> <inputProcessedTriples.csv> <outputFilename.csv>
  -p          Specifies that the output includes relationships of the Parthood Binary Relation Set.
  -s          Specifies that the output includes relationships of the Set (HQDM Class) Binary Relation Set.
  -o          Specifies that the output includes relationships of the Order Binary Relation Set.
  -e          Specifies that the output includes relationships of the Emergent Binary Relation Set.
  -r          Specifies that the output includes relationships of the Reified Binary Relation Set.
  -w          Specifies that the output includes relationships of the part_of_possible_world Relation Set.
      --help  The command should have the general form: hqdmCardinalityChecker -psoerw hqdmRelations.csv hqdmEntityTypes.csv inputTriples.csv outputTriplesFilename.csv
```

## Running the command
This runs on any Linux command line as:

`./hqdmCardinalityChecker -sw ../PureHqdmRelations_v9.csv ../HqdmTypes_v4.csv mappedHqdmDataset.csv`

It may run on macOS (could be straightforward) and Windows.  The binary for this is included in the repo so that youy don't have to build it.  If you don't like trusting binaries then you can rebuild it by installing the Haskell GHC and Cabal build system. The command `stack build` will then build this command line tool and all of its dependencies.  The tool can then be used using `stack` itself like this:

`stack exec -- hqdmCardinalityChecker -sw ../PureHqdmRelations_v9.csv ../HqdmTypes_v4.csv mappedHqdmDataset.csv`

If you wish to use it without `stack` then you can build it with th `--copy-bins` switch. 

`stack build --copy-bins .`

This will copy the binary to the local bin path (you can then use it locally or copy it top where you need it) and as shown above. If you are using the Haskell dev container included in the vscode workspace [dev container file](https://github.com/ClimbingAl/hqdmHaskell/blob/main/.devcontainer/devcontainer.json) for this repo then the path to the built binary will be here: `/home/vscode/.local/bin/`.  You can move it to whereever you want it to be accessible.  If you do this, ensure that it has executable permissions (e.g. `-rwxr-xr-x`).

The format of the output is given with cardinality violations (by the sort of violation selected by the switches) followed by the range type violations, with each error haing the following form:

```
ObjectId: <the uuid of the object that has the relationship error>
Relation check result: <one of MinCardinalityViolation, MaxCardinalityViolation or RangeTypeViolation>
<Hint>
RELATION SPECIFICATION:
<list of the Binary Relation properties in human-readable form (UUID and associated names)>
```

## Example output

The following command using a test file with cardinality and range type errors (some filtered using the selected switches) gives the text output beneath it:

`./hqdmCardinalityChecker -w ../PureHqdmRelations_v9.csv ../HqdmTypes_v4.csv ../../datasets/joinedAllRelsTestWith1Err.csv`

```
**hqdmCardinalityChecker**

Loading model files and the supplied input file of processed mapped User Data, from file '../../datasets/joinedAllRelsTestWith1Err.csv'.

PART1: CARDINALITY VIOLATION CHECKS


Part_of_possible_world Relation Cardinality Exceptions:



Object Id:"21b2b267-3956-491f-8276-a705b80b3831" of type 'possible_world'
Relation check result: MinCardinalityViolation
Likely error, investigate to confirm it doesn't impact your use of the model and data.
RELATION SPECIFICATION:
        Domain: a5dc8c36-56fc-44c2-909b-7592b1f44fd7 type `possible_world'
        Relation UUID: 989c624a-2b34-49b1-8adb-584c6c2b082d
        Original Relation Name: part_of_possible_world
        Range: a5dc8c36-56fc-44c2-909b-7592b1f44fd7 type `possible_world'
        Min Cardinality: 1
        Max Cardinality: -1


        Number of part_of_possible_world Relation Cardinality Exceptions:1
        Note: Strictly, there will always be at least one of these exceptions (see HQDM entity type definition https://github.com/hqdmTop/hqdmFramework/wiki/spatio_temporal_extent)




Total number of Relation Cardinality Exceptions:197



PART2: BINARY RELATION RANGE TYPE VIOLATION CHECKS



Object Id:"00dac31b-acad-40cf-a44c-72ed092afa08" of type 'functional_system_component'
Relation check result: RangeTypeViolation
Likely error, investigate to confirm it doesn't impact your use of the model and data.
RELATION SPECIFICATION:
        Domain: a980b931-9769-4975-847a-5d81405b213b type `functional_system_component'
        Relation UUID: 8ea62706-fa07-40d7-8586-a8768403c01e
        Original Relation Name: member_of_kind
        Range: 5ccc13dd-c63c-4502-aa0d-786410b138cd type `kind_of_functional_system_component'
        Min Cardinality: 1
        Max Cardinality: -1


        Number of Binary Relation Range Exceptions:1
        Note: Strictly, this only tests for instances of Binary Relations that are present in the dataset.  If an instance of a necessary Binary Relation is missing this will be caught by the Cardinality Checking above.




**DONE**
```
