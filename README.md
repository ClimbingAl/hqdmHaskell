# Haskell parser for HQDM AllAsData

This is an experiment in the functional parsing (and ultimately querying) of the HQDM data model when represented as triples - _subject_ <- _predicate_ -> _object_ statements.  The source data for the model itself is the master HQDM AllAsData used in some of my other projects, such as [HQDM Patterns](https://climbingal.github.io/HqdmPatterns/).  Any project that uses Magma Core is likely compatible with it although my fork of it is the best place to look for some tweaks that help [Magma Core](https://github.com/ClimbingAl/MagmaCore/).  All that Magma Core-based datasets need is a binding to the [HqdmAllAsData](https://github.com/ClimbingAl/code-for-hqdm-patterns/blob/main/source-files/hqdmAllAsData.ttl) dataset that represents the model used for this functional parser.  The actual file used to apply the [HqdmLib](https://github.com/ClimbingAl/hqdmHaskell/blob/main/hqdm/src/HqdmLib.hs) functions to is a filtered version of the hqdmAllAsData.ttl file (linked in the last sentence), listed as triple statements in a CSV file and with `rdf:type` replaced by `hqdm:type` (to remove any suggestion that an external spevcification may be needed to interpret the model.  This source data is stored in [hqdmAllAsDataFormal1.csv](https://github.com/ClimbingAl/hqdmHaskell/blob/main/hqdm/hqdmAllAsDataFormal1.csv).

The first 12 lines of the .csv file are:

| subject | predicate | object|
| --- | --- | --- |
|hqdm:f5ac9254-2b93-4ed7-b89e-70994842b438|hqdm:temporal__part_of|hqdm:dd8359c4-0a2b-416a-af27-3bcb0551b41a|
|hqdm:f5ac9254-2b93-4ed7-b89e-70994842b438|hqdm:part_of_possible_world|hqdm:a5dc8c36-56fc-44c2-909b-7592b1f44fd7|
|hqdm:f5ac9254-2b93-4ed7-b89e-70994842b438|hqdm:consists__of|hqdm:dd8359c4-0a2b-416a-af27-3bcb0551b41a|
|hqdm:f5ac9254-2b93-4ed7-b89e-70994842b438|hqdm:type|hqdm:state_of_biological_object|
|hqdm:f5ac9254-2b93-4ed7-b89e-70994842b438|hqdm:member_of|hqdm:6beb4515-89e8-4bce-9318-77935200f9bb|
|hqdm:f5ac9254-2b93-4ed7-b89e-70994842b438|hqdm:ending|hqdm:545b4541-8a34-46b8-8704-2265be0244c3|
|hqdm:f5ac9254-2b93-4ed7-b89e-70994842b438|hqdm:beginning|hqdm:545b4541-8a34-46b8-8704-2265be0244c3|
|hqdm:f5ac9254-2b93-4ed7-b89e-70994842b438|hqdm:part__of|hqdm:dd8359c4-0a2b-416a-af27-3bcb0551b41a|
|hqdm:f5ac9254-2b93-4ed7-b89e-70994842b438|hqdm:member__of|hqdm:4a8cba08-035c-4902-935b-26da61ed282c|
|hqdm:f5ac9254-2b93-4ed7-b89e-70994842b438|hqdm:temporal_part_of|hqdm:21c1fe82-1b48-46b3-8df8-6bddcbb2d92e|
|hqdm:f5ac9254-2b93-4ed7-b89e-70994842b438|hqdm:aggregated_into|hqdm:dd8359c4-0a2b-416a-af27-3bcb0551b41a|
|hqdm:f5ac9254-2b93-4ed7-b89e-70994842b438|hqdm:has_supertype|hqdm:f9cb048d-a2f7-4ff6-b824-c59b44e2aabe|


All the NodeIds are preserved, with prefixes applied, and with all the predicate names and HQDM Entity Types as-per HQDM as documented (and as in Magma Core).

### Functions in HqdmLib

All the functions in HQDMLib are pure Haskell with minimised guards; that is, guards tend to be employed solely to terminate a recursive function (or list comprehension) on an Empty List (e.g. [] or [[]]) or when the root Entity Type Thing is found.  The functions each have a short description in the comments above it.  The functions have not been written for efficiency, with the focus being on purity.  Variable names and function names have been chosen to indicate the purpose or role.  This should make them fairly readable as long as you are familiar with (or tolerant to) the Haskell function notations (e.g. `.`, `$`, `<-`, `\` lambda expressions, and built-in functions).

## Install and Execute

Install Haskell Glasgow Haskell Compiler with Cabal & Stack: https://www.haskell.org/ghcup/

After cloning the repo first go to the `hqdm` folder and run the following 2 commands:

    `stack build`
    `stack exec hqdm-exe` (this runs the Haskell executable resulting from building the Main.hs file)

This should result in a number of illustrative results for most of the functions in HqdmLib being written to the console.

The `inheritance` folder contains a Haskell Main.hs file to calculate the inherited relations for each of the HQDM Entity Types.  It is best to pipe the output to a file as the console may not cope with the size of the output.  Run the following 2 commands:

  `stack build`
  `stack exec inhertiance > HqdmTypesAndInheritedRels.txt`

This text file should match the text file of that name in this repo.  
Note: The `hqdm` and `inheritance` folders use different versions of Cabal, which explains why the `inheritance` one doesn't need `-exe` to post-fix the folder name when executing it.  I'll clean this up sometime.

## ToDo


## Caveats
This implementation uses pure Haskell with no Monadic functions (once the data has loaded).  This means that if any errors are introduced in the source data the GHC Executable will fail if an exception is hit.  I am happy with this, as it is intended to be a formal (pure) application.
