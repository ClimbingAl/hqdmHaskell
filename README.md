# Haskell parser for HQDM notated in a functionally-compatible form

:new: :star:
Command line tools for querying the functionally-compatible specification information of HQDM using the functions from this project: [`entityTypeSpec`](https://github.com/ClimbingAl/hqdmHaskell/tree/main/entityTypeSpec), [`relationPathUp`](https://github.com/ClimbingAl/hqdmHaskell/tree/main/relationPathUp), [`hqdmMapToPure`](https://github.com/ClimbingAl/hqdmHaskell/tree/main/hqdmMapToPure) and [`hqdmCardinalityChecker`](https://github.com/ClimbingAl/hqdmHaskell/tree/main/hqdmCardinalityChecker). :star:

This is an experiment in the functional parsing (and ultimately querying) of the HQDM data model.  This project started with another experiment to represent HQDM model itself as RDF triples - _subject_ <- _predicate_ -> _object_ statements, without the description logic baggage of RDFS and OWL.  The source data for that orgininal experiment model itself is the master HQDM AllAsData used in some of my other projects, such as [HQDM Patterns](https://climbingal.github.io/HqdmPatterns/).  That dataset has been used as the input to the representation of HQDM for functional parsing.  One of the goals of the work has been to see if compatability with other uses of HQDM can be achieved while removing any dependencies on them.  Any project that uses Magma Core is likely compatible with what I have done although my fork of it is the best place to look for some tweaks that help [Magma Core](https://github.com/ClimbingAl/MagmaCore/).  All that Magma Core-based datasets need is a binding to the [HqdmAllAsData](https://github.com/ClimbingAl/code-for-hqdm-patterns/blob/main/source-files/hqdmAllAsData.ttl) dataset that represents the model used for this functional parser.  This binding can be done with the `hqdmJoin` package in this repo.

## Repo purpose

The aim of this repo is to provide basic but accessible commands to provide specification information on the HQDM Entity Types and their *relationships* and do it in a way that is compatible with publicly available implementations of HQDM.  HQDM was originally documented in [a book](https://www.oreilly.com/library/view/developing-high-quality/9780123751065/) with the specification being [notated in EXPRESS](https://github.com/hqdmTop/hqdmFramework/blob/main/hqdm_framework.txt).  This has presented some challenges for data modellers and software developers alike, due to the number of entity types and the aparent complexity of the relationships between them.  After a failed attempt to reproduce the EXPRESS specification in OWL, it was clear that the logical comittments of the model were not compatible with OWL*.  Other Linked Data specifications, like RDFS*, don't have the expressive features to capture the comittments of the model so something else seemed necessary.  This repo is intended to take a fresh look at representing HQDM in a form that is netural to actual implementations for Enterprise Applications but can be used to:

1. Be a stable master for HQDM as a model, allowing version control and supporting documentation efforts.
2. Validate other implementations of HQDM.
3. Allow the development of a domain-specific language to test, query and validate data based on HQDM.

*- This is a big topic, but a dependence on standards that were not created with models like HQDM in mind didn't seem sensible.

## Going functional
A few principles were adopted to enable this consistent approach:

- HQDM Entity Types are the unique collections (or groups) that can, at least conceptually, be divided into sub-collections.  Some can also have instances (or elements).
- Entity Types and instances of them have unique identity.  The identity is universally unique and doesn't change over time.
- Formal features of HQDM, such as material things being comprised of _parts_ and being members of _sets_, are structural features within the model AND the data records based on it (they don't sit outside it)
- Relationships have identity and are handled as Binary Relation Sets (elements of those sets are the actual instances of these relations... the ordered pairs.. in the HQDM user data).

## Specification files

The actual HQDM entity type specification file used to apply the [HqdmLib](https://github.com/ClimbingAl/hqdmHaskell/blob/main/hqdm/src/HqdmLib.hs) functions to is a filtered version of the original hqdmAllAsData.ttl file (linked in the opening paragraph), listed as triple statements in a CSV file and with `rdf:type` replaced by `type` (to remove any suggestion that an external specification may be needed to interpret the model and to allow IRI paths to be ditched in favor of universally unique identifiers to represent anything that has identity).  This source data is stored in [HqdmAllAsDataFormal4Short.csv](https://github.com/ClimbingAl/hqdmHaskell/blob/main/HqdmAllAsDataFormal4Short.csv).  Previous versions of this file contained many additional (triple) statements relating to relationships within the HQDM model.  They have been removed as the handling of relationships (as Binary Relation sets of ordered pairs) is enabled by this separate file: [PureHqdmRelations_v8.csv](https://github.com/ClimbingAl/hqdmHaskell/blob/main/PureHqdmRelations_v8.csv) (see description below).

The first 2 lines of the original hqdmAllAsData triples relevant to having a record of the HQDM Entity Types and their subtype-supertype relationships was:

| subject | predicate | object|
| --- | --- | --- |
|hqdm:f5ac9254-2b93-4ed7-b89e-70994842b438|rdf:type|hqdm:state_of_biological_object|
|hqdm:f5ac9254-2b93-4ed7-b89e-70994842b438|hqdm:has_supertype|hqdm:f9cb048d-a2f7-4ff6-b824-c59b44e2aabe|

The original generation of hqdmAllAsData was done using MaggmaCore and pseudo-reliance on RDF but the principles for _hqdmHaskell_ demanded independence of external standards.  The namespace prefixes are therefore removed for use in hqdmHaskell

| subject | predicate | object|
| --- | --- | --- |
|f5ac9254-2b93-4ed7-b89e-70994842b438|type|state_of_biological_object|
|f5ac9254-2b93-4ed7-b89e-70994842b438|has_supertype|f9cb048d-a2f7-4ff6-b824-c59b44e2aabe|

When processed these triples are loaded using the following new data type, with `Id` being a <ins>Haskell type</ins> synonym of the `String` <ins>Haskell type</ins>:

```haskell
    data HqdmTriple = HqdmTriple
      { subject :: !Id,
        predicate :: !Id,
        object :: !Id
      }
      deriving (Show, Eq, Generic)
```

It turns out that relations (Binary Relations) are more complex to address than Entity Types.  Entity Types are taken to be collections of records that are all of a common 'type' (we'll avoid calling them sets because sets and their elements have a place within the Entity Type model itself).  The _relationship_ lines between the Entity Type boxes in the original EXPRESS-G version of [HQDM](https://github.com/hqdmTop/hqdmFramework/wiki/) are under-specified as the EXPRESS standard does not accommodate relationships between.... other relationships.  This means that these sets of relationships.  We are not violating anything here by calling them sets because we are talking about [Binary Relation](https://en.wikipedia.org/wiki/Binary_relation) Sets and not the 2-sorted sets that are introduced within the HQDM Entity Type hierarchy.  Matthew West used a naming convention to notate correspondence between relationships in the EXPRESS version of HQDM.  This approach was not easy to use, was incomplete and required analysis (and debate) to work out what some of them likely meant.  Attempts to represent these relationships in other languages (e.g. RDFS & SHACL) had to work round these issues, adopt the ambiguous representation of relationships or avoid them altogether.  This project takes a ground-up approach to Binary Relations as Binary Relation Sets, all of which are sub-binaryRelationSets of a top level Binary Relation Set called:

_`universal_relation_set`_

Further documentation will soon be provided on the top level structure of Binary Relation Sets but all other Binary Relation Sets are a sub-BinaryRelation Set of this universal relation set.  The relation sets have been recomputed for the whole of HQDM and the sub- and super- set relations between them, domains, ranges, Cardinality information and original name specified by Matthew are stored in a Haskell New Data Type called `HqdmBinaryRelationPure` specified as:

```haskell
    -- | HqdmBinaryRelationPure
    -- A data type that uses only identities to specify the xR'y of a HQDM Binary 
    data HqdmBinaryRelationPure = HqdmBinaryRelationPure
      { pureDomain :: !HqdmLib.Id,
        pureBinaryRelationId :: !RelationId,  -- Relation unique Id (uuid)
        pureBinaryRelationName :: String,     -- Name of Binary Relation Set (doesn't need to be unique)
        pureRange:: !HqdmLib.Id,              -- Range Set ids
        pureHasSuperBR :: [RelationId],       -- SuperBR Set Ids (empty if none)
        pureCardinalityMin :: Int,            -- 0,1,...
        pureCardinalityMax :: Int,            -- -1 (indicates no max!),0,1,2,...
        pureRedeclaredBR :: Bool,             -- True means this is redeclared from its SuperBR
        pureRedeclaredFromRange :: HqdmLib.Id -- reserved
      }
      deriving (Show, Eq, Generic)
```

The data is kept separate from the functions used to parse it, with the CSV data corresponding to the `HqdmBinaryRelationPure` data type being stored as follows:

```csv
Domain, Relation Id, Relation Original Name, Range, Super Binary Relation(s), Min Cardinality, Max Cardinality, Redeclared from Super Binary Relation, RESERVED
```
The resulting file looks like this:

```csv
f5ac9254-2b93-4ed7-b89e-70994842b438,d6865f69-4c0c-4a3d-9d95-803ddce86b82,temporal_part_of,21c1fe82-1b48-46b3-8df8-6bddcbb2d92e,4096218c-eac7-44a6-90f8-e7ac21599c85,0,-1,True,
f5ac9254-2b93-4ed7-b89e-70994842b438,ce789976-153f-45da-81cf-70f6665c0853,member_of,6beb4515-89e8-4bce-9318-77935200f9bb,96a99f0a-1e95-48a1-976e-2d50a0610d64,0,-1,True,
f5ac9254-2b93-4ed7-b89e-70994842b438,94d5cc9f-677f-4b4a-a822-4a9c13efcb8d,temporal__part_of,dd8359c4-0a2b-416a-af27-3bcb0551b41a,e1345cdd-b768-4c6a-a5e5-2931ffe5d337,0,-1,False,
f5ac9254-2b93-4ed7-b89e-70994842b438,be3010c6-ef33-4291-8af6-ddb8d1c0f689,part_of_possible_world,a5dc8c36-56fc-44c2-909b-7592b1f44fd7,7cb00c15-78c8-4d05-b253-f6c5e4065a33,1,-1,False,
f5ac9254-2b93-4ed7-b89e-70994842b438,d59061a7-1c02-4f0f-b157-2a6fa668bb9d,consists__of,dd8359c4-0a2b-416a-af27-3bcb0551b41a,a00d2246-1c61-428f-88a5-4fbc4fadda66,0,1,False,
f5ac9254-2b93-4ed7-b89e-70994842b438,f6ebbbfd-c0c5-491c-8da2-2fdb5b822d15,ending,545b4541-8a34-46b8-8704-2265be0244c3,06c09931-3a05-4754-ae49-67dc4696ab4d,0,-1,False,
f5ac9254-2b93-4ed7-b89e-70994842b438,fd7144fc-b82c-414b-809b-4bd87a66531a,beginning,545b4541-8a34-46b8-8704-2265be0244c3,140b6769-bf45-4aea-b35f-f0c7c3089caa,0,-1,False,
f5ac9254-2b93-4ed7-b89e-70994842b438,ee9dbef6-449f-443a-bdfe-6a0c026bddb6,part__of,dd8359c4-0a2b-416a-af27-3bcb0551b41a,b2c653f1-4e1d-48d8-83d0-f77abb1347d0,0,-1,False,
f5ac9254-2b93-4ed7-b89e-70994842b438,a6472825-0e97-46cb-a9ec-5a512343705b,member__of,4a8cba08-035c-4902-935b-26da61ed282c,ac2f46ca-cea1-439a-9284-4065a7094b70,0,-1,False,
f5ac9254-2b93-4ed7-b89e-70994842b438,5ec56b95-7945-45ce-b3eb-c96bda096cc9,aggregated_into,dd8359c4-0a2b-416a-af27-3bcb0551b41a,f4fe15fe-c015-4c6d-a32f-7d9ff8b7e312,0,-1,False,
f5ac9254-2b93-4ed7-b89e-70994842b438,13d7b63b-8428-4f97-9c38-15254d2af135,member_of,09ff1076-4557-48d3-b4eb-e4db4e8824ca,96a99f0a-1e95-48a1-976e-2d50a0610d64,0,-1,False,
f5ac9254-2b93-4ed7-b89e-70994842b438,2adc222f-927f-41f6-8497-74567bd6e49b,temporal_part_of,db723822-93f7-4c1d-8f17-fb094a5c10d9,4096218c-eac7-44a6-90f8-e7ac21599c85,0,-1,False,
f5ac9254-2b93-4ed7-b89e-70994842b438,b785bbc9-5ddf-4225-9d55-6162fbf211c6,member_of,fabc65fd-5fe0-4f68-87bb-b4eb93eb2f1b,96a99f0a-1e95-48a1-976e-2d50a0610d64,0,-1,False,
f5ac9254-2b93-4ed7-b89e-70994842b438,079d4bef-cee0-455b-a1c1-743fe9d8bb6c,temporal_part_of,58107227-267a-4f2e-a44a-25bb61c6a455,4096218c-eac7-44a6-90f8-e7ac21599c85,0,-1,False,
f5ac9254-2b93-4ed7-b89e-70994842b438,38639ead-3a1b-46d3-8aa6-d0698bfbb678,member_of,bb6f6d3f-1ed1-41ab-942c-6b3667c5da37,96a99f0a-1e95-48a1-976e-2d50a0610d64,0,-1,False,
```

All the Entity Type NodeIds are preserved from the hqdmAllAsData experiment, with all the predicate names and HQDM Entity Types as-per HQDM as documented (and as in Magma Core).  The following example `HqdmBinaryRelationPure` entry shows the populated fields for the first entry in the PureHqdmRelations file, showing the domain uuid for `state_of_biological_object`, unique Binary Relation Id for the originally named `temporal_part_of` relationship, the range uuid for `physical_object`, the uuid for a single Super Binary Relation Set and then specified properties for Cardinality constraints and redeclaration:

```
HqdmBinaryRelationPure {pureDomain = "f5ac9254-2b93-4ed7-b89e-70994842b438", pureBinaryRelationId = "hqdmRelation:2adc222f-927f-41f6-8497-74567bd6e49b", pureBinaryRelationName = "temporal_part_of", pureRange = "db723822-93f7-4c1d-8f17-fb094a5c10d9", pureHasSuperBR = "4096218c-eac7-44a6-90f8-e7ac21599c85", pureCardinalityMin = 0, pureCardinalityMax = -1, pureRedeclaredBR = False, pureRedeclaredFromRange = ""},
```

It should be noted that in the original documentation for HQDM there was considerable re-use of relationship names in the super-subtype hierarchy.  Those names have been preserved in the Binary Relation Sets specifications but they have no role in the identity of the relations within the Haskell functions.  They are, however, used for the mapping to/from externally generated datasets such as those generated using MagmaCore.  Ther are currently 2764 Binary Relation Sets created for the hqdmHaskell codebase, corresponding to the originally specified HQDM relationships and the inheritance relations the computed when developing *hqdmHaskell*.

### Functions in HqdmLib (TBC)

All the functions in HQDMLib are pure Haskell with minimised guards; that is, guards tend to be employed solely to terminate a recursive function (or list comprehension) on an Empty List (e.g. [] or [[]]) or when the root Entity Type Thing is found.  The functions each have a short description in the comments above it.  The functions have not been written for efficiency, with the focus being on purity.  Variable names and function names have been chosen to indicate the purpose or role.  This should make them fairly readable as long as you are familiar with (or tolerant to) the Haskell function notations (e.g. `.`, `$`, `<-`, `\` lambda expressions, and built-in functions).

## Quick Links
ADD LINKS!!
- Basic processing of the hqdmAllAsData triples - subtype and supertype hierarchies, node id <-> entity type name queries, etc
- Functional re-calculation of the inherited relations
- Representation of Relations adopting the cardinalities and other restrictions from `hqdm.exp`
- Joining of MagmaCore data with the functional core of hqdmAllAsData
- Functional queries over joined MagmaCore datasets

## Install and Execute

Install Haskell Glasgow Haskell Compiler with Cabal & Stack: https://www.haskell.org/ghcup/

After cloning the repo first go to the `hqdm` folder and run the following 2 commands:

    `stack build`
    `stack exec hqdm-exe` (this runs the Haskell executable resulting from building the Main.hs file)

This should result in a number of illustrative results for most of the functions in HqdmLib being written to the console.

The `inheritance` folder contains a Haskell Main.hs file to calculate the inherited relations for each of the HQDM Entity Types.  It is best to pipe the output to a file as the console may not cope with the size of the output.  Run the following 2 commands:

  `stack build`
  `stack exec inheritance > HqdmTypesAndInheritedRels.txt`

This text file should match the text file of that name in this repo.  
Note: The `hqdm` and `inheritance` folders use different versions of Cabal, which explains why the `inheritance` one doesn't need `-exe` to post-fix the folder name when executing it.  I'll clean this up sometime.

## ToDo

- [x] Regenerate the triples to find any missing ones from the input data (there will be some).
- [x] Add Cardinalities that are in the original [HQDM EXPRESS file](https://github.com/hqdmTop/hqdmFramework/blob/main/hqdm_framework.txt).
- [x] Add relation specialisation (i.e. implement functions to apply the specialisation of one predicate being a specialisation of another).
- [x] Do a test with binding to a Magma Core dataset (perhaps NetworksBasic).  I'm thinking about using `hqdm:instance_of` instead of `rdf:type`, to remove logical ambiguity.
- [ ] Build a Domain Specific Language to enable HQDM operations (initially for query).
- [ ] Add data modification and exception handling.

Done but not documented:   Re-create triples (using collapsed sets) and compare with original set.

Notes on relations, cardinalities, etc
    - [x] compute/identify the missing relations from the pre-constructed hqdmRelations
        -- Add handling of pureRedeclaredFromRange
    - [x] compute all relations and their relation-supersubset relationships
H   - [ ] test mc datasets to see that they conform to cardinalities
    - [ ] calculate the inverse relations
    - [ ] perform predicate searches that include inverse relations
    - [x] calculate all allowed relations (based on inheritance, not the sets of all possible relations)
    - [ ] introduce the powerset?... probably best to miss this out, but could be useful to think it through

    - Functions to parse hqdm patterns on mapped user data:
        - set & parts (inc. specializations of them)
        - temporal parts and their cardinalities (as optional test)
        - activity and association
        - system and component
        - representation_by_sign
        - possible worlds
        - specialization of associations

    - [ ] Aspire to make this a DSL


## Caveats
This implementation uses pure Haskell with no Monadic functions (once the data has loaded).  This means that if any errors are introduced in the source data the GHC Executable will fail if an exception is hit.  I am happy with this, as it is intended to be a formal (pure) application.  For some of the IO aspects I have done some exception handling, particularly for the command line utilities.
