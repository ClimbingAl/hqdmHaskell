# hqdm

This was the original starting point for the development of a functionally-compatible version of HQDM.  The `Main.hs` file was primarily used for basic test and development purposes.  However, the packages in the source (`./src`) directory are used as dependencies in most of the other Haskell project files.  They are:

`HqdmLib.hs`    Functions to represent the entity type specifications of HQDM and query over them for subtype and supertype purposes.  
`HqdmInspection.hs`     A stub that is hardly used.  It will be deleted soon.
`HqdmIds.hs`    All HQDM uuids mapped to Haskell functions bearing the name of the function.  This is merely a convenient library to refer to entity types in code without having to ues the uuids directly.  This is mostly used for test purposes.
`HqdmMermaid.hs` A stub for generating Mermaid diagrams from the type hierarchies available from some of the HqdmLib functions.  In development.

### Notes
Watch with dependencies.  New ones that are not in the "System" module space likely need to be added to both `package.yaml` and `stack.yaml`.  To list buildt dependencies use: `stack ls dependencies`.
Also, the Cabal file for this is for a slightly older version of Cabal.  To build it `stack build` works but it needs `-exe` added to the execution command like: `stack exec hqdm-exe`.

## ToDo - Essentially all done but not just in this package.
- compose graph structure to map HQDM to for functional parsing
- functions to:
    - derive super-subtype paths for all nodes
    - derive relation inheritance collections for all nodes
    - detect circularity
    - handle cardinalities (not present in HQDMAllAsData)
