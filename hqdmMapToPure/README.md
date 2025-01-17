# hqdmMapToPure

A command line tool to convert Magma Core data exported as triples (in a CSV file format) from Fuseki to a pure form that can be handled by the hqdmHaskell applications.

## General mapping process

1. Export `subject -> predicate -> object` statements from Magma Core (or whatever source is being used)
  - If Fuseki is being used then there is a CSV export feature.  This seems to decorate the exported IRIs and strings with additional spaces, inverted commas and commas.  A Python script, [PrepareHqdmTriplesForImport.py
](https://github.com/ClimbingAl/hqdmHaskell/blob/main/hqdmMapToPure/PrepareHqdmTriplesForImport.py), is provded in this repo to clean up CSVs exported from Fuseki to make them comatible with the standard Haskell CSV file import library `Data.Csv`.

Using the `hqdmMapToPure` command line tool:

2. Remove IRI URL parts, leaving only basic uuids and strings
3. Covert predicates to relation Ids
4. Save results in a specified file name, as a csv

## Example

`./hqdmMapToPure ../PureHqdmRelations_v9.csv ../HqdmAllAsDataFormal4Short.csv ../../datasets/eagleSystem-prepared.csv ../../datasets/eagleSystem-mapped.csv`

Note: the source file ("prepared.csv") and the output mapped file ("mapped.csv") are not provided in this repo but the HQDM model specification files are.

## General Command pattern

The command help switch (`hqdmMapToPure --help`) gives the following output:

```
Usage: hqdmMapToPure <hqdmRelations.csv> <hqdmEntityTypes.csv> <inputProcessedTriples.csv> <outputFilename.csv>
    --help  The command should have the general form: hqdmMapToPure hqdmRelations.csv hqdmEntityTypes.csv inputTriples.csv outputTriplesFilename.csv
```

## Running the command
This runs on any Linux command line as:

`./hqdmMapToPure ../PureHqdmRelations_v9.csv ../HqdmAllAsDataFormal4Short.csv ../../datasets/eagleSystem-prepared.csv ../../datasets/eagleSystem-mapped.csv`

It may run on macOS (could be straightforward) and Windows.  The binary for this is included in the repo so that youy don't have to build it.  If you don't like trusting binaries then you can rebuild it by installing the Haskell GHC and Cabal build system. The command `stack build` will then build this command line tool and all of its dependencies.  The tool can then be used using `stack` itself like this:

`stack exec -- hqdmMapToPure ../PureHqdmRelations_v9.csv ../HqdmAllAsDataFormal4Short.csv ../../datasets/eagleSystem-prepared.csv ../../datasets/eagleSystem-mapped.csv`

If you wish to use it without `stack` then you can build it with th `--copy-bins` switch. 

`stack build --copy-bins .`

This will copy the binary to the local bin path (you can then use it locally or copy it top where you need it) and as shown above. If you are using the Haskell dev container included in the vscode workspace [dev container file](https://github.com/ClimbingAl/hqdmHaskell/blob/main/.devcontainer/devcontainer.json) for this repo then the path to the built binary will be here: `/home/vscode/.local/bin/`.  You can move it to whereever you want it to be accessible.  If you do this, ensure that it has executable permissions (e.g. `-rwxr-xr-x`).

ToDo:
- Possibly list predicates that couldn't be converted?

#Example Output

The ouput from a successful mapping will look something like this:

```csv
0207de1e-4dd6-4ab5-9cf6-cbebbe0d67f7,7e249a64-9f13-47d3-a232-562a3d080198,functional_system
0207de1e-4dd6-4ab5-9cf6-cbebbe0d67f7,919a3f90-b681-422c-8481-fe313daa0044,2023-12-30T14:47:06.040256617Z
0207de1e-4dd6-4ab5-9cf6-cbebbe0d67f7,972bdd5f-5f8c-42d1-a47f-1ac08d1da48e,HqdmPatternProject_User1
0207de1e-4dd6-4ab5-9cf6-cbebbe0d67f7,fe987366-a8ad-48fa-8821-73f54f6df180,Descent_Module_Functional_System_1969-059D
0207de1e-4dd6-4ab5-9cf6-cbebbe0d67f7,2276fddc-a7bc-430d-9aed-be97b109a5cb,a7d315b4-bd7e-450c-a0aa-f62b405dea61
0207de1e-4dd6-4ab5-9cf6-cbebbe0d67f7,69b0e5b9-3be2-4ec3-a9a6-bb5b523d4b32,b75a1057-3cdd-45a2-884e-205f377ce320
0207de1e-4dd6-4ab5-9cf6-cbebbe0d67f7,8130458f-ae96-4ab3-89b9-21f06a2aac78,dabf85cb-c1e7-489b-aa66-0572dd1bd1dd
0e763e8b-7ce5-4f9b-a52b-efb3e1b34faa,7e249a64-9f13-47d3-a232-562a3d080198,functional_system_component
0e763e8b-7ce5-4f9b-a52b-efb3e1b34faa,919a3f90-b681-422c-8481-fe313daa0044,2023-12-30T14:47:06.039761518Z
0e763e8b-7ce5-4f9b-a52b-efb3e1b34faa,972bdd5f-5f8c-42d1-a47f-1ac08d1da48e,HqdmPatternProject_User1
0e763e8b-7ce5-4f9b-a52b-efb3e1b34faa,69b0e5b9-3be2-4ec3-a9a6-bb5b523d4b32,aba474ad-c827-4de2-9210-b4e4b964b38c
0e763e8b-7ce5-4f9b-a52b-efb3e1b34faa,163a237b-c25f-4062-84ee-695f217d6bec,efbd22e3-d26c-4a82-996d-ad644bc423f5
0e763e8b-7ce5-4f9b-a52b-efb3e1b34faa,fe987366-a8ad-48fa-8821-73f54f6df180,Descent_Module_System_Component_1969-059D
0e763e8b-7ce5-4f9b-a52b-efb3e1b34faa,3259ef23-d8c9-4771-a888-769f66380d0d,8e8b80f3-d9f0-4ede-a26c-f85903bd7c68
0e763e8b-7ce5-4f9b-a52b-efb3e1b34faa,8ea62706-fa07-40d7-8586-a8768403c01e,d966ea66-4aef-4634-b7f4-5cd956e0f0b5
0e763e8b-7ce5-4f9b-a52b-efb3e1b34faa,69b0e5b9-3be2-4ec3-a9a6-bb5b523d4b32,b75a1057-3cdd-45a2-884e-205f377ce320
0e763e8b-7ce5-4f9b-a52b-efb3e1b34faa,8130458f-ae96-4ab3-89b9-21f06a2aac78,a980b931-9769-4975-847a-5d81405b213b
```
