# hqdm

New project: `stack new helloworld new-template`

Build project (in project directory): `stack build`

Execute project:
```
    stack exec helloworld-exe
    someFunc
```

Test project: `stack test`

Watch with dependencies.  New ones that are not in the "System" module space likely need to be added to both `package.yaml` and `stack.yaml`.  To list buildt dependencies use: `stack ls dependencies`.

## ToDo
- compose graph structure to map HQDM to for functional parsing
- functions to:
    - derive super-subtype paths for all nodes
    - derive relation inheritance collections for all nodes
    - detect circularity
    - handle cardinalities (not present in HQDMAllAsData)