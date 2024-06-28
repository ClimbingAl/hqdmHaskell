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