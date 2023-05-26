# aspic-flexdd

### Input parameters

Call the program by invoking: 
```
java -jar aspic-flexdds.jar <INPUT FILE> [-g <GOAL>] [-i <INPUT FORMAT>] [-s] [-d <DFS>] [-t <TERMINATION CRITERION>] [-a <ADVANCEMENT TYPE>] [-o <ORDERING>]
```

where:
- `<INPUT FILE>` - path to the framework file
- `<GOAL>` single statement from the framework as the proponent's goal
- `<INPUT FORMAT>` - currently supported input formats ar:
  - `aspicp` - input format for ASPIC+ framework
  - `apx` - input format for ABA frameworks (with no defeasible rules, where every assumption is an ordinary premise), for examples of input frameworks [see below for input framework files](#input-frameworks)
- `-s` flag to solve and quit, yielding `YES` if satisfiable and `NO` otherwise
e.g. 
- `<DFS>` either `1` for depth-first search of `0` for breadth-first search
- `<ADVANCEMENT TYPE>` - advancement type, possible values are `DAB`, `DABF`, `DC`, `DS` (case insensitive)
- `<TERMINATION CRITERION>` -  termination criterion, possible values are `TA`, `TC`, `TS` (case insensitive)
- `<ORDERING>` - preference ordering on move types for automatic reasoner, e.g. `pf1pb2pf2pb1ob2of2ob1` 


```agsl
java -jar aspic-flexdds.jar examples/hofa2.5.aspicp -g t
```


### Current functionality (interactive)

- `?` - check possible moves
- `q` - quit
- `s` - show state (basic)
- `ss` - show state (all aux. definitions)
- `b` - backtrack 1 move
- `bb` - backtrack to the beginning
- `a` - finish the derivation automatically 
  - if successful state is found, input `ENTER` to confirm it or `;` to find the next one
- `m` - show the list of performed moves
- `ca <ADVANCEMENT TYPE>` - change advancement type to `<ADVANCEMENT TYPE>` possible values are `DAB`, `DABF`, `DC`, `DS` (case insensitive)
- `ct <TERMINATION CRITERION>` - change termination criterion type to `<TERMINATION CRITERION>` possible values are `TA`, `TC`, `TS` (case insensitive)
- `<MOVE TYPE> <INDEX>` - perform the `<INDEX>`-th move of type `<MOVE TYPE>`, possible values are `PB1`, `PB2`, `PF1`, `PF2`, `OB1`, `OB2`, `OF1`, `OF2` (case insensitive)

### <a href="#input-frameworks">Input frameworks</a>
