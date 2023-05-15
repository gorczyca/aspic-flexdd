# aspic-flexdd

### Input parameters

Call the program by invoking: 
```
java -jar aspic-flexdds.jar <INPUT FILE> [-g <GOAL>]
```
e.g. 

```agsl
java -jar aspic-flexdds.jar examples/hofa2.5.aspicp -g t
```


### Current functionality

- `?` - check possible moves
- `q` - quit
- `s` - show state (basic)
- `ss` - show state (all aux. definitions)
- `b` - backtrack 1 move
- `m` - show the list of performed moves
- `ca <ADVANCEMENT TYPE>` - change advancement type to `<ADVANCEMENT TYPE>` possible values are `DAB`, `DABF`, `DC`, `DS` (case insensitive)
- `ct <ADVANCEMENT TYPE>` - change termination criteria type to `<TERMINATION CRITERIA>` possible values are `TA`, `TC`, `TS` (case insensitive)
- `<MOVE TYPE> <INDEX>` - perform the `<INDEX>`-th move of type `<MOVE TYPE>`, possible values are `PB1`, `PB2`, `PF1`, `PF2`, `OB1`, `OB2`, `OF1`, `OF2` (case insensitive)