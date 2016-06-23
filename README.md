# RegexMatcher
An  implementation of a regex matcher based on [Brzozowski derivatives](https://en.wikipedia.org/wiki/Brzozowski_derivative)

Regex defined in this language use the following BNF:
```ocaml
w := [A-Z a-z 0-9]

r :=  w
    | r*        (Iteration)
    | r1 + r2   (choice operation)
    | r1.r2     (sequencing)
    | (r)
```
