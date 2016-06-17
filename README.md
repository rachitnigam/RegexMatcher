# RegexMatcher
An  implementation of a regex matcher based on Brzozowski derivatives

Regex defined in this language use the following BNF:
```
w := [A-Za-z0-9]

r :=  w
    | r*
    | r1 + r2   (choice operation)
    | r1.r2     (sequencing)
    | (r)
```
