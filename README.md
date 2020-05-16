# nik-nak-nok

Rules:

 - Each player chooses value 1-4.
 - Choosing value just below opponent wins.
 - Otherwise, choosing highest value wins.
 - Choosing "2" gives you two points.

What's the better strategy in this game?

It depends on the opponent, but assuming everyone plays uniformly?

```
$ stack ghci
λ> generate (uniformGameGen 100000)
fromList [(Hand 1,31196),(Hand 2,50096),(Hand 3,12429),(Hand 4,6217)]
```

What if the next iteration of strategies assumes a distribution based on the first uniform strategy?
