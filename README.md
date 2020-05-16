# nig-nag-nug

## Rules

 - Each player chooses value 1-4.
 - Choosing value just below opponent wins.
 - Otherwise, choosing highest value wins.
 - Choosing "1" gives you two points.

## What's the better strategy in this game?

It depends on the opponent, but assuming everyone plays uniformly random?

Analysing this,

 - 4 > 2 > 1
 - 3 > 4 > 1
 - 2 > 3
 - 1 > 2 (and scores two points)

Both hands 4 and 3 beat two other hands and lose to one hand.

Both hands 2 and 1 beat one other hand and lose to two hands.

And hand 1 scores two points if it wins.

## But what hand scores the most points?

```
$ stack ghci
λ> generate (uniformGameGen 100000)
fromList [(Hand 1,31196),(Hand 2,50096),(Hand 3,12429),(Hand 4,6217)]
```

What if the next iteration of strategies assumes a distribution based on the first uniform strategy?
