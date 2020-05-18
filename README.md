# Rock-Spock

A collection of small programs related to Rock-Paper-Scissors.

## "nig-nag-nug"

 - Each player chooses value 1-4.
 - Choosing value just below opponent wins.
 - Otherwise, choosing highest value wins.
 - Choosing "1" gives you two points.

### What's the better strategy in this game?

It depends on the opponent, but assuming everyone plays uniformly random?

Analysing this,

 - 4 > 2 > 1
 - 3 > 4 > 1
 - 2 > 3
 - 1 > 2 (and scores two points)

Both hands 4 and 3 beat two other hands and lose to one hand.

Both hands 2 and 1 beat one other hand and lose to two hands.

### But what hand scores the most points?

```
$ stack ghci
λ> QC.generate (uniformGameGen 100000)
fromList [(Hand 1,49744),(Hand 2,25064),(Hand 3,12544),(Hand 4,12513)]
```

It seems that picking hand 1 gives the most points when playing against a
uniformly random player in spite of losing more games.

What if the next generation of strategies assumes a discrete distribution based
on how many points the first uniform strategy scores? How well would this perform
against the uniform strategy and itself?
