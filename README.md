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

### What hand scores the most points?

```haskell
$ stack ghci
λ> QC.generate (uniformGameGen 100000)
fromList [(Hand 1,49744),(Hand 2,25064),(Hand 3,12544),(Hand 4,12513)]
```

Picking hand 1 gives the most points when playing against a uniformly random
player in spite of losing more games. Maybe a player that picks hand 1 more
often will win, on average, against a uniformly random player.

### Pitting different strategies against one another

The following logic lets multiple players play each other, but we only use it
for two players for the initial hypothesis. At the core `roundM` plays a single
round and `tournament` gathers the score for multiple rounds:

```haskell
tournament :: Int -> Map Player (Gen Hand) -> Gen (Map Player Int)
tournament n = fmap (Map.unionsWith (+)) . replicateM n . roundM

roundM :: Map Player (Gen Hand) -> Gen (Map Player Int)
roundM = fmap decideWinners . traverse go . Map.assocs
  where
    go :: (Player, Gen Hand) -> Gen (Player, Hand)
    go (player, handGen) = fmap (player,) handGen

simple :: Map Player (Gen Hand)
simple = Map.fromList
  [ (Player 1, uniformHandGen)
  , (Player 2, uniformGameGen 10000 >>= frequencyMapGen)
  ]
```

Trying this out (this is very inefficient right now):

```haskell
λ> QC.generate (tournament 1000 simple)
fromList [(Player 1,500),(Player 2,454)]
```

Apparently the uniform player is better, so my initial gut feeling was wrong.
What if the next generation of strategies assumes some other discrete
distribution based on how many points the first uniform strategy scores? How
well would this perform against the uniform strategy and itself?
