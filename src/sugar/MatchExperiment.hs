module MatchExperiment where

data Ctr = Ctr String [Ctr]
  deriving (Show, Eq, Ord)

-- transform :: Ctr -> LList String
-- transform (Ctr name []) = name :. End
-- transform (Ctr name xs) = _

{-

match x {
  (pair nil nil)
  (pair cons nil)
  (pair nil cons)
  (pair cons cons)
}

=>

[
  (pair, [(nil, []), (nil, [])]),
  (pair, [(cons, []), (nil, [])]),
  (pair, [(nil, []), (cons, [])]),
  (pair, [(cons, []), (cons, [])])
]

=> (ordena)

[
  (pair, [(nil, []), (nil, [])]),
  (pair, [(nil, []), (cons, [])]),
  (pair, [(cons, []), (nil, [])]),
  (pair, [(cons, []), (cons, [])])
]

=>

[
  match x {
    pair: match x.fst {
      
    }
  }
]

-}


-- For testing

data T1 = A T2 | B T2
  deriving (Eq, Ord, Show)

data T2 = C | D
  deriving (Eq, Ord, Show)

------------------------------------------------------
-- Thinking canvas

-- | List of lists with increasingly large dimensions.
--
-- Example:
-- >>> llist_ex = 1 :. [2] :. [[3]] :. End
data LList a = a :. (LList [a]) | End
  deriving (Show, Eq, Ord)

infixr 9 :.

-- [
--   x :. [x0]     :. End,
--   y :. [y0, y1] :. [[y2, y3], [y4]] :. End,
--   z :. [z0, z1] :. [[z2], [z3, z4]] :. End,
--   ..
-- ]

--    [x, y, z, ..]
-- :. [[x0], [y0, y1], [z0, z1], ..]
-- :. [[[y2, y3], [y4]], [[z2], [z3, z4]], ..]
-- :. End

detach :: [LList a] -> [(a, LList [a])]
detach [] = []
detach (ll : lls) = case ll of
  End -> detach lls
  x :. xs -> (x, xs) : detach lls

flatten :: [LList a] -> LList [a]
flatten [] = End
flatten lls =
  let (heads, tails) = unzip (detach lls)
   in heads :. flatten tails

-- transform :: [[T]] -> [[T]]
-- transform = groupBy (\p1 p2 -> fst p1 == fst p2) . sort

-- >>> transform [(A, A), (B, A), (B, B), (A, B)]
-- [[(A,A),(A,B)],[(B,A),(B,B)]]
