module MatchExperiment where

import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as L

-- | ADT Constructor
data Ctr = Ctr String [Ctr]
  deriving (Show, Eq, Ord)

-- | Pattern match case
data Cse = Cse String [Cse]
  deriving (Show, Eq, Ord)

{-

match x {
  (pair (unit nil) nil): _1
  (pair (unit cons) nil): _2
  (pair (unit nil) cons): _3
  (pair (unit cons) cons): _4
}

=>

[
  Ctr "pair" [Ctr "unit" [Ctr "nil" []], Ctr "nil" []],
  Ctr "pair" [Ctr "unit" [Ctr "cons" []], Ctr "nil" []],
  Ctr "pair" [Ctr "unit" [Ctr "nil" []], Ctr "cons" []],
  Ctr "pair" [Ctr "unit" [Ctr "cons" []], Ctr "cons" []]
]

=> (sort)

[
  Ctr "pair" [Ctr "unit" [Ctr "nil" []], Ctr "nil" []],
  Ctr "pair" [Ctr "unit" [Ctr "nil" []], Ctr "cons" []],
  Ctr "pair" [Ctr "unit" [Ctr "cons" []], Ctr "nil" []],
  Ctr "pair" [Ctr "unit" [Ctr "cons" []], Ctr "cons" []]
]

=> (group)

[Cse "pair"
  [Cse "unit"
    [Cse "nil"
      [Cse "nil" [],
       Cse "cons" []],
     Cse "cons"
      [Cse "nil" [],
       Cse "cons" []]]]]

=>

match x {
  pair: match x.fst {
    unit: match x.fst.val {
      nil: match x.snd {
        nil: _1
        cons: _3
      }
      cons: match x.snd {
        nil: _2
        cons: _4
      }
    }
  }
}

-}

-- Strategy: Ctr -> Cse, merge Cses

mergeCses :: [Cse] -> [Cse]
mergeCses = foldl f []
  where
    f [] cse = [cse]
    f ((Cse n1 a1) : xs) (Cse n2 a2) =
      if n1 == n2
        then Cse n1 (mergeCses (a1 ++ a2)) : xs
        else Cse n2 a2 : Cse n1 a1 : xs

-- mergeCse :: Cse -> Cse -> [Cse]
-- mergeCse c1@(Cse n1 a1) c2@(Cse n2 a2) =
--   if n1 == n2
--     then [Cse n1 (zipWith (_ . mergeCse) a1 a2)]
--     else [c1, c2]

ctrList =
  [ Ctr "pair" [Ctr "nil" [], Ctr "nil" []],
    Ctr "pair" [Ctr "nil" [], Ctr "cons" []],
    Ctr "pair" [Ctr "cons" [], Ctr "nil" []],
    Ctr "pair" [Ctr "cons" [], Ctr "cons" []]
  ]

cseList =
  [ Cse
      "pair"
      [ Cse
          "nil"
          [ Cse "nil" [],
            Cse "cons" []
          ],
        Cse
          "cons"
          [ Cse "nil" [],
            Cse "cons" []
          ]
      ]
  ]

-- ctrList =
--   [ Ctr "pair" [Ctr "unit" [Ctr "nil" []], Ctr "nil" []],
--     Ctr "pair" [Ctr "unit" [Ctr "nil" []], Ctr "cons" []],
--     Ctr "pair" [Ctr "unit" [Ctr "cons" []], Ctr "nil" []],
--     Ctr "pair" [Ctr "unit" [Ctr "cons" []], Ctr "cons" []]
--   ]

-- cseList =
--   [ Cse
--       "pair"
--       [ Cse
--           "unit"
--           [ Cse
--               "nil"
--               [ Cse "nil" [],
--                 Cse "cons" []
--               ],
--             Cse
--               "cons"
--               [ Cse "nil" [],
--                 Cse "cons" []
--               ]
--           ]
--       ]
--   ]

sCtrList = sort ctrList

-- >>> groupCtrs sCtrList
x =
  [ Ctr "pair" [Ctr "unit" [Ctr "cons" []], Ctr "cons" []]
      :| [ Ctr "pair" [Ctr "unit" [Ctr "cons" []], Ctr "nil" []],
           Ctr "pair" [Ctr "unit" [Ctr "nil" []], Ctr "cons" []],
           Ctr "pair" [Ctr "unit" [Ctr "nil" []], Ctr "nil" []]
         ]
  ]

-- For testing

data T1 = A T2 | B T2
  deriving (Eq, Ord, Show)

data T2 = C | D
  deriving (Eq, Ord, Show)

------------------------------------------------------
-- Thinking canvas

-- == Failed attempt 2: group ==

-- | Joins multiple constructors into a case.
-- Assumes all constructors have the same name.
-- joinCtr :: [NonEmpty Ctr] -> [Cse]
-- joinCtr [] = []
-- joinCtr ((ctr :| ctrs) : groups) =
--   let (Ctr name _) = ctr
--       args = getArgs (ctr : ctrs)
--    in _
--   where
--     getArgs :: [Ctr] -> [[Ctr]]
--     getArgs = foldr (\(Ctr _ args) ctrs -> args : ctrs) []

-- joinCtr :: L.NonEmpty Ctr -> Cse
-- joinCtr ctrs@(c :| cs) =
--   let (Ctr name args) = c
--    in Cse name (map joinCtr (groupCtrs args))
--   where
--     getArgs :: [Ctr] -> [[Ctr]]
--     getArgs = foldr (\(Ctr _ args) cs -> args : cs) []

-- groupCtrs :: [Ctr] -> [L.NonEmpty Ctr]
-- groupCtrs = L.groupBy (\(Ctr n1 _) (Ctr n2 _) -> n1 == n2)


-- == Failed attempt 1: LList ==

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
