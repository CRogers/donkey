module Prop (Ref, Prop(..), simplify, tostring) where

import Data.List

type Ref = String

data Prop =
	  T
	| F
	| Not   Prop
    | And   Prop Prop
    | Or    Prop Prop
    | Predi Ref [Ref]
    | Exist Ref Prop
    | All Ref Prop
    | Impl Prop Prop
    deriving (Eq, Show)

simp :: Prop -> Prop
simp (Not (Exist k p)) = simp (All k (Not p))
simp (Not (All k p)) = simp (Exist k (Not p))
simp (Not (And p q)) = simp (Impl p (Not q))
simp (Not (Impl p q)) = simp (And p (Not q))
simp (Not (Not p)) = simp p
simp (Not T) = F
simp (Not F) = T


simp (Impl p F) = simp (Not p)
simp (Impl p (Impl q r)) = simp (Impl (And p q) r)

simp (And p T) = simp p
simp (And p F) = F

simp (And p q) = And (simp p) (simp q)
simp (All k p) = All k (simp p)
simp (Exist k p) = Exist k (simp p)
simp (Not p) = Not (simp p)
simp (Impl p q) = Impl (simp p) (simp q)

-- We still don't have too many rules for Or, as we are quite interested in things like `foo v false`
simp (Or p q) = Or (simp p) (simp q)

simp p = p

fexp :: (a -> a) -> Int -> (a -> a)
fexp f e = (iterate (f.) id) !! e

simplify :: Prop -> Prop
simplify = fexp simp 10

-- Printing

wrap :: Int -> (String, Int) -> String
wrap n (p, k) | k > n     = "(" ++ p ++ ")"
              | otherwise = p

pretty :: Prop -> (String, Int)
pretty T = ("⊤", 0)
pretty F = ("⊥", 0)
pretty (Not p) = ("¬" ++ wrap 0 (pretty p), 0)
pretty (And p q) = (wrap 1 (pretty p) ++ "∧" ++ wrap 1 (pretty q), 1)
pretty (Or p q) = (wrap 2 (pretty p) ++ "∨" ++ wrap 2 (pretty q), 2)
pretty (Impl p q) = (wrap 3 (pretty p) ++ "→" ++ wrap 3 (pretty q), 3)
pretty (Predi f ks) = (f ++ "(" ++ concat (intersperse "," ks) ++ ")", 0)
pretty (Exist k p) = ("∃" ++ k ++ wrap 0 (pretty p), 0)
pretty (All k p) = ("∀" ++ k ++ wrap 0 (pretty p), 0)

tex :: Prop -> (String, Int)
tex T = ("\\top", 0)
tex F = ("\\bot", 0)
tex (Not p) = ("\\neg " ++ wrap 0 (tex p), 0)
tex (And p q) = (wrap 1 (tex p) ++ " \\wedge " ++ wrap 1 (tex q), 1)
tex (Or p q) = (wrap 2 (tex p) ++ " \\ve " ++ wrap 2 (tex q), 2)
tex (Impl p q) = (wrap 3 (tex p) ++ " \\rightarrow " ++ wrap 3 (tex q), 3)
tex (Predi f ks) = (f ++ "(" ++ concat (intersperse "," ks) ++ ")", 0)
tex (Exist k p) = ("\\exists " ++ k ++ wrap 0 (tex p), 0)
tex (All k p) = ("\\forall " ++ k ++ wrap 0 (tex p), 0)

tostring :: Prop -> String
tostring = fst . pretty
