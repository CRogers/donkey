module Fol (Ref, Sigma, Fol(..), simplify, Entity, Domain, Predicate, pretty, tex) where

import Utils (replace, wrap)
import Data.List (intersperse)

-----------------------------------------------------------
-- Syntax

type Ref = String
type Sigma = [Ref]

data Fol =
	  T
	| F
	| Not   Fol
  | And   Fol Fol
  | Or    Fol Fol
  | Predi Ref [Ref]
  | Exists Ref Fol
  | All   Ref Fol
  | Impl  Fol Fol
  deriving (Eq, Show)

simp :: Fol -> Fol
simp (Not (Exists k p)) = simp (All k (Not p))
simp (Not (All k p)) = simp (Exists k (Not p))
simp (Not (And p q)) = simp (Impl p (Not q))
simp (Not (Impl p q)) = simp (And p (Not q))
simp (Not (Not p)) = simp p
simp (Not T) = F
simp (Not F) = T

simp (Impl p F) = simp (Not p)
simp (Impl p (Impl q r)) = simp (Impl (And p q) r)

simp (And p T) = simp p
simp (And p F) = F

-- Otherwise just let the call go through
simp (And p q) = And (simp p) (simp q)
simp (All k p) = All k (simp p)
simp (Exists k p) = Exists k (simp p)
simp (Not p) = Not (simp p)
simp (Impl p q) = Impl (simp p) (simp q)
-- We still don't have too many rules for Or, as we are quite interested in things like `foo v false`
simp (Or p q) = Or (simp p) (simp q)
simp p = p

simplify :: Fol -> Fol
simplify p = iterate simp p !! 10

pretty :: Fol -> String
pretty = fst . pretty' where
  pretty' T = ("⊤", 0)
  pretty' F = ("⊥", 0)
  pretty' (Not p) = ("¬" ++ wrap 0 (pretty' p), 0)
  pretty' (And p q) = (wrap 1 (pretty' p) ++ "∧" ++ wrap 1 (pretty' q), 1)
  pretty' (Or p q) = (wrap 2 (pretty' p) ++ "∨" ++ wrap 2 (pretty' q), 2)
  pretty' (Impl p q) = (wrap 3 (pretty' p) ++ "→" ++ wrap 3 (pretty' q), 3)
  pretty' (Predi f ks) = (f ++ "(" ++ concat (intersperse "," ks) ++ ")", 0)
  pretty' (Exists k p) = ("∃" ++ k ++ wrap 0 (pretty' p), 0)
  pretty' (All k p) = ("∀" ++ k ++ wrap 0 (pretty' p), 0)

tex :: Fol -> String
tex = fst . tex' where
  tex' T = ("\\top", 0)
  tex' F = ("\\bot", 0)
  tex' (Not p) = ("\\neg " ++ wrap 0 (tex' p), 0)
  tex' (And p q) = (wrap 1 (tex' p) ++ " \\wedge " ++ wrap 1 (tex' q), 1)
  tex' (Or p q) = (wrap 2 (tex' p) ++ " \\ve " ++ wrap 2 (tex' q), 2)
  tex' (Impl p q) = (wrap 3 (tex' p) ++ " \\rightarrow " ++ wrap 3 (tex' q), 3)
  tex' (Predi f ks) = (replace "-" "\\_" f ++ "(" ++ concat (intersperse "," ks) ++ ")", 0)
  tex' (Exists k p) = ("\\exists " ++ k ++ wrap 0 (tex' p), 0)
  tex' (All k p) = ("\\forall " ++ k ++ wrap 0 (tex' p), 0)


-----------------------------------------------------------
-- Interpretation

type Entity = Integer
type Domain = [Entity]
type Predicate = [Entity] -> Bool
type Interpretation = (Domain, Ref -> Entity, Ref -> Predicate)
type IFol = Interpretation -> Bool

patch :: Eq a => (a, b) -> (a -> b) -> a -> b
patch (x, y) f z | x == z    = y
                 | otherwise = f z

intFol :: Fol -> IFol
intFol T _ = True
intFol F _ = False
intFol (Not p) i = not (intFol p i)
intFol (And p q) i = intFol p i && intFol q i
intFol (Or p q) i = intFol p i || intFol q i
intFol (Impl p q) i = intFol p i || intFol q i
intFol (Predi name args) (_, eint, pint) = pint name (map eint args)
intFol (Exists var p) (dom, eint, pint) = any (intFol p) [(dom, patch (var,e) eint, pint) | e <- dom]
intFol (All var p) (dom, eint, pint) = all (intFol p) [(dom, patch (var,e) eint, pint) | e <- dom]
