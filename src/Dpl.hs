module Dpl (Dpl(..), tex, pretty, simplify, Assignment, intDpl, intDpl2, intDpl3) where

import Fol (Ref, Sigma, Fol(..), Entity, Domain, Predicate)

import Data.List (intersperse)
import Utils (wrap, replace)
import Control.Monad ((>=>))

-----------------------------------------------------------
-- Syntax

data Dpl =
    DT | DF
  | DC Dpl Dpl
  | DD Dpl Dpl
  | DN Dpl
  | DI Dpl Dpl
  | DE Ref
  | DP Ref [Ref]
  deriving (Eq, Show)

-- Printing functions

tex :: Dpl -> String
tex = fst . tex' where
  tex' DT = ("\\top", 0)
  tex' DF = ("\\bot", 0)
  tex' (DC p q) = (wrap 1 (tex' p) ++ " \\cdot " ++ wrap 1 (tex' q), 1)
  tex' (DD p q) = (wrap 2 (tex' p) ++ " \\vee " ++ wrap 2 (tex' q), 2)
  tex' (DI p q) = (wrap 2 (tex' p) ++ " \\rightarrow " ++ wrap 2 (tex' q), 3)
  tex' (DN p) = ("\\neg " ++ wrap 0 (tex' p), 0)
  tex' (DE k) = ("\\exists " ++ k, 0)
  tex' (DP f ks) = (replace "-" "\\_" f ++ "(" ++ concat (intersperse "," ks) ++ ")", 0)

pretty :: Dpl -> String
pretty = fst . pretty' where
  pretty' DT = ("⊤", 0)
  pretty' DF = ("⊥", 0)
  pretty' (DC p q) = (wrap 1 (pretty' p) ++ "⋅" ++ wrap 1 (pretty' q), 1)
  pretty' (DD p q) = (wrap 2 (pretty' p) ++ "∨" ++ wrap 2 (pretty' q), 2)
  pretty' (DI p q) = (wrap 2 (pretty' p) ++ "→" ++ wrap 2 (pretty' q), 3)
  pretty' (DN p) = ("¬" ++ wrap 0 (pretty' p), 0)
  pretty' (DE k) = ("∃" ++ k, 0)
  pretty' (DP f ks) = (f ++ "(" ++ concat (intersperse "," ks) ++ ")", 0)


simplify :: Dpl -> Dpl
simplify p = iterate simp p !! 10

simp :: Dpl -> Dpl
simp (DC p DT) = simp p
simp (DC DT p) = simp p
simp (DC p DF) = DF
simp (DC DF p) = DF
simp (DI p DF) = DN p
-- scope related
simp (DI DT (DI DT p)) = DT `DI` simp p
simp (DN (DN p)) = DT `DI` simp p
-- Fall throughs
simp (DC p q) = simp p `DC` simp q
simp (DD p q) = simp p `DD` simp q
simp (DI p q) = simp p `DI` simp q
simp (DN p) = DN (simp p)
simp p = p
-- See the paper on why the following disjunction rules are invalid
-- simp (DD p DT) = DT
-- simp (DD DT p) = DT
-- simp (DD p DF) = simp p
-- simp (DD DF p) = simp p

-----------------------------------------------------------
-- Interpretation

type Assignment = [(Ref, Entity)]
type Interpretation1 = (Sigma, Domain, Ref -> Predicate)
type IDpl = Interpretation1 -> Assignment -> Assignment -> Bool

intDpl :: Dpl -> IDpl
intDpl DT _ x y                      = x == y
intDpl DF _ x y                      = False
intDpl (DC p q) i@(sigma,dom,_) x y  = or [x `r` z && z `s` y | z <- assignments]
    where (r, s) = (intDpl p i, intDpl q i)
          assignments = sequence [[(k,v) | v <- dom] | k <- sigma]
intDpl (DN p) i x y                  = x == y && not (x `r` y) where r = intDpl p i
intDpl (DI p q) i x y                = intDpl (DN (p `DC` (DN q))) i x y
intDpl (DE var) i x y                = hide var x == hide var y where hide key = filter (\(x,y) -> x /= key)
intDpl (DP name args) (_,_,intp) x y = x == y && intp name (map (lookup' x) args)

-- Like `lookup', but throws an error in case the entity was not available.
-- We can use this version since our formulas are guarenteed to be valid.
lookup' :: Assignment -> Ref -> Entity
lookup' xys key | Just e <- lookup key xys = e
                | otherwise                = error ("Variable not in scope: " ++ key)



type Interpretation2 = (Domain, Ref -> Predicate)
type IDpl2 = Interpretation2 -> Assignment -> [Assignment]

intDpl2 :: Dpl -> IDpl2
intDpl2 DT _                    = return
intDpl2 DF _                    = const []
intDpl2 (DC p q) i              = intDpl2 p i >=> intDpl2 q i
intDpl2 (DN p) i                = \x -> if null (intDpl2 p i x) then [[]] else []
intDpl2 (DI p q) i              = intDpl2 (DN (p `DC` (DN q))) i
intDpl2 (DE var) (dom,_)        = \x -> [set var val x | val <- dom]
intDpl2 (DP name args) (_,pint) = \x -> if pint name (map (lookup' x) args) then [] else [x]

set :: Ref -> Entity -> Assignment -> Assignment
set k v xys = (k,v) : filter (\(x,y) -> x /= k) xys



intDpl3 :: Dpl -> Fol
intDpl3 p = addDpl T [p]

addDpl :: Fol -> [Dpl] -> Fol
addDpl p [] = p
addDpl p (DT:ds) = addDpl p ds
addDpl p (DF:ds) = F
addDpl p ((DC d1 d2):ds) = addDpl p (d2:d1:ds)
addDpl p ((DN d):ds) = addDpl (And (Not (addDpl T [d])) p) ds
addDpl p ((DI d1 d2):ds) = addDpl p ((DN (DC d1 (DN d2))):ds)
addDpl p ((DE k):ds) = addDpl (Exists k p) ds
addDpl p ((DP f ks):ds) = addDpl (And (Predi f ks) p) ds
addDpl p ((DD d1 d2):ds) = Or (addDpl p (d1:ds)) (addDpl p (d2:ds))


-- More possible interpretation includes Visser's disjunction interpretation
