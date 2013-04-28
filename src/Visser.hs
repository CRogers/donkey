module Visser (Visser(..), simplify, tex, pretty, cleanVariables, intVisser1, intVisser2, intVisser3) where

import Fol (Ref, Fol(..))
import Dpl (Dpl(..))
import Stanford (variables)

import Data.List (intersperse, nub)
import Utils (replace)

-- Syntax

data Visser =
    VT | VF
  | VC Visser Visser
  | VD Visser Visser
  | VE Ref
  | VP Ref [Ref]
  | VSw
  | VSc
  | VQ0 Visser
  | VQ1 Visser
  deriving (Eq, Show)

tex :: Visser -> String
tex VT = "\\top"
tex VF = "\\bot"
tex VSw = "\\Bowtie"
tex VSc = "\\triangle"
tex (VC p q) = tex p ++ " \\cdot " ++ tex q
tex (VD p q) = "(" ++ tex p ++ ") \\vee (" ++ tex q ++ ")"
tex (VE k) = "\\exists " ++ k
tex (VP f ks) = replace "-" "\\_" f ++ "(" ++ concat (intersperse "," ks) ++ ")"
tex (VQ0 p) = "?_0(" ++ tex p ++ ")"
tex (VQ1 p) = "?_1(" ++ tex p ++ ")"

pretty :: Visser -> String
pretty VT = "⊤"
pretty VF = "⊥"
pretty VSw = "⋈ "
pretty VSc = "△"
pretty (VC p q) = pretty p ++ "⋅" ++ pretty q
pretty (VD p q) = "(" ++ pretty p ++ ")∨(" ++ pretty q ++ ")"
pretty (VE k) = "∃" ++ k
pretty (VP f ks) = f ++ "(" ++ concat (intersperse "," ks) ++ ")"
pretty (VQ0 p) = "?0(" ++ pretty p ++ ")"
pretty (VQ1 p) = "?1(" ++ pretty p ++ ")"

simp :: Visser -> Visser
simp (VC p VT) = simp p
simp (VC VT p) = simp p
--simp (VC p VF) = VF
--simp (VC VF p) = VF
-- TODO: Get rid of conjunctive, equal stream devices △△, ⋈⋈ etc.
-- Fall throughs
simp (VC p q) = simp p `VC` simp q
simp (VD p q) = simp p `VD` simp q
simp (VQ0 p) = VQ0 (simp p)
simp (VQ1 p) = VQ1 (simp p)
simp p = p

simplify :: Visser -> Visser
simplify p = iterate simp p !! 2

cleanVariables :: Visser -> Visser
cleanVariables p = foldl rename p (zip used im ++ zip im variables)
  where
    used = nub (listem p)
    im = map (++"0") used
    listem (VE k) = [k]
    listem (VP f ks) = ks
    listem (VC p q) = listem p ++ listem q
    listem (VD p q) = listem p ++ listem q
    listem (VQ0 p) = listem p
    listem (VQ1 p) = listem p
    listem p = []
    rename (VE k) (x,y) = if k == x then VE y else VE k
    rename (VP f ks) (x,y) = VP f [if k == x then y else k | k <- ks]
    rename (VC p q) (x,y) = VC (rename p (x,y)) (rename q (x,y))
    rename (VD p q) (x,y) = VD (rename p (x,y)) (rename q (x,y))
    rename (VQ0 p) (x,y) = VQ0 (rename p (x,y))
    rename (VQ1 p) (x,y) = VQ1 (rename p (x,y))
    rename p (x,y) = p

-- Interpretation

type IVisser1 = (Dpl, Dpl, Integer)

intVisser1 :: Visser -> IVisser1
intVisser1 VT = (DT, DT, 1)
intVisser1 VF = (DT, DF, 1)
intVisser1 VSw = (DT, DT, -1)
intVisser1 (VP p rs) = (DT, DP p rs, 1)
intVisser1 (VE r) = (DT, DE r, 1)
intVisser1 (VC vis1 vis2) = compVisser1 (intVisser1 vis1) (intVisser1 vis2)
intVisser1 (VQ0 vis) = (DT, qm `DI` qp, 1) where (qm, qp, _) = intVisser1 vis
intVisser1 (VQ1 vis) = error "?1 is not defined in Visser1"

compVisser1 :: IVisser1 -> IVisser1 -> IVisser1
compVisser1 (qm, qp, 1) (rm, rp, b) = (qm`DC`rm, qp`DC`rp, b)
compVisser1 (qm, qp, -1) (rm, rp, b) = (qm`DC`rp, qp`DC`rm, -b)



type IVisser2 = (Dpl, Dpl, Dpl, Dpl, Integer, Integer)

intVisser2 :: Visser -> IVisser2
intVisser2 VT = (DT, DT, DT, DT, 1, 0)
intVisser2 VF = (DT, DT, DT, DF, 1, 0)
intVisser2 VSw = (DT, DT, DT, DT, -1, 0)
intVisser2 VSc = (DT, DT, DT, DT, 1, 1)
intVisser2 (VP p rs) = (DT, DT, DT, DP p rs, 1, 0)
intVisser2 (VE r) = (DT, DT, DT, DE r, 1, 0)
intVisser2 (VC vis1 vis2) = compVisser2 (intVisser2 vis1) (intVisser2 vis2)
intVisser2 (VQ0 vis) = (qm1, DT, qp1, qm0 `DI` qp0, 1, 0)
    where (qm1, qm0, qp1, qp0, a, i) = intVisser2 vis
intVisser2 (VQ1 vis) = (DT, DT, DT, (qm1 `DC` qm0) `DI` (qp1 `DC` qp0), 1, 0)
    where (qm1, qm0, qp1, qp0, a, i) = intVisser2 vis

compVisser2 :: IVisser2 -> IVisser2 -> IVisser2
compVisser2 (qm1, qm0, qp1, qp0, 1, 0) (rm1, rm0, rp1, rp0, b, j)  = (qm1`DC`rm1, qm0`DC`rm0, qp1`DC`rp1, qp0`DC`rp0, b, j)
compVisser2 (qm1, qm0, qp1, qp0, 1, 1) (rm1, rm0, rp1, rp0, b, j)  = (qm1`DC`rm0, qm0`DC`rm1, qp1`DC`rp0, qp0`DC`rp1, b, 1-j)
compVisser2 (qm1, qm0, qp1, qp0, -1, 0) (rm1, rm0, rp1, rp0, b, j) = (qm1`DC`rp1, qm0`DC`rp0, qp1`DC`rm1, qp0`DC`rm0, -b, j)
compVisser2 (qm1, qm0, qp1, qp0, -1, 1) (rm1, rm0, rp1, rp0, b, j) = (qm1`DC`rp0, qm0`DC`rp1, qp1`DC`rm0, qp0`DC`rm1, -b, 1-j)



intVisser3 :: Visser -> Dpl
intVisser3 p = (qm1 `DC` qm0) `DI` (qp1 `DC` qp0)
    where (qm1, qm0, qp1, qp0, a, i) = intVisser2 p


-- More possible interpretations include support for disjunction
-- and one directional switch
