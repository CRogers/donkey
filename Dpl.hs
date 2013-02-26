module Dpl (dpl2prop, Type2, s2dpl,
	sT, sF, sSwitch, sScope, sPredi, sExist, sComp, sTest0, sTest1) where

import Prop (Ref, Prop(..))

-- Type 1: Basic dynamic predicate logic
----------------------------------------

data DPL =
	    DT | DF
	  | DPredi Ref [Ref]
	  | DExist Ref
	  | DComp DPL DPL
	  | DNot DPL
	  | DImpl DPL DPL
	  deriving (Eq, Show)

-- TODO: Move exec code here

dpl2prop :: DPL -> Prop
dpl2prop dpl = addDpl T [dpl]

addDpl :: Prop -> [DPL] -> Prop
addDpl p [] = p
addDpl p (DF:ds) = F
addDpl p (DT:ds) = addDpl p ds
addDpl p ((DPredi f ks):ds) = addDpl (And (Predi f ks) p) ds
addDpl p ((DExist k):ds) = addDpl (Exist k p) ds
addDpl p ((DComp d1 d2):ds) = addDpl p (d2:d1:ds)
addDpl p ((DNot d):ds) = addDpl (And (Not (addDpl T [d])) p) ds
addDpl p ((DImpl d1 d2):ds) = addDpl p ((DNot (DComp d1 (DNot d2))):ds)

-- If there's a farmer and a donkey, the farmer beats the donkey
r1 = DImpl (DComp (DComp (DComp (DExist "x") (DPredi "farmer" ["x"])) (DExist "y")) (DPredi "donkey" ["y"])) (DPredi "beats" ["x", "y"])



-- Type 1: Logic with polarity switcher
---------------------------------------

type Type1 = (DPL, DPL, Integer)

-- there is a weird syntax / semnatics thing going on here
-- We should implement a much more normal syntax + interpretaion halloj

pT = (DT, DT, 1)
pF = (DT, DF, 1)
pSwitch = (DT, DT, -1)
pPredi :: Ref -> [Ref] -> Type1
pPredi f ks = (DT, DPredi f ks, 1)
pExist :: Ref -> Type1
pExist k = (DT, DExist k, 1)
pComp :: Type1 -> Type1 -> Type1
pComp (qm, qp, 1) (rm, rp, b) = (DComp qm rm, DComp qp rp, b)
pComp (qm, qp, -1) (rm, rp, b) = (DComp qm rp, DComp qp rm, -b)
pTest :: Type1 -> Type1
pTest (qm, qp, a) = (DT, DImpl qm qp, 1)
p2dpl :: Type1 -> DPL
p2dpl (qm, qp, a) = DImpl qm qp

-- Axy ((donkey(y) && beats(x,y)) -> farmer(x))
r2 = pSwitch `pComp` pExist "x" `pComp` pSwitch `pComp` pPredi "farmer" ["x"] `pComp` pSwitch `pComp` pExist "y" `pComp` pPredi "donkey" ["y"] `pComp` pPredi "beats" ["x", "y"]



-- Type 2: Logic with polarity and scope switcher
-------------------------------------------------
-- sT, sF, sSwitch, sScope, sPredi, sExist, sComp, sTest0, sTest1
type Type2 = (DPL, DPL, DPL, DPL, Integer, Integer)
sT = (DT, DT, DT, DT, 1, 0)
sF = (DT, DT, DT, DF, 1, 0)
sSwitch = (DT, DT, DT, DT, -1, 0)
sScope = (DT, DT, DT, DT, 1, 1)
sPredi :: Ref -> [Ref] -> Type2
sPredi f ks = (DT, DT, DT, DPredi f ks, 1, 0)
sExist :: Ref -> Type2
sExist k = (DT, DT, DT, DExist k, 1, 0)
sComp :: Type2 -> Type2 -> Type2
sComp (qm1, qm0, qp1, qp0, 1, 0) (rm1, rm0, rp1, rp0, b, j) =  (DComp qm1 rm1, DComp qm0 rm0, DComp qp1 rp1, DComp qp0 rp0, b, j)
sComp (qm1, qm0, qp1, qp0, 1, 1) (rm1, rm0, rp1, rp0, b, j) =  (DComp qm1 rm0, DComp qm0 rm1, DComp qp1 rp0, DComp qp0 rp1, b, 1-j)
sComp (qm1, qm0, qp1, qp0, -1, 0) (rm1, rm0, rp1, rp0, b, j) = (DComp qm1 rp1, DComp qm0 rp0, DComp qp1 rm1, DComp qp0 rm0, -b, j)
sComp (qm1, qm0, qp1, qp0, -1, 1) (rm1, rm0, rp1, rp0, b, j) = (DComp qm1 rp0, DComp qm0 rp1, DComp qp1 rm0, DComp qp0 rm1, -b, 1-j)
sTest0 :: Type2 -> Type2
sTest0 (qm1, qm0, qp1, qp0, a, i) = (qm1, DT, qp1, DImpl qm0 qp0, 1, 0)
sTest1 :: Type2 -> Type2
sTest1 (qm1, qm0, qp1, qp0, a, i) = (DT, DT, DT, DImpl (DComp qm1 qm0) (DComp qp1 qp0), 1, 0)
s2dpl :: Type2 -> DPL
s2dpl (qm1, qm0, qp1, qp0, a, i) = DImpl (DComp qm1 qm0) (DComp qp1 qp0)

-- A farmer owns a donkey, he beats it
-- Ex (farmer(x) && Ey (donkey(y) && owns(x,y) && beats(x,y)))
r3 = sScope `sComp` sExist "x" `sComp` sPredi "farmer" ["x"] `sComp` sScope `sComp` sPredi "owns" ["x", "y"] `sComp` sScope `sComp` sExist "y" `sComp` sPredi "donkey" ["y"] `sComp` sScope `sComp` sPredi "beats" ["x", "y"]

-- Only if a FARMER owns a donkey, does he beat it
-- some error here!
r4 = sTest0 (sSwitch `sComp` sScope `sComp` sExist "x" `sComp` sSwitch `sComp` sPredi "farmer" ["x"] `sComp` sSwitch `sComp` sScope `sComp` sPredi "owns" ["x", "y"] `sComp` sScope `sComp` sExist "y" `sComp` sPredi "donkey" ["y"] `sComp` sScope `sComp` sPredi "beats" ["x", "y"])

-- A farmer beats a donkey, if he owns it
-- Ax (farmer(x) -> Ay((donkey(y) && owns(x,y)) -> beats(x,y)))
-- Ex (farmer(x) && Ay((donkey(y) && owns(x,y)) -> beats(x,y)))
r5 = sTest0 (sScope `sComp` sSwitch `sComp` sExist "x" `sComp` sPredi "farmer" ["x"] `sComp` sSwitch `sComp` sScope `sComp` sPredi "beats" ["x", "y"] `sComp` sScope `sComp` sSwitch `sComp` sExist "y" `sComp` sPredi "donkey" ["y"] `sComp` sSwitch `sComp` sScope `sComp` sSwitch `sComp` sPredi "owns" ["x", "y"])
r6 = sTest0 (sScope `sComp` sExist "x" `sComp` sPredi "farmer" ["x"] `sComp` sScope `sComp` sPredi "beats" ["x", "y"] `sComp` sScope `sComp` sExist "y" `sComp` sPredi "donkey" ["y"] `sComp` sScope `sComp` sSwitch `sComp` sPredi "owns" ["x", "y"])



-- Type 3: Logic with polarity and scope switcher and disjunction
-----------------------------------------------------------------

type Type3 = ([DPL], [DPL], [DPL], [DPL], Integer, Integer)

