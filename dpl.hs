import Data.List

type Val = Integer
type Ref = String
type Ass = [(Ref, Val)]
-- type Rel = [Ass] -> [Ass] -> Bool
type Rel = [Ass] -> [Ass]
--type M = [Ref] -> Rels


true :: Rel
true = id

false :: Rel
false ass = []

-- like and
comp :: Rel -> Rel -> Rel
comp = flip (.)

test :: Rel -> Ass -> Bool
test s ass = s [ass] /= []

rnot :: Rel -> Rel
rnot s = filter (\as -> not (test s as))

impl :: Rel -> Rel -> Rel
impl s r = rnot (s `comp` (rnot r))

exist :: Ref -> [Val] -> Rel
exist k vs ass = [a : as | a <- news, as <- olds]
	where olds = map (filter ((/=k) . fst)) ass
	      news = [(k,v) | v <- vs]

predi :: (Ass -> Bool) -> Rel
predi f ass = filter f ass

-- like or
union :: Rel -> Rel -> Rel
union s r ass = (s ass) ++ (r ass)


-- Will fail if ref not in ass
get :: Ass -> Ref -> Val
get ((k, v):as) q
	| k == q    = v
	| otherwise = get as q

vals = [0,1,2]
farmer k as = get as k == 0
donkey k as = get as k == 1
beats k q as = get as k == 0 && get as q == 1

-- If a farmer owns a donkey, he beats it.
-- This doesn't work because E doesn't bind over -> it does
-- Ex.farmer(x).Ey.donkey(y) -> beats(x,y)


r = ((exist "x" vals) `comp` (predi (farmer "x")) `comp` (exist "y" vals) `comp` (predi (donkey "y"))) `impl` (predi (beats "x" "y"))

data Prop =
	    PT | PF
		| PNot   Prop
    | PAnd   Prop Prop
    | PPredi Ref [Ref]
    | PExist Ref Prop
    | PAll Ref Prop
    | PImpl Prop Prop
    deriving (Eq, Show)

data Dpl =
	    DT | DF
	  | DPredi Ref [Ref]
	  | DExist Ref
	  | DComp Dpl Dpl
	  | DNot Dpl
	  | DImpl Dpl Dpl
	  deriving (Eq, Show)

dpl2prop :: Prop -> [Dpl] -> Prop
dpl2prop p [] = p
dpl2prop p (DF:ds) = PF
dpl2prop p (DT:ds) = dpl2prop p ds
dpl2prop p ((DPredi f ks):ds) = dpl2prop (PAnd (PPredi f ks) p) ds
dpl2prop p ((DExist k):ds) = dpl2prop (PExist k p) ds
dpl2prop p ((DComp d1 d2):ds) = dpl2prop p (d2:d1:ds)
dpl2prop p ((DNot d):ds) = dpl2prop (PAnd (PNot (dpl2prop PT [d])) p) ds
dpl2prop p ((DImpl d1 d2):ds) = dpl2prop p ((DNot (DComp d1 (DNot d2))):ds)

simplify :: Prop -> Prop
simplify (PNot (PExist k p)) = simplify (PAll k (PNot p))
simplify (PNot (PAll k p)) = simplify (PExist k (PNot p))
simplify (PNot (PAnd p (PNot q))) = simplify (PImpl p q)
--simplify (PNot PT) = PF
--simplify (PNot PF) = PT
simplify (PAnd p PT) = simplify p
--simplify (PAnd p PF) = PF
simplify (PNot (PNot p)) = simplify p

simplify (PAnd p q) = PAnd (simplify p) (simplify q)
simplify (PAll k p) = PAll k (simplify p)
simplify (PExist k p) = PExist k (simplify p)
simplify (PNot p) = PNot (simplify p)

simplify p = p


r1 = DImpl (DComp (DComp (DComp (DExist "x") (DPredi "farmer" ["x"])) (DExist "y")) (DPredi "donkey" ["y"])) (DPredi "beats" ["x", "y"])

type Pol = (Dpl, Dpl, Integer)
pT = (DT, DT, 1)
pF = (DT, DF, 1)
pSwitch = (DT, DT, -1)
pPredi :: Ref -> [Ref] -> Pol
pPredi f ks = (DT, DPredi f ks, 1)
pExist :: Ref -> Pol
pExist k = (DT, DExist k, 1)
pComp :: Pol -> Pol -> Pol
pComp (qm, qp, 1) (rm, rp, b) = (DComp qm rm, DComp qp rp, b)
pComp (qm, qp, -1) (rm, rp, b) = (DComp qm rp, DComp qp rm, -b)
pTest :: Pol -> Pol
pTest (qm, qp, a) = (DT, DImpl qm qp, 1)
p2dpl :: Pol -> Dpl
p2dpl (qm, qp, a) = DImpl qm qp

-- Axy ((donkey(y) && beats(x,y)) -> farmer(x))
r2 = pSwitch `pComp` pExist "x" `pComp` pSwitch `pComp` pPredi "farmer" ["x"] `pComp` pSwitch `pComp` pExist "y" `pComp` pPredi "donkey" ["y"] `pComp` pPredi "beats" ["x", "y"]

type Scop = (Dpl, Dpl, Dpl, Dpl, Integer, Integer)
sT = (DT, DT, DT, DT, 1, 0)
sF = (DT, DT, DT, DF, 1, 0)
sSwitch = (DT, DT, DT, DT, -1, 0)
sScope = (DT, DT, DT, DT, 1, 1)
sPredi :: Ref -> [Ref] -> Scop
sPredi f ks = (DT, DT, DT, DPredi f ks, 1, 0)
sExist :: Ref -> Scop
sExist k = (DT, DT, DT, DExist k, 1, 0)
sComp :: Scop -> Scop -> Scop
sComp (qm1, qm0, qp1, qp0, 1, 0) (rm1, rm0, rp1, rp0, b, j) =  (DComp qm1 rm1, DComp qm0 rm0, DComp qp1 rp1, DComp qp0 rp0, b, j)
sComp (qm1, qm0, qp1, qp0, 1, 1) (rm1, rm0, rp1, rp0, b, j) =  (DComp qm1 rm0, DComp qm0 rm1, DComp qp1 rp0, DComp qp0 rp1, b, 1-j)
sComp (qm1, qm0, qp1, qp0, -1, 0) (rm1, rm0, rp1, rp0, b, j) = (DComp qm1 rp1, DComp qm0 rp0, DComp qp1 rm1, DComp qp0 rm0, -b, j)
sComp (qm1, qm0, qp1, qp0, -1, 1) (rm1, rm0, rp1, rp0, b, j) = (DComp qm1 rp0, DComp qm0 rp1, DComp qp1 rm0, DComp qp0 rm1, -b, 1-j)
sTest0 :: Scop -> Scop
sTest0 (qm1, qm0, qp1, qp0, a, i) = (qm1, DT, qp1, DImpl qm0 qp0, 1, 0)
sTest1 :: Scop -> Scop
sTest1 (qm1, qm0, qp1, qp0, a, i) = (DT, DT, DT, DImpl (DComp qm1 qm0) (DComp qp1 qp0), 1, 0)
s2dpl :: Scop -> Dpl
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
