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
simplify (PAnd p PT) = simplify p
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
