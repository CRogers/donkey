
type Val = Integer
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

