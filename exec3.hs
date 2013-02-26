

import Data.List
import Data.Maybe

type Val = Integer
type Ref = String
type Ass = [(Ref, Val)]
type Rel = [Ass] -> [Ass]

-- "The correct way to go is undoubtly to replace relations by indexed sets of relations read disjunctively"

sigma = ["x","y"]
domain = [0,1,2]
everything = sequence [[(k,v) | v <- domain] | k <- sigma]

true :: [Rel]
true = [id]

false :: [Rel]
false = [const []]

-- like and, conjunction, cross product
-- (a -> b eller c -> d) og (d -> e eller f -> g)
comp :: [Rel] -> [Rel] -> [Rel]
comp rs ss = [s.r | r <- rs, s <- ss]

disj :: [Rel] -> [Rel] -> [Rel]
disj = (++)

test :: [Rel] -> Ass -> Bool
test rs as = any (not.null) [r [as] | r <- rs]

rtest :: Rel -> Ass -> Bool
rtest r as = (not.null) (r [as])

-- not in dpl works by filtering out exactly those assignments that would before have been accepted
-- (not rel) ass = ass - (rel ass)
rnot :: Rel -> Rel
rnot r = filter (\as -> not (rtest r as))

-- not (a -> b eller c -> d)
-- we must push the not through the disjunction with de-morgan
dnot :: [Rel] -> [Rel]
dnot rs = foldl comp true [[rnot r] | r <- rs]

impl :: [Rel] -> [Rel] -> [Rel]
impl rs ss = dnot (rs `comp` (dnot ss))
--impl rs ss = (dnot rs) `disj` ss
-- This bottom one somehow doesn't allow left E's to be used in right. Hm
-- So di morgan er ikke rigtig paa plads

exist :: Ref -> [Rel]
exist k = [\ass -> [set (k,v) as | v <- domain, as <- ass]]

predi1 :: [Val] -> Ref -> [Rel]
predi1 f k = [filter (\as -> elem (get k as) f)]

predi2 :: [(Val,Val)] -> Ref -> Ref -> [Rel]
predi2 f k l = [filter (\as -> elem (get k as, get l as) f)]

-- Will fail if ref not in ass
--runp1 f k as = elem (get k as) (map Just f)

get :: Ref -> Ass -> Val
get k = fromJust . (lookup k)

set :: (Ref, Val) -> Ass -> Ass
set (k,v) as = (k,v) : filter ((/=k).fst) as
--set (k,v) as = (k,v) : [a | a <- as, fst a /= k]

farmer = [0]
donkey = [1]
beats = [(0,1)]

-- If a farmer owns a donkey, he beats it.
-- This doesn't work because E doesn't bind over -> it does
-- Ex.farmer(x).Ey.donkey(y) -> beats(x,y)


r = ((exist "x") `comp` (predi1 farmer "x") `comp` (exist "y") `comp` (predi1 donkey "y")) `impl` (predi2 beats "x" "y")

--[(v,k) | v <- vals, k <- ["x","y","z"]]
-- test r []
