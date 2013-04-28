
-- These direct semantics don't work, since they require quantification over all assignments

import Data.List
import Data.Maybe

type Val = Integer
type Ref = String
type Ass = [(Ref, Val)]
type Rel = Ass -> Ass -> Bool

sigma = ["x","y"]
domain = [0,1,2]
everything = sequence [[(k,v) | v <- domain] | k <- sigma]

true :: Rel
true x y = x == y

false :: Rel
false x y = False

comp :: Rel -> Rel -> Rel
comp r s x y = or [x `r` z && z `s` y | z <- everything]

rnot :: Rel -> Rel
rnot r x y = x == y && not (r x y)

impl :: Rel -> Rel -> Rel
impl s r = rnot (s `comp` (rnot r))

exist :: Ref -> Rel
exist key x y = and [lookup k x == lookup k y | k <- sigma \\ [key]]

predi1 :: [Val] -> Ref -> Rel
predi1 f k x y = x == y && elem (get k x) f

predi2 :: [(Val,Val)] -> Ref -> Ref -> Rel
predi2 f k l x y = x == y && elem ((get k x), (get l x)) f

-- Will fail if ref not in ass
get :: Ref -> Ass -> Val
get k = fromJust . (lookup k)

set :: (Ref, Val) -> Ass -> Ass
set (k,v) as = (k,v) : filter ((/=k).fst) as

farmer = [0]
donkey = [1]
beats = [(0,1)]

-- If a farmer owns a donkey, he beats it.
-- This doesn't work because E doesn't bind over -> it does
-- Ex.farmer(x).Ey.donkey(y) -> beats(x,y)


r = ((exist "x") `comp` (predi1 farmer "x") `comp` (exist "y") `comp` (predi1 donkey "y")) `impl` (predi2 beats "x" "y")

--[(v,k) | v <- vals, k <- ["x","y","z"]]
-- test r []
