

import Data.Maybe
import Data.Either
import Control.Monad ((>=>), liftM, liftM2)

type Val = Integer
type Ref = String
type Ass = [(Ref, Val)]
type Rel = Ass -> [Ass]

-- We don't have to used indexed sets and disjoint union, since we don't eliminate duplicates
type DRel = [Maybe Rel]

-- Rel a = [b in ASS | Ez in a (z R b)]
-- tanke: den initielle assignment er til de fri variable.

--

sigma :: [Ref]
sigma = ["x","y"]

domain :: [Val]
domain = [0,1,2]

everything :: [Ass]
everything = sequence [[(k,v) | v <- domain] | k <- sigma]


true :: DRel
true = [Just return]

false :: DRel
false = [Just (const [])]

err :: DRel
err = [Nothing]


-- like and
comp :: DRel -> DRel -> DRel
comp rs ss = [liftM2 (>=>) r s | r <- rs, s <- ss]

test :: DRel -> Ass -> [Maybe [Ass]]
test rs a = map (liftM ($a)) rs

--test :: Rel -> Ass -> Rel
--test r a = if r a == [] then [] else [[]]
-- test = (rnot . rnot)
-- test = impl true

-- notice 'not' cannot bring anybody back 'from the dead'
rnot :: Rel -> Rel
rnot r a = if r a == [] then [a] else []

-- just use 'all' and 'test'. Doh
dnot :: DRel -> DRel
dnot rs = [foldl (liftM2 (\a b -> a >=> (rnot b))) (Just return) rs]

impl :: DRel -> DRel -> DRel
impl rs ss = dnot (rs `comp` (dnot ss))
--impl r s a = if all (\z -> s z /= []) (r a) then [a] else []

exist :: Ref -> DRel
exist k = [Just (\a -> [set k v a | v <- domain])]

-- Problem: Vi ved f'rst ved runtime om vi har en error eller ej
predi1 :: [Val] -> Ref -> DRel
predi1 f k = [(\a -> get k a >>= (\e -> if elem e f then [a] else []))]

predi2 :: [(Val,Val)] -> Ref -> Ref -> DRel
predi2 f k l = [Just (\a -> if elem (get k a, get l a) f then [a] else [])]

-- like or
disj :: DRel -> DRel -> DRel
disj = (++)

forall :: Ref -> DRel -> DRel
forall k rs = exist k `impl` rs

-- Will fail if ref not in ass
get :: Ref -> Ass -> Val
get k = fromJust . (lookup k)

set :: Ref -> Val -> Ass -> Ass
set k v a = (k,v) : filter ((/=k).fst) a




farmer = predi1 [0]
donkey = predi1 [1]
horse = predi1 [2]
beats = predi2 [(0,1)]

-- If a farmer owns a donkey, he beats it.
-- This doesn't work because E doesn't bind over -> it does
-- Ex.farmer(x).Ey.donkey(y) -> beats(x,y)


r1 = ((exist "x") `comp` (farmer "x") `comp` (exist "y") `comp` (donkey "y")) `impl` (beats "x" "y")

r2 = ((exist "x" `comp` farmer "x") `disj` (exist "x" `comp` donkey "x")) `comp` beats "x" "x"

r3 = exist "x" `comp` farmer "x" `comp` ((exist "y" `comp` donkey "y") `disj` false) `comp` beats "x" "y"

r4 = exist "x" `comp` farmer "x" `comp` ((exist "y" `comp` donkey "y") `disj` true) `comp` beats "x" "y"

--[(v,k) | v <- vals, k <- ["x","y","z"]]
-- test r []
