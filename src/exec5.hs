

import Data.Maybe
import Control.Monad ((>=>), liftM, liftM2)

type Val = Integer
type Ref = String
type Ass = [(Ref, Val)]
type Rel = Maybe Ass -> Maybe [Ass]

-- Rel a = [b in ASS | Ez in a (z R b)]
-- tanke: den initielle assignment er til de fri variable.

--ma (|>)

sigma :: [Ref]
sigma = ["x","y"]

domain :: [Val]
domain = [0,1,2]

everything :: [Ass]
everything = sequence [[(k,v) | v <- domain] | k <- sigma]

unlift :: (Maybe a -> Maybe b) -> a -> b
unlift f x = fromJust (f (Just x))

true :: Rel
true = liftM return

false :: Rel
false = liftM (const [])

panic :: Rel
panic = const Nothing

-- like and
comp :: Rel -> Rel -> Rel
comp r s ma = r ma >>= (\as -> liftM concat $ sequence $ map (s . return) as)

--test :: Rel -> Rel
--test r a = if r a == [] then [] else [[]]
-- test = (rnot . rnot)
-- test = impl true

-- hm, this will make not (error) = [], should be error
rnot :: Rel -> Rel
rnot r = liftM (\a -> if r a == [] then [a] else [])

impl :: Rel -> Rel -> Rel
-- impl r s = rnot (r `comp` (rnot s))
impl r s = liftM (\a -> if all (\z -> s z /= []) (r a) then [a] else [])

exist :: Ref -> Rel
exist k a = [set k v a | v <- domain]

predi1 :: [Val] -> Ref -> Rel
predi1 f k = liftM (\a -> if elem (get k a) f then [a] else [])

predi2 :: [(Val,Val)] -> Ref -> Ref -> Rel
predi2 f k l = liftM (\a -> if elem (get k a, get l a) f then [a] else [])

-- Will fail if ref not in ass
get :: Ref -> Ass -> Val
get k = fromJust . (lookup k)

set :: Ref -> Val -> Ass -> Ass
set k v a = (k,v) : filter ((/=k).fst) a




farmer = predi1 [0]
donkey = predi1 [1]
horse = predi1 [2]
beats = predi2 [(0,1)]
owns = predi2 [(0,1)]

-- If a farmer owns a donkey, he beats it.
-- This doesn't work because E doesn't bind over -> it does
-- Ex.farmer(x).Ey.donkey(y) -> beats(x,y)


r1 = ((exist "x") `comp` (farmer "x") `comp` (exist "y") `comp` (donkey "y")) `impl` (beats "x" "y")

--r2 = ((exist "x" >=> farmer "x") `union` (exist "x" >=> donkey "x")) >=> beats "x" "x"

--r3 = exist "x" >=> (farmer "x" `union` (exist "y" >=> donkey "y")) -- >=> beats "x" "y"


--[(v,k) | v <- vals, k <- ["x","y","z"]]
-- test r []
