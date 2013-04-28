

import Dpl (DPL(..))
import Data.Maybe
import Control.Monad ((>=>))

type Val = Integer
type Ref = String
type Ass = [(Ref, Val)]
type Rel = Ass -> [Ass]
type Pre = [[Val]]

-- Rel a = [b in ASS | Ez in a (z R b)]
-- tanke: den initielle assignment er til de fri variable.

sigma :: [Ref]
sigma = ["x","y"]

domain :: [Val]
domain = [0,1,2]

predicates :: [(Ref, Pre)]
predicates = [
    ("farmer", [[0]]),
    ("donkey", [[1]]),
    ("owns", [[0,1]]),
    ("beats", [[0,1]])
    ]

everything :: [Ass]
everything = sequence [[(k,v) | v <- domain] | k <- sigma]

sem :: DPL -> Rel
sem DT = return
sem DF = const []
sem (DComp r s) = sem r >=> sem s
sem (DNot r) = \a -> if sem r a == [] then [a] else []
sem (DImpl r s) = \a -> if all (\z -> sem s z /= []) (sem r a) then [a] else []
sem (DExists k) = \a -> [set k v a | v <- domain]
sem (DPredi f ks) = \a -> if elem (map (get a) ks) (get predicates f) then [a] else []

true :: Rel
true = return

false :: Rel
false = const []

-- like and
comp :: Rel -> Rel -> Rel
comp = (>=>)

test :: Rel -> Rel
test r a = if r a == [] then [] else [[]]
-- test = (rnot . rnot)
-- test = impl true

-- notice 'not' cannot bring anybody back 'from the dead'
rnot :: Rel -> Rel
rnot r a = if r a == [] then [a] else []

impl :: Rel -> Rel -> Rel
-- impl r s = rnot (r `comp` (rnot s))
impl r s a = if all (\z -> s z /= []) (r a) then [a] else []

exist :: Ref -> Rel
exist k a = [set k v a | v <- domain]

predi1 :: [Val] -> Ref -> Rel
predi1 f k a = if elem (get a k) f then [a] else []

predi2 :: [(Val,Val)] -> Ref -> Ref -> Rel
predi2 f k l a = if elem (get a k, get a l) f then [a] else []

-- like or
union :: Rel -> Rel -> Rel
union s r a = s a ++ r a

forall :: Ref -> Rel -> Rel
forall k r = exist k `impl` r
--forall k r = rnot (exist k (rnot r))

-- Will fail if ref not in ass
get :: Eq a => [(a, b)] -> a -> b
get a k = fromJust (lookup k a)



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

r2 = ((exist "x" >=> farmer "x") `union` (exist "x" >=> donkey "x")) >=> beats "x" "x"

r3 = exist "x" >=> (farmer "x" `union` (exist "y" >=> donkey "y")) -- >=> beats "x" "y"


--[(v,k) | v <- vals, k <- ["x","y","z"]]
-- test r []
