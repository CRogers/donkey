

import Data.Maybe
import Control.Monad ((>=>))

type Val = Integer
type Ref = String
type Ass = [(Ref, Val)]
type Rel = Maybe Ass -> [Maybe Ass]

-- Rel a = [b in ASS | Ez in a (z R b)]
-- tanke: den initielle assignment er til de fri variable.

sigma :: [Ref]
sigma = ["x","y"]

domain :: [Val]
domain = [0,1,2]

everything :: [Ass]
everything = sequence [[(k,v) | v <- domain] | k <- sigma]


true :: Rel
true = return

false :: Rel
false = const []

panic :: Rel
panic = const [Nothing]

-- like and
comp :: Rel -> Rel -> Rel
comp = (>=>)

test :: Rel -> Ass -> Bool
test r a = case sequence (r (Just a)) of
    Just [] -> False
    Just _ -> True
    Nothing -> undefined
-- test = (rnot . rnot)
-- test = impl true

rnot :: Rel -> Rel
rnot r ma = case sequence (r ma) of
    Just [] -> [Just []]
    Just _ -> []
    Nothing -> [Nothing]

impl :: Rel -> Rel -> Rel
impl r s = rnot (r `comp` (rnot s))
-- impl r s a = if all (\z -> s z /= []) (r a) then [a] else []

exist :: Ref -> Rel
exist k ma = fromMaybe [Nothing] (do
    a <- ma
    return [Just (set k v a) | v <- domain])

predi1 :: [Val] -> Ref -> Rel
predi1 f k ma = fromMaybe [Nothing] (do
    a <- ma
    e <- lookup k a
    if elem e f then return [Just a] else return [])

predi2 :: [(Val,Val)] -> Ref -> Ref -> Rel
predi2 f k1 k2 ma = fromMaybe [Nothing] (do
    a <- ma
    e1 <- lookup k1 a
    e2 <- lookup k2 a
    if elem (e1, e2) f then return [Just a] else return [])

-- Will fail if ref not in ass

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


r1 = ((exist "x") >=> (farmer "x") >=> (exist "y") >=> (donkey "y")) >=> (beats "x" "y")

r2 = beats "x" "y"

--r2 = ((exist "x" >=> farmer "x") `union` (exist "x" >=> donkey "x")) >=> beats "x" "x"

--r3 = exist "x" >=> (farmer "x" `union` (exist "y" >=> donkey "y")) -- >=> beats "x" "y"


--[(v,k) | v <- vals, k <- ["x","y","z"]]
-- test r []
