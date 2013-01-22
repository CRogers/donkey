import Data.List

type Var = Integer
type Rels = Var -> Var -> Bool
type M = [Var] -> Rels

-- monaden skal have constanter, værdier, variable, funktioner...

create :: [(Var,Var)] -> Rels
create pairs x y = elem (x,y) pairs

result :: Rels -> M
result r vs = r

new = result . create

-- ($>) :: M -> (Rels -> M) -> M
-- (r $> f) vs = f (filter r (zip vs' vs')) where vs' = r vs

draComp :: M -> M -> M
draComp r s vs x y = any (\z -> r vs x z && s vs z y) vs

draId :: M
draId vs x y = x == y

draBot :: M
draBot vs x y = False

impl a b = not a || b

draImpl :: M -> M -> M
draImpl r s vs x y = x == y && all (\z -> r vs x z `impl` any (\u -> s vs z u) vs) vs

draNot :: M -> M
draNot r = r `draImpl` draBot

cross :: [a] -> [b] -> [(a, b)]
cross [] ys = []
cross (x:xs) ys = [(x, y) | y <- ys] ++ cross xs ys

draDiag :: [Var] -> M
draDiag xs vs x y = x == y && elem x xs

draShow :: M -> [Var] -> [(Var,Var)]
draShow r vs = filter (uncurry (r vs)) (cross vs vs)

draDom :: M -> [Var] -> [Var]
draDom r vs = map fst (draShow r vs)

test :: [(Var,Var)] -> [Var] -> Bool
test pairs vs =
    draShow (draNot r) vs == draShow (draDiag (vs \\ draDom r vs)) vs
    && draShow ((draNot.draNot) r) vs == draShow (draId `draImpl` r) vs
    && draShow (draId `draImpl` r) vs == draShow (draDiag (draDom r vs)) vs
    where r = (result . create) pairs

testAll :: [Var] -> Bool
testAll vs = and [test pairs vs | pairs <- subsequences (cross vs vs)]

-- Der må være forskel på værdier og variable
-- exists skal tage en variabel, og assigne den til alle forskellige værdier
-- indtil det, den bliver kædet med, virker.
-- Så faktisk skal der være noget effekt at et element, nemlig at det kan ændre
-- assigmenten på variablerne
-- Kræver måske renaming...
exists 