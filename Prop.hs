module Dpl where

type Ref = String

data Prop =
	  T
	| F
	| Not   Prop
    | And   Prop Prop
    | Predi Ref [Ref]
    | Exist Ref Prop
    | All Ref Prop
    | Impl Prop Prop
    deriving (Eq, Show)

simplify :: Prop -> Prop
simplify (Not (Exist k p)) = simplify (All k (Not p))
simplify (Not (All k p)) = simplify (Exist k (Not p))
simplify (Not (And p q)) = simplify (Impl p (Not q))
simplify (Not (Not p)) = simplify p
simplify (Not T) = F
simplify (Not F) = T

simplify (Impl p F) = simplify (Not p)
simplify (Impl p (Impl q r)) = simplify (Impl (And p q) r)

simplify (And p T) = simplify p
simplify (And p F) = F

simplify (And p q) = And (simplify p) (simplify q)
simplify (All k p) = All k (simplify p)
simplify (Exist k p) = Exist k (simplify p)
simplify (Not p) = Not (simplify p)
simplify (Impl p q) = Impl (simplify p) (simplify q)

simplify p = p

fexp f e = (iterate (f.) id) !! e
