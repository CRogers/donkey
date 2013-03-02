module Prop (Ref, Prop(..), simplify) where

type Ref = String

data Prop =
	  T
	| F
	| Not   Prop
    | And   Prop Prop
    | Or    Prop Prop
    | Predi Ref [Ref]
    | Exist Ref Prop
    | All Ref Prop
    | Impl Prop Prop
    deriving (Eq, Show)

simp :: Prop -> Prop
simp (Not (Exist k p)) = simp (All k (Not p))
simp (Not (All k p)) = simp (Exist k (Not p))
simp (Not (And p q)) = simp (Impl p (Not q))
simp (Not (Not p)) = simp p
simp (Not T) = F
simp (Not F) = T

simp (Impl p F) = simp (Not p)
simp (Impl p (Impl q r)) = simp (Impl (And p q) r)

simp (And p T) = simp p
simp (And p F) = F

simp (And p q) = And (simp p) (simp q)
simp (All k p) = All k (simp p)
simp (Exist k p) = Exist k (simp p)
simp (Not p) = Not (simp p)
simp (Impl p q) = Impl (simp p) (simp q)

simp p = p

fexp :: (a -> a) -> Int -> (a -> a)
fexp f e = (iterate (f.) id) !! e

simplify :: Prop -> Prop
simplify = fexp simp 10
