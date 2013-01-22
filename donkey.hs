-- http://stackoverflow.com/questions/3228856/predicate-logic-in-haskell

import Data.List

data Prop =
	  Not   Prop
    | And   Prop Prop
    | Or    Prop Prop
    | Impl  Prop Prop
    | Equiv Prop Prop
    | Equals Obj Obj
    | ForAll (Obj -> Prop)
    | Exists (Obj -> Prop)
    deriving (Eq, Ord)

data Dra =
	  DraComp Dra Dra
	| DraId
	| DraBottom
	| DraImpl Dra Dra
	| Set

class Pol p where
	plus :: 

data Pol = 1 | -1
type Dlp = (Prop, Prop, Pol)

true :: Dlp
true = (true, true, 1)

false :: Dlp
false = (true, false, 1)

switch :: Dlp
switch = (true, true, -1)

mul :: Dlp -> Dlp -> Dlp
mul (qm, qp, 1) (rm, rp, b) = (pmul qm rm, pmul qp rp, b)
mul (qm, qp, -1) (rm, rp, b) = (pmul qm rp, pmul qp rm, -b)

test :: Dlp -> Prop
test (qm, qp, a) = Impl qm qp
