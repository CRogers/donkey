module ToLogic (logify) where

import Dpl (dpl2prop, s2dpl, Type2, sT, sF, sSwitch, sScope, sPredi, sExist, sComp, sTest0, sTest1)
import Prop
import Stanford (Word, Store, Index, IndexTree(..))

import Data.Map hiding (lookup, mapMaybe)
import Data.Maybe

logify :: (Store, [IndexTree]) -> Prop
logify (refs, tree) = (simplify . dpl2prop . s2dpl) logic
	where logic = intRoot tree refs

type SyntaxB = [IndexTree]
type SyntaxL = (Word, Index)
type Predicate = Ref -> Type2
type Predicate2 = Ref -> Ref -> Type2

intRoot :: [IndexTree] -> Store -> Type2
intRoot ((P "S" s):ss) r = intS s r `sComp` intRoot ss r
intRoot [] r = sT

intS :: SyntaxB -> Store -> Type2
intS [P "NP" np, P "VP" vp] r = intNP np r (intVP vp r)
intS (P "SBAR" ([(L "IN" ("If",_)), (P "S" s1)]) : (L "," _) : s2) r =
		sSwitch `sComp` intS s1 r `sComp` sSwitch `sComp` intS s2 r
intS [P "S" s1, L "CC" ("and",_), P "S" s2] r = intS s1 r `sComp` intS s2 r
intS other r = error ("Unsuported by intS: " ++ show other)

intNP :: SyntaxB -> Store -> Predicate -> Type2
-- intNP ([L "NNP" (nnp,_)], i) r vb = sPredi nnp [v] `sComp`
intNP [L "PRP" (prp,i)] r = \vb -> vb (r i)
intNP [L "DT" dt, L "NN" nn] r = intDT dt r (intNN nn r)
intNP other r = error ("Unsuported by intNP: " ++ show other)

intVP :: SyntaxB -> Store -> Predicate
intVP [L ('V':'B':_) ("is",_), P "NP" np] r = \v -> sPredi (stringify np) [v]
intVP [L ('V':'B':_) vb] r = intVB vb r
intVP [L ('V':'B':_) vb, P "NP" np] r = \v -> intNP np r (intVB2 vb r v)
intVP [P "VP" vp1, L "CC" ("and",_), P "VP" vp2] r = \v -> intVP vp1 r v `sComp` intVP vp2 r v
-- also VP and VP NP for the transitive case
intVP other r = error ("Unsuported by intVP: " ++ show other)
--intVP ([L "VB" vb, P "NP" np, L "," _, P "SBAR" [L "IN" "unless", P "S" s]], i) r = ...

-- Stanford parser doesn't seperate transitive verbs, so our type has to accept any number of arguments
-- Hvis de to n;ste ikke bliver mere komplicerede kan vi ogs[ bare proppe dem ind i VP ovenfor.
intVB :: SyntaxL -> Store -> Predicate
intVB (vb,_) r = \v -> sPredi vb [v]
intVB2 :: SyntaxL -> Store -> Predicate2
intVB2 (vb,_) r = \v1 v2 -> sPredi vb [v1, v2]

intNN :: SyntaxL -> Store -> Predicate
intNN (nn,_) r = \v -> sPredi nn [v]

intDT :: SyntaxL -> Store -> Predicate -> Predicate -> Type2
intDT ("some", i) r = \p q -> sExist m `sComp` p m `sComp` q m where m = r i
intDT ("a", i) r = intDT ("some", i) r
intDT ("every", i) r = \p q -> sSwitch `sComp` sExist m `sComp` p m `sComp` sSwitch `sComp` q m where m = r i
intDT ("no", i) r = \p q -> sSwitch `sComp` sExist m `sComp` p m `sComp` q m `sComp` sSwitch `sComp` sF where m = r i
-- I don't like this one a lot
--intDT ("the",i) p q = (unique m (p m)) `conj` intDT "some" p q
--	where m = i2m i


stringify :: SyntaxB -> String
stringify [] = ""
stringify ((L _ (word,_)):ps) = word ++ "-" ++ stringify ps
stringify ((P _ pss):ps) = stringify (pss ++ ps)
