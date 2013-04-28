module ToLogic (logify) where

import Dpl (dpl2prop, s2dpl, Type2, sT, sF, sSwitch, sScope, sPredi, sExist, sComp, sTest0, sTest1)
import Prop
import Stanford (Word, Store, Index, PosTree(..))

import Data.List
import Data.Map hiding (lookup, mapMaybe, map)
import Data.Maybe

type IWord = (Word, Index)
type SyntaxB = [PosTree IWord]
type Predicate = Ref -> Type2
type Predicate2 = Ref -> Ref -> Type2

logify :: (Store, [PosTree IWord]) -> Prop
logify (refs, tree) = (simplify . dpl2prop . s2dpl) logic
	where logic = intRoot tree refs

intRoot :: SyntaxB -> Store -> Type2
intRoot ((P "S" s):ss) r = intS s r `sComp` intRoot ss r
intRoot ((P "NP" np):ss) r = intNP np r (\x -> sT) `sComp` intRoot ss r
intRoot [] r = sT

intS :: SyntaxB -> Store -> Type2
--intS [P "NP" [L "EX" _], P "VP" [L "VB" _, P "NP" np]] r = intNP np r (intVP vp r)
intS [P "NP" np, P "VP" vp] r = intNP np r (intVP vp r)
intS (P "SBAR" ([(L "IN" ("if",_)), (P "S" s1)]) : (L "," _) : s2) r =
		sSwitch `sComp` intS s1 r `sComp` sSwitch `sComp` intS s2 r
--intS [P "CC" [P "S" s1, P "S" s2]] r = intS s1 r `sComp` intS s2 r
intS [P "S" s1, L "CC" _, P "S" s2] r = intS s1 r `sComp` intS s2 r
intS other r = error ("Unsuported by intS: " ++ show other)

intNP :: SyntaxB -> Store -> Predicate -> Type2
-- intNP ([L "NNP" (nnp,_)], i) r vb = sPredi nnp [v] `sComp`
--intNP ([L "NN" (nnp,_)], i) r vb = sPredi nnp [v] `sComp`
intNP [L "PRP" (prp,i)] r vb = vb (r i)
intNP [L "DT" dt, L "NN" nn] r vb = intDT dt r (intNN nn r) vb
intNP [L "DT" dt, L "NN" nn1, L "CC" _, L "NN" nn2] r vb = intDT dt r (\x -> intNN nn1 r x `sComp` intNN nn2 r x) vb
-- In 'a farmer and a donkey visit a friend' this makes the friend differ in the two cases
--intNP [P "NP" np1, L "CC" _, P "NP" np2] r = \vb -> intNP np1 r vb `sComp` intNP np2 r vb
intNP [P "NP" np1, L "CC" _, P "NP" np2] r vb = intNP np1 r (\x -> intNP np2 r (inner x))
	where
		inner = \x y -> sTest0 (sSwitch `sComp` sExist z `sComp` sPredi "eq-on-of" [z,x,y] `sComp` sSwitch `sComp` vb z)
		z = r (4,0) -- just some free variable

intNP other r vb = error ("Unsuported by intNP: " ++ show other)

-- Adjectives:
-- (NP (JJ big) (NN sausage))
-- What about adjective phrases? ADJP
-- (ADJP (RB very) (JJ big))

-- Also:
-- ADVP - Adverb Phrase.
-- CONJP - Conjunction Phrase
-- PP - Prepositional Phrase.
-- Different WH phrases

intVP :: SyntaxB -> Store -> Predicate
intVP [L "VB" ("be",_), what] r = \v -> sPredi (stringify [what]) [v]
intVP [L "VB" vb] r = intVB vb r
intVP [L "VB" vb, P "NP" np] r = \v -> intNP np r (intVB2 vb r v)
intVP [P "VP" vp1, L "CC" _, P "VP" vp2] r = \v -> intVP vp1 r v `sComp` intVP vp2 r v
-- In 'a farmer starves and beats a donkey' this makes the donkeys differ
intVP [L "VB" vb1, L "CC" _, L "VB" vb2, P "NP" np] r = \v1 -> (intNP np r (\v2 -> (intVB2 vb1 r v1 v2) `sComp` (intVB2 vb2 r v1 v2)))
intVP n@[L "VB" vb, P "PP" pp] r = intVB (stringify n,(0,0)) r
intVP [L "VB" (vb,i), P "NP" np, P "NP-TMP" nptmp] r = intVP [L "VB" (vb++"-"++stringify nptmp,i), P "NP" np] r
intVP other r = error ("Unsuported by intVP: " ++ show other)
--intVP ([L "VB" vb, P "NP" np, L "," _, P "SBAR" [L "IN" "unless", P "S" s]], i) r = ...

-- Stanford parser doesn't seperate transitive verbs, so our type has to accept any number of arguments
-- Hvis de to n;ste ikke bliver mere komplicerede kan vi ogs[ bare proppe dem ind i VP ovenfor.
intVB :: IWord -> Store -> Predicate
intVB (vb,_) r = \v -> sPredi vb [v]
intVB2 :: IWord -> Store -> Predicate2
intVB2 (vb,_) r = \v1 v2 -> sPredi vb [v1, v2]

--intADVP :: SyntaxB -> Store -> Predicate -> Predicate
--intADVP [L "RB" (rb,_)] r vb =

intNN :: IWord -> Store -> Predicate
intNN (nn,_) r = \v -> sPredi nn [v]

intDT :: IWord -> Store -> Predicate -> Predicate -> Type2
intDT ("some", i) r = \p q -> sExist m `sComp` p m `sComp` q m where m = r i
intDT ("a", i) r = intDT ("some", i) r
intDT ("all", i) r = intDT ("every", i) r
intDT ("every", i) r = \p q -> sSwitch `sComp` sExist m `sComp` p m `sComp` sSwitch `sComp` q m where m = r i
intDT ("no", i) r = \p q -> sSwitch `sComp` sExist m `sComp` p m `sComp` q m `sComp` sSwitch `sComp` sF where m = r i
intDT other r = error ("Unsuported by intDT: " ++ show other)
-- I don't like this one a lot
--intDT ("the",i) p q = (unique m (p m)) `conj` intDT "some" p q
--	where m = i2m i

flatten :: PosTree a -> [a]
flatten (P _ xs) = xs >>= flatten
flatten (L _ x) = [x]

stringify :: [PosTree IWord] -> String
stringify ts = intercalate "-" words
	where words = map fst (ts >>= flatten)
