module ToLogic (montague) where

import Fol (Ref)
import Visser (Visser(..), cleanVariables)
import Stanford (IWord, Store, PosTree(..))

import Data.List
import Data.Map hiding (lookup, mapMaybe, map)
import Data.Maybe

type Predicate = Ref -> Visser
type Predicate2 = Ref -> Ref -> Visser

montague :: (Store, [PosTree IWord]) -> Visser
montague (refs, tree) = cleanVariables $ intRoot tree refs

intRoot :: [PosTree IWord] -> Store -> Visser
intRoot ((P "S" s):ss) r = VQ0 (intS s r) `VC` intRoot ss r
intRoot ((P "NP" np):ss) r = intNP np r (\x -> VT) `VC` intRoot ss r
intRoot [] r = VT

intS :: [PosTree IWord] -> Store -> Visser
intS [P "NP" np, P "VP" vp] r = intNP np r (intVP vp r)
intS (P "SBAR" ([(L "IN" ("if",_)), (P "S" s1)]) : (L "," _) : s2) r =
		VSw `VC` intS s1 r `VC` VSw `VC` intS s2 r
intS [P "S" s1, L "CC" ("and",_), P "S" s2] r = intS s1 r `VC` intS s2 r
intS other r = error ("Unsuported by intS: " ++ show other)

intNP :: [PosTree IWord] -> Store -> Predicate -> Visser
intNP [L "PRP" (prp,i)] r vb = vb (r i)
intNP [L "DT" dt, L "NN" nn] r vb = intDT dt r (intNN nn r) vb
intNP [L "DT" dt, L "NN" nn1, L "CC" ("and",_), L "NN" nn2] r vb = intDT dt r (\x -> intNN nn1 r x `VC` intNN nn2 r x) vb
intNP [P "NP" np1, L "CC" ("and",_), P "NP" np2] r vb = intNP np1 r (\x -> intNP np2 r (inner x))
	where
		inner = \x y -> VQ0 (VSw `VC` VE (x++y) `VC` VP "eq-on-of" [(x++y),x,y] `VC` VSw `VC` vb (x++y))
		--z = x++y -- just some free variable
intNP other r vb = error ("Unsuported by intNP: " ++ show other)

intVP :: [PosTree IWord] -> Store -> Predicate
intVP [L "VB" ("be",_), what] r = \v -> VP (stringify [what]) [v]
intVP [L "VB" vb] r = intVB vb r
intVP [L "VB" vb, P "NP" np] r = \v -> intNP np r (intVB2 vb r v)
intVP [P "VP" vp1, L "CC" ("and",_), P "VP" vp2] r = \v -> intVP vp1 r v `VC` intVP vp2 r v
intVP [L "VB" vb1, L "CC" ("and",_), L "VB" vb2, P "NP" np] r = \v1 -> (intNP np r (\v2 -> (intVB2 vb1 r v1 v2) `VC` (intVB2 vb2 r v1 v2)))
intVP n@[L "VB" vb, P "PP" pp] r = intVB (stringify n,(0,0)) r
intVP [L "VB" (vb,i), P "NP" np, P "NP-TMP" nptmp] r = intVP [L "VB" (vb++"-"++stringify nptmp,i), P "NP" np] r
intVP [L "VB" ("do",_), L "RB" ("not",_), P "VP" vp] r = \v -> VQ0 (VSw `VC` intVP vp r v `VC` VSw `VC` VF)
intVP other r = error ("Unsuported by intVP: " ++ show other)

-- Stanford parser doesn't seperate transitive verbs, so our type has to accept any number of arguments
-- Hvis de to n;ste ikke bliver mere komplicerede kan vi ogs[ bare proppe dem ind i VP ovenfor.
intVB :: IWord -> Store -> Predicate
intVB (vb,_) r = \v -> VP vb [v]
intVB2 :: IWord -> Store -> Predicate2
intVB2 (vb,_) r = \v1 v2 -> VP vb [v1, v2]

intNN :: IWord -> Store -> Predicate
intNN (nn,_) r = \v -> VP nn [v]

intDT :: IWord -> Store -> Predicate -> Predicate -> Visser
intDT ("some", i) r = \p q -> VSc `VC` VE m `VC` p m `VC` VSc `VC` q m where m = r i
intDT ("a", i) r = intDT ("some", i) r
intDT ("the", i) r = \p q -> q m where m = r i
intDT ("every", i) r = \p q -> VSw `VC` VE m `VC` p m `VC` VSw `VC` q m where m = r i
intDT ("all", i) r = intDT ("every", i) r
intDT ("no", i) r = \p q -> VQ0 (VSw `VC` VE m `VC` p m `VC` q m `VC` VSw `VC` VF) where m = r i
intDT other r = error ("Unsuported by intDT: " ++ show other)

-- Future:

-- Adjectives:
-- (NP (JJ big) (NN sausage))
-- What about adjective phrases? ADJP
-- (ADJP (RB very) (JJ big))

-- Also:
-- ADVP - Adverb Phrase.
-- CONJP - Conjunction Phrase
-- PP - Prepositional Phrase.
-- Different WH phrases

flatten :: PosTree a -> [a]
flatten (P _ xs) = xs >>= flatten
flatten (L _ x) = [x]

stringify :: [PosTree IWord] -> String
stringify ts = intercalate "-" words
    where words = map fst (ts >>= flatten)
