module ToLogic (logify) where

import Dpl (dpl2prop, s2dpl, Type2, sT, sF, sSwitch, sScope, sPredi, sExist, sComp, sTest0, sTest1)
import Prop
import Stanford (DepTree(..), Word, Sentence, PosTree(..))

import Data.Map hiding (lookup, mapMaybe)
import Data.Maybe

parseVP :: Sentence -> Map Int Ref -> DepTree -> Type2
parseVP ws refs (Dep n ds) = foldl sComp sT parts `sComp` predi
	where
		parts = mapMaybe (\(name, dep) -> (case name of
			 "nsubj" -> Just (parseNP ws refs dep)
			 "dobj"  -> Just (parseNP ws refs dep)
			 "advcl" -> Just (parseAdvcl ws refs dep)
			 otherwise -> Nothing)) ds
		Just (Dep sub _) = lookup "nsubj" ds
		Just (Dep obj _) = lookup "dobj" ds
		predi = sPredi (ws !! n) [refs ! sub, refs ! obj]

parseAdvcl :: Sentence -> Map Int Ref -> DepTree -> Type2
parseAdvcl ws refs dep = sSwitch `sComp` (parseVP ws refs dep) `sComp` sSwitch

-- Check if we are the primary mention, if true then add an exists and a test
parseNP :: Sentence -> Map Int Ref -> DepTree -> Type2
parseNP ws refs (Dep n ds) = case isJust (lookup "det" ds) of
	 True -> sExist var `sComp` sPredi (ws !! n) [var]
	 False -> sT
	where var = refs ! n

logify :: (Sentence, Map Int Ref, DepTree) -> Prop
logify (sentence, refs, deps) = (simplify . dpl2prop . s2dpl) logic
	where
		logic = parseVP sentence refs deps

{-
monte :: PosTree (Int, Word) -> Map Int Ref -> Type2
monte (Phrase "ROOT" ts) = monte (head ts)
monte (Phrase "S" [(Phrase "NP" a), (Phrase "VP" b)])


parseRoot ws refs (D n ds)
	|
	 [("auxpass", p), ("nsubjpass", q) ("tmod", r)])


    monteHead (D _ vb [ns@(D "nsubj" _ _), do@(D "dobj" _ _)]) = (monteDobj dobj) (monteNsubj nsubj)
    monteHead ((Phrase "SBAR" [(Leaf "IN" "if"), (Phrase "S" s1)]):(Leaf "," _):s2) = sSwitch `sComp` monteS s1 `sComp` sSwitch `sComp` monteS s2

parse (P "NP" [("")])
-}

-- we can use a lookup :: a -> [(a,b)] -> [b]
-- but then how do we patternmatch to avoid empties? n:ns?
monteHead (w, es)
	| (n:subj) <- lookups "nsubj" es
	, (d:obj) <- lookups "dobj" es
	= monteNsubj nsubj `sComp`

lookups :: (Eq a) => a -> [(a,b)] -> [b]
lookups _ []                         = []
lookups  key ((x,y):xys) | key == x  = y : lookup key xys
                         | otherwise = lookup key xys


monteRoot :: [PosTree Word] -> Type2
monteRoot [(Phrase "ROOT" xs)] = monteRoot xs
monteRoot [(Phrase "S" xs)] = monteS xs
monteRoot p = error (show p)

monteS :: [PosTree Word] -> Type2
monteS [(Phrase "NP" np), (Phrase "VP" vp)] = (monteVP vp) (monteNP np)
monteS ((Phrase "SBAR" [(Leaf "IN" "if"), (Phrase "S" s1)]):(Leaf "," _):s2) = sSwitch `sComp` monteS s1 `sComp` sSwitch `sComp` monteS s2
monteS p = error (show p)

monteNP :: [PosTree Word] -> (Type2, [Ref])
monteNP [(Phrase "NP" n1), (Leaf "CC" "and"), (Phrase "NP" n2)] = (l1 `sComp` l2, r1 ++ r2)
	where (l1, r1) = monteNP n1
	      (l2, r2) = monteNP n2
monteNP [(Leaf "DT" "a"), (Leaf "NN" n)] = (sExist "x" `sComp` sPredi n ["x"], ["x"])
monteNP [(Phrase "NP" np), (Phrase "SBAR" sbar)] = (sSwitch `sComp` l `sComp` monteSBAR sbar `sComp` sSwitch, r)
	where (l, r) = monteNP np
monteNP [(Leaf "PRP$" _)] = (sT, ["x"])
monteNP [Leaf "PRP$" prp, Leaf "NN" nn] = (sExist "y" `sComp` sPredi nn ["y"] `sComp` sPredi "owns" ["x","y"], ["y"])
monteNP p = error (show p)

monteSBAR :: [PosTree Word] -> Type2
monteSBAR _ = sT
--monteSBAR p = error (show p)

monteVP :: [PosTree Word] -> (Type2, [Ref]) -> Type2
monteVP ps = \(nl, nrs) -> nl `sComp` foldl sComp sT [sPredi (stringify ps) [r] | r <- nrs]
--monteVP p = error (show p)

stringify :: [PosTree Word] -> String
stringify [] = ""
stringify ((Leaf _ word):ps) = word ++ "-" ++ stringify ps
stringify ((Phrase _ pss):ps) = stringify (pss ++ ps)

-- Or is a bitch. What if the amount of refs is not the same from each side?
-- And how does the outside code learn to know about the assignments we've made?
--     perhaps by changing the dictionary? But that would require sequentializing
--monteNP [(Phrase "NP" n1), (Leaf "CC" "or"), (Phrase "NP" n2)] = [l1 `sComp` (assign )]
--	where (l1, r1) = monteNP n1
--	      (l2, r2) = monteNP n2
--	      assign r s = sExist s `sComp` sPredi "=" [r,s]


-- tostring $ simplify $ dpl2prop $ s2dpl $ monteRoot [postreeS "(ROOT (S (SBAR (IN if) (S (NP (DT a) (NN farmer)) (VP (VBZ owns) (NP (DT a) (NN donkey))))) (, ,) (NP (PRP he)) (VP (VBZ beats) (NP (PRP it)))))"]
-- "Ax(((farmer(x))&(owns-a-donkey-(x)))->(beats-it-(x)))"

-- tostring $ simplify $ dpl2prop $ s2dpl $ monteRoot [postreeS "(ROOT (S (NP (NP (DT a) (NN man)) (CC and) (NP (PRP$ his) (NN dog))) (VP (VBD went) (PP (IN for) (NP (DT a) (NN walk))))))"]
-- "Ex((man(x))&(Ey((dog(y))&((owns(x,y))&((went-for-a-walk-(x))&(went-for-a-walk-(y)))))))"


{-
1. S -> NP VP : VP(NP)
2. NP -> Name : Name
3. VP -> Vintr : Vintr
4. VP -> Vtrans NP : Vtrans (NP)
5. Vintr -> snores : \x.snore(x), etc
6. Name -> John : john, etc
7. Vtrans -> hits : \y. \x. hit(x,y)
8. S -> S and S : S ^ S
9. VP -> VP and VP : \x.VP1(x) ^ VP2(x)
10. VP -> doesn’t VP : \x.:(VP(x))
-}
