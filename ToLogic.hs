module ToLogic (logify) where

import Dpl (dpl2prop, s2dpl, Type2, sT, sF, sSwitch, sScope, sPredi, sExist, sComp, sTest0, sTest1)
import Prop
import Stanford (DepTree(..), Word, Sentence)

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

monte :: PosTree (Int, Word) -> Map Int Ref -> Type2
monte (Phrase "ROOT" ts) = monte (head ts)
monte (Phrase "S" [(Phrase "NP" a), (Phrase "VP" b)])

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
