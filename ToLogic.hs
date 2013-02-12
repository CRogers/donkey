module ToLogic (logify) where

import Dpl
import Prop

parseVP :: Sentence -> Map Int Ref -> DepTree -> Scop
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

parseAdvcl :: Sentence -> Map Int Ref -> DepTree -> Scop
parseAdvcl ws refs dep = sSwitch `sComp` (parseVP ws refs dep) `sComp` sSwitch

-- Check if we are the primary mention, if true then add an exists and a test
parseNP :: Sentence -> Map Int Ref -> DepTree -> Scop
parseNP ws refs (Dep n ds) = case isJust (lookup "det" ds) of
	 True -> sExist var `sComp` sPredi (ws !! n) [var]
	 False -> sT
	where var = refs ! n

logify :: (Sentence, Map Int Refs, DepTree) -> Prop
logify (sentence, refs, deps) = (simplify . dpl2prop' . s2dpl) logic
	where
		logic = parseVP sentence refs deps

