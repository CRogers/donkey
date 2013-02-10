module ToLogic ( parse, deps, corefs, Prop ) where

parseVP :: Sentence -> Corefs -> DepTree -> Scop
parseVP ws crs (Dep n ds) = foldl sComp sT parts `sComp` predi
	where
		parts = mapMaybe (\(name, dep) -> (case name of
			 "nsubj" -> Just (parseNP ws crs dep)
			 "dobj"  -> Just (parseNP ws crs dep)
			 "advcl" -> Just (parseAdvcl ws crs dep)
			 otherwise -> Nothing)) ds
		Just (Dep sub _) = lookup "nsubj" ds
		Just (Dep obj _) = lookup "dobj" ds
		predi = sPredi (ws !! n) [takeVar crs sub, takeVar crs obj]

parseAdvcl :: Sentence -> Corefs -> DepTree -> Scop
parseAdvcl ws crs dep = sSwitch `sComp` (parseVP ws crs dep) `sComp` sSwitch

-- Check if we are the primary mention, if true then add an exists and a test
parseNP :: Sentence -> Corefs -> DepTree -> Scop
parseNP ws crs (Dep n ds) = case isJust (lookup "det" ds) of
	 True -> sExist var `sComp` sPredi (ws !! n) [var]
	 False -> sT
	where var = takeVar crs n

parse :: String -> String -> Prop
parse s xml = conv (parseVP sentence crs dpt)
	where
		sentence = "root" : words s
		vars = ["x", "y", "z", "t"]
		Just doc = parseXMLDoc xml
		crs = corefs doc vars
		dpt = (deps doc "basic-dependencies") !! 0
		conv = (fexp simplify 10) . dpl2prop' . s2dpl

