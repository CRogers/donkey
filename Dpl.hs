module Dpl ( parse, deps, corefs, Prop ) where

import Data.List
import Text.XML.Light
import Control.Monad
import Data.Maybe

data Dpl =
	    DT | DF
	  | DPredi Ref [Ref]
	  | DExist Ref
	  | DComp Dpl Dpl
	  | DNot Dpl
	  | DImpl Dpl Dpl
	  deriving (Eq, Show)

dpl2prop' dpl = dpl2prop PT [dpl]

dpl2prop :: Prop -> [Dpl] -> Prop
dpl2prop p [] = p
dpl2prop p (DF:ds) = PF
dpl2prop p (DT:ds) = dpl2prop p ds
dpl2prop p ((DPredi f ks):ds) = dpl2prop (PAnd (PPredi f ks) p) ds
dpl2prop p ((DExist k):ds) = dpl2prop (PExist k p) ds
dpl2prop p ((DComp d1 d2):ds) = dpl2prop p (d2:d1:ds)
dpl2prop p ((DNot d):ds) = dpl2prop (PAnd (PNot (dpl2prop PT [d])) p) ds
dpl2prop p ((DImpl d1 d2):ds) = dpl2prop p ((DNot (DComp d1 (DNot d2))):ds)


r1 = DImpl (DComp (DComp (DComp (DExist "x") (DPredi "farmer" ["x"])) (DExist "y")) (DPredi "donkey" ["y"])) (DPredi "beats" ["x", "y"])

type Pol = (Dpl, Dpl, Integer)
pT = (DT, DT, 1)
pF = (DT, DF, 1)
pSwitch = (DT, DT, -1)
pPredi :: Ref -> [Ref] -> Pol
pPredi f ks = (DT, DPredi f ks, 1)
pExist :: Ref -> Pol
pExist k = (DT, DExist k, 1)
pComp :: Pol -> Pol -> Pol
pComp (qm, qp, 1) (rm, rp, b) = (DComp qm rm, DComp qp rp, b)
pComp (qm, qp, -1) (rm, rp, b) = (DComp qm rp, DComp qp rm, -b)
pTest :: Pol -> Pol
pTest (qm, qp, a) = (DT, DImpl qm qp, 1)
p2dpl :: Pol -> Dpl
p2dpl (qm, qp, a) = DImpl qm qp

-- Axy ((donkey(y) && beats(x,y)) -> farmer(x))
r2 = pSwitch `pComp` pExist "x" `pComp` pSwitch `pComp` pPredi "farmer" ["x"] `pComp` pSwitch `pComp` pExist "y" `pComp` pPredi "donkey" ["y"] `pComp` pPredi "beats" ["x", "y"]

type Scop = (Dpl, Dpl, Dpl, Dpl, Integer, Integer)
sT = (DT, DT, DT, DT, 1, 0)
sF = (DT, DT, DT, DF, 1, 0)
sSwitch = (DT, DT, DT, DT, -1, 0)
sScope = (DT, DT, DT, DT, 1, 1)
sPredi :: Ref -> [Ref] -> Scop
sPredi f ks = (DT, DT, DT, DPredi f ks, 1, 0)
sExist :: Ref -> Scop
sExist k = (DT, DT, DT, DExist k, 1, 0)
sComp :: Scop -> Scop -> Scop
sComp (qm1, qm0, qp1, qp0, 1, 0) (rm1, rm0, rp1, rp0, b, j) =  (DComp qm1 rm1, DComp qm0 rm0, DComp qp1 rp1, DComp qp0 rp0, b, j)
sComp (qm1, qm0, qp1, qp0, 1, 1) (rm1, rm0, rp1, rp0, b, j) =  (DComp qm1 rm0, DComp qm0 rm1, DComp qp1 rp0, DComp qp0 rp1, b, 1-j)
sComp (qm1, qm0, qp1, qp0, -1, 0) (rm1, rm0, rp1, rp0, b, j) = (DComp qm1 rp1, DComp qm0 rp0, DComp qp1 rm1, DComp qp0 rm0, -b, j)
sComp (qm1, qm0, qp1, qp0, -1, 1) (rm1, rm0, rp1, rp0, b, j) = (DComp qm1 rp0, DComp qm0 rp1, DComp qp1 rm0, DComp qp0 rm1, -b, 1-j)
sTest0 :: Scop -> Scop
sTest0 (qm1, qm0, qp1, qp0, a, i) = (qm1, DT, qp1, DImpl qm0 qp0, 1, 0)
sTest1 :: Scop -> Scop
sTest1 (qm1, qm0, qp1, qp0, a, i) = (DT, DT, DT, DImpl (DComp qm1 qm0) (DComp qp1 qp0), 1, 0)
s2dpl :: Scop -> Dpl
s2dpl (qm1, qm0, qp1, qp0, a, i) = DImpl (DComp qm1 qm0) (DComp qp1 qp0)

-- A farmer owns a donkey, he beats it
-- Ex (farmer(x) && Ey (donkey(y) && owns(x,y) && beats(x,y)))
r3 = sScope `sComp` sExist "x" `sComp` sPredi "farmer" ["x"] `sComp` sScope `sComp` sPredi "owns" ["x", "y"] `sComp` sScope `sComp` sExist "y" `sComp` sPredi "donkey" ["y"] `sComp` sScope `sComp` sPredi "beats" ["x", "y"]

-- Only if a FARMER owns a donkey, does he beat it
-- some error here!
r4 = sTest0 (sSwitch `sComp` sScope `sComp` sExist "x" `sComp` sSwitch `sComp` sPredi "farmer" ["x"] `sComp` sSwitch `sComp` sScope `sComp` sPredi "owns" ["x", "y"] `sComp` sScope `sComp` sExist "y" `sComp` sPredi "donkey" ["y"] `sComp` sScope `sComp` sPredi "beats" ["x", "y"])

-- A farmer beats a donkey, if he owns it
-- Ax (farmer(x) -> Ay((donkey(y) && owns(x,y)) -> beats(x,y)))
-- Ex (farmer(x) && Ay((donkey(y) && owns(x,y)) -> beats(x,y)))
r5 = sTest0 (sScope `sComp` sSwitch `sComp` sExist "x" `sComp` sPredi "farmer" ["x"] `sComp` sSwitch `sComp` sScope `sComp` sPredi "beats" ["x", "y"] `sComp` sScope `sComp` sSwitch `sComp` sExist "y" `sComp` sPredi "donkey" ["y"] `sComp` sSwitch `sComp` sScope `sComp` sSwitch `sComp` sPredi "owns" ["x", "y"])
r6 = sTest0 (sScope `sComp` sExist "x" `sComp` sPredi "farmer" ["x"] `sComp` sScope `sComp` sPredi "beats" ["x", "y"] `sComp` sScope `sComp` sExist "y" `sComp` sPredi "donkey" ["y"] `sComp` sScope `sComp` sSwitch `sComp` sPredi "owns" ["x", "y"])



------------------------------------------------

type Sentence = [String]
data DepTree = Dep Int [(Ref, DepTree)] deriving Show
type Corefs = [(Int, Int, Ref)]

deps :: Element -> String -> [DepTree]
deps doc name = map (buildTree deps) roots
	where
		deps = listDeps doc name
		lefts = nub [gov | (typ, gov, dep) <- deps]
		rights = nub [dep | (typ, gov, dep) <- deps]
		roots = lefts \\ rights

buildTree :: [(String, Int, Int)] -> Int -> DepTree
buildTree deps root = Dep root [(name, buildTree deps dep) | (name, gov, dep) <- deps, gov == root]

listDeps :: Element -> String -> [(String, Int, Int)]
listDeps doc name = map parseDep deps
	where
		Just dep = findElement (unqual name) doc
		deps = findElements (unqual "dep") dep

parseDep :: Element -> (String, Int, Int)
parseDep e = (typ, read gov, read dep)
	where
		Just typ = findAttr (unqual "type") e
		Just gov = findChild (unqual "governor") e >>= (findAttr (unqual "idx"))
		Just dep = findChild (unqual "dependent") e >>= (findAttr (unqual "idx"))

 -- todo: We might let unused variables go to other nps, just in case
corefs :: Element -> [String] -> Corefs
corefs doc vs = [(a,b,v) | (ms,v) <- zip refs vs, (a,b) <- ms]
	where
		refs = map parseCoref (findElements (unqual "coreference") doc >>= elChildren)

parseCoref :: Element -> [(Int, Int)]
parseCoref ref = map parseMention (findElements (unqual "mention") ref)

parseMention :: Element -> (Int, Int)
parseMention men = (toint start, toint end)
	where
		Just start = findChild (unqual "start") men
		Just end = findChild (unqual "end") men
		toint = read . strContent

takeVar :: Corefs -> Int -> Ref
takeVar crs n = head [s | (a, b, s) <- crs, a <= n && n < b]

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

