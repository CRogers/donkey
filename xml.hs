import Text.XML.Light
import Control.Monad
import System.IO.Unsafe
import Data.List
import Data.Maybe
--main = interact parse
-- stanford-corenlp-full-2012-11-12/input.txt.xml
parse xml = show doc where Just doc = parseXMLDoc xml
-- readFile "stanford-corenlp-full-2012-11-12/input.txt.xml" >>= 

type Sentence = [String]

data DepTree = Dep Int [(String, DepTree)] deriving Show
data Coref = Reprs Int Int Int | Mention Int Int Int

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

 

--super :: Sentence -> Element -> Scop

-- let s = (unsafePerformIO . readFile) "stanford-corenlp-full-2012-11-12/input.txt.xml"
-- let Just doc = parseXMLDoc s

-- listDeps doc "basic-dependencies"
-- [("nn",2,1),("nsubj",4,2),("cop",4,3),("prep",4,5),("pobj",5,6)]
{-
Stanford University is located in California
Dep 4 [
	("nsubj", Dep 2 [
				("nn", Dep 1 [])]),
	("cop", Dep 3 []),
	("prep", Dep 5 [
		("pobj", Dep 6 [])])]
-}

-- todo: We might let unused variables go to other nps, just in case
corefs :: Element -> [String] -> [(Int, Int, String)]
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

takeVar :: [(Int, Int, String)] -> Int -> String
takeVar crs n = [s | (a, b, s) <- crs, a <= n && n < b] !! 0

parseVp ws crs (Dep n ds) = foldl sComp sT parts `sComp` sPredi (ws !! n) [takeVar sub, takeVar obj]
	where
		parts = mapMaybe (\(name, dep) -> (case name of
			 "nsubj" -> Just (parseNP var crs dep)
			 "dobj"  -> Just (parseNP var crs dep)
			 "advcl" -> Just (parseAdvcl var crs dep)
			 otherwise -> Nothing)) ds
		Just (_, Dep sub _) = lookup "nsubj" ds
		Just (_, Dep obj _) = lookup "dobj" ds

parseAdvcl ws crs dep = sSwitch `sComp` parseVp crs dep `sComp` sSwitch

-- Check if we are the primary mention, if true then add an exists and a test
parseNp ws crs (Dep n ds) = case isJust (lookup "det") ds of
	 True -> sExist var `sComp` sPredi (ws !! n) [var]
	 False -> sT
	where var = takeVar crs n


{-
If a farmer owns a donkey, he beats it

[Dep 9 [
	("advcl",Dep 4 [
		("mark",Dep 1 []),
		("nsubj",Dep 3 [
			("det",Dep 2 [])]),
		("dobj",Dep 6 [
			("det",Dep 5 [])])]),
	("nsubj",Dep 8 []),
	("dobj",Dep 10 [])]]


-}
-- let mention = findElements (unqual "mention") doc2 !! 0
-- let Just sentence = findChild (unqual "sentence") mention