module Stanford (DepTree(..), Word, Sentence, run, runDry, runOnFile, PosTree(..), toMapping, postreeS) where

import Prop (Ref)

import Text.ParserCombinators.Parsec
import Data.Map hiding (map, (\\))

import Data.List hiding (union, insert)
import Text.XML.Light
import Control.Monad
import Data.Maybe
import System.Process

type Word = String
type Sentence = [Word]

type PosTag = String
data PosTree a = Phrase PosTag [PosTree a] | Leaf PosTag a deriving (Show)

data DepTree = Dep Int [(Ref, DepTree)] deriving (Show, Eq)

------------------

-- Run these functions on each <sentence>

-- Use "lemma" instead of "word" to get standardized forms
-- (did -> do, n't -> not)

lemmas :: Element -> Sentence
lemmas doc = map strContent (findElements (unqual "lemma") doc)

------------------

postree :: Element -> PosTree Word
postree doc = postreeS dat
	where
		dat = (strContent . fromJust) (findElement (unqual "parse") doc)

postreeS :: String -> PosTree Word
postreeS dat = tree
	where
		Right tree = parse posEither "(unknown)" dat

posEither =
	do
		char '('
		res <- try posTree <|> posLeaf
		char ')'
		return res
posTree =
	do
		tag <- many (noneOf " ")
		char ' ' -- maybe not needed?
		subs <- sepBy1 posEither (char ' ')
		return (Phrase tag subs)
posLeaf =
	do
		tag <- many (noneOf " ")
		char ' '
		word <- many (noneOf " ()")
		return (Leaf tag word)

toMapping :: [Ref] -> PosTree Word -> ([Ref], Map Int Ref)
toMapping vs (Leaf _ _) = (vs, empty)

toMapping vs tr@(Phrase "NP" subs) = (vs', submaps `union` np)
	where np = mapInterval 0 (tsize tr) v
	      (v:vs', submaps) = toMapping vs (Phrase "" subs)

toMapping vs (Phrase _ []) = (vs, empty)
toMapping vs (Phrase _ (sub:subs)) = (vs'', p `union` shifted)
	where (vs', subm) = toMapping vs (Phrase "" subs)
	      shifted = mapKeys (+tsize sub) subm
	      (vs'', p) = toMapping vs' sub

tsize :: PosTree Word -> Int
tsize (Leaf _ _) = 1
tsize (Phrase _ subs) = sum (map tsize subs)

mapInterval :: Int -> Int -> Ref -> Map Int Ref
mapInterval a b r = foldl (\m i -> insert i r m) empty [a..b-1]

------------------

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
parseDep e = (typ, read gov - 1, read dep - 1)
	where
		Just typ = findAttr (unqual "type") e
		Just gov = findChild (unqual "governor") e >>= (findAttr (unqual "idx"))
		Just dep = findChild (unqual "dependent") e >>= (findAttr (unqual "idx"))

------------------

corefs :: Element -> [Ref] -> ([Ref], Map Int Ref)
corefs doc vs = (vs', unions [mapInterval a b v | (men, v) <- zip ments vs, (a, b) <- map parseMention men])
	where
		corefs = findElements (unqual "coreference") doc >>= elChildren
		ments = map (findElements (unqual "mention")) corefs
		vs' = drop (length corefs) vs

parseMention :: Element -> (Int, Int)
parseMention men = (toint start - 1, toint end - 1)
	where
		Just start = findChild (unqual "start") men
		Just end = findChild (unqual "end") men
		toint = read . strContent

allRefs :: Element -> [Ref] -> Map Int Ref
allRefs doc vs = comap `union` posmap
	where (vs', posmap) = toMapping vs (postree doc)
	      (vs'', comap) = corefs doc vs'

-- TODO: To help us avoid running out of variables, we might want to
--       flatten the map once in a while

--------------------

variables :: [Ref]
variables = [c:"" | c <- "abcdefghijklmnopqrstuvwxyz"]

runDry :: [String] -> IO [(Sentence, Map Int Ref, DepTree)]
runDry sentences =
	do
		sequence [runOnFile (name ++ ".xml") | name <- names]
	where
		names = ["input" ++ show i | i <- [1..length sentences]]

run :: [String] -> IO [(Sentence, Map Int Ref, DepTree)]
run sentences =
	do
		sequence (zipWith writeFileLn names sentences)
		writeFileLn "inputlist" (intercalate "\n" names)
		(_, _, _, h) <- createProcess (shell "stanford-corenlp-full-2012-11-12/corenlp.sh -filelist inputlist")
		exitCode <- waitForProcess h
		putStrLn ("Exit code: " ++ show exitCode)
		sequence [runOnFile (name ++ ".xml") | name <- names]
	where
		names = ["input" ++ show i | i <- [1..length sentences]]
		writeFileLn path s = writeFile path (s ++ "\n")

runOnFile :: FilePath -> IO (Sentence, Map Int Ref, DepTree)
runOnFile name =
	do
		f <- readFile name
		case parseXMLDoc f of
			Nothing -> error ("Unable to parse XML " ++ name)
			Just xml -> do
				return (
					lemmas xml,
					allRefs xml variables,
					(deps xml "collapsed-ccprocessed-dependencies") !! 0)
