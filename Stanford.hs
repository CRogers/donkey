module Stanford (Store, Word, Index, run, runDry, runOnFile, IndexTree(..), postrees) where

import Prop (Ref)

import Text.ParserCombinators.Parsec
import Data.Map hiding (map, (\\))

import Data.List hiding (union, insert, lookup)
import Text.XML.Light
import Control.Monad
import Data.Maybe
import System.Process

type Index = (Int, Int)
type Word = String
type Sentence = [Word]

type PosTag = String
data PosTree = Phrase PosTag [PosTree] | Leaf PosTag Word deriving (Show)
data IndexTree = P PosTag [IndexTree] | L PosTag (Word,Index) deriving (Show)

data DepTree = Dep Int [(Ref, DepTree)] deriving (Show, Eq)

type Store = Index -> Ref

------------------

-- Run these functions on each <sentence>

-- Use "lemma" instead of "word" to get standardized forms
-- (did -> do, n't -> not)

lemmas :: Element -> Sentence
lemmas doc = map strContent (findElements (unqual "lemma") doc)

------------------

postrees :: Element -> [PosTree]
postrees doc = map rootfilter roots
	where
		parses = findElements (unqual "parse") doc
		roots = map (postreeRoot . strContent) parses
		rootfilter (Phrase "ROOT" [(Phrase "S" s)]) = (Phrase "S" s)
		rootfilter root = error ("Cant parse strange sentence " ++ show root)

postreeRoot :: String -> PosTree
postreeRoot dat = tree
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

-- Why use an index tree if we can move the referants stright in there?
-- Because a lot of things don't have references...

-- Adds a word index to every element in the postree
toIndexTrees :: [PosTree] -> [IndexTree]
toIndexTrees trees = map toIndexTree (zip trees [0..])
toIndexTree :: (PosTree, Int) -> IndexTree
toIndexTree (tree, sentence) = snd (toIndexTree_ 0 tree)
	where
		toIndexTree_ i (Phrase tag (t:ts)) = (k, P tag (it:its))
			where (j, it) = toIndexTree_ i t
			      (k, P _ its) = toIndexTree_ j (Phrase tag ts)
		toIndexTree_ i (Phrase tag []) = (i, P tag [])
		toIndexTree_ i (Leaf tag word) = (i+1, L tag (word,(sentence,i)))

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

corefs :: Element -> [Ref] -> ([Ref], Map Index Ref)
corefs doc vs = (vs', unions [singleton i v | (men, v) <- zip ments vs,
	                                         indices <- map parseMention men,
	                                         i <- indices])
	where
		corefs = findElements (unqual "coreference") doc >>= elChildren
		ments = map (findElements (unqual "mention")) corefs
		vs' = drop (length corefs) vs

parseMention :: Element -> [Index]
parseMention men = [(toint sentence-1, i) | i <- [toint start-1..toint end-2]]
	where
		Just sentence = findChild (unqual "sentence") men
		Just start = findChild (unqual "start") men
		Just end = findChild (unqual "end") men
		toint = read . strContent

allRefs :: Element -> [Ref] -> Store
allRefs doc vs = searcher
	where (vs', comap) = corefs doc vs
	      searcher i = findWithDefault (vs' !! diagonal i) i comap
	      diagonal (j,k) = (j+k)*(j+k+1)`div`2 + j


-- TODO: To help us avoid running out of variables, we might want to
--       flatten the map once in a while

--------------------

alphabet = [c:"" | c <- "abcdefghijklmnopqrstuvwxyz"]
variables :: [Ref]
variables = alphabet ++ [b++a | b <- variables, a <- alphabet]

runDry :: [String] -> IO [(Store, [IndexTree])]
runDry sentences =
	do
		sequence [runOnFile (name ++ ".xml") | name <- names]
	where
		names = ["input" ++ show i | i <- [1..length sentences]]

run :: [String] -> IO [(Store, [IndexTree])]
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

runOnFile :: FilePath -> IO (Store, [IndexTree])
runOnFile name =
	do
		f <- readFile name
		case parseXMLDoc f of
			Nothing -> error ("Unable to parse XML " ++ name)
			Just xml -> do
				return (
					--lemmas xml,
					allRefs xml variables,
					toIndexTrees (postrees xml))
					--(deps xml "collapsed-ccprocessed-dependencies") !! 0
