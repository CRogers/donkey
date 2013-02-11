
module Stanford (Corefs, DepTree, run, PosTree(..), toMapping2, parse, posEither) where

import Text.ParserCombinators.Parsec
import Data.Map hiding (map, (\\))

import Data.List hiding (union, insert)
import Text.XML.Light
import Control.Monad
import Data.Maybe
import System.Process



type Ref = String
type Word = String
type Sentence = [Word]
type Corefs = [(Int, Int, Ref)]
type PosTag = String
data PosTree = Phrase PosTag [PosTree] | Leaf PosTag Word deriving (Show)

variables :: [Ref]
variables = [c:"" | c <- "abcdefghijklmnopqrstuvwxyz"]

------------------

-- Run these functions on each <sentence>

-- Use "lemma" instead of "word" to get standardized forms
-- (did -> do, n't -> not)

lemmas :: Element -> Sentence
lemmas doc = map strContent (findElements (unqual "word") doc)

------------------

postree :: Element -> PosTree
postree doc = tree
	where
		Right tree = parse posEither "(unknown)" dat
		dat = (strContent . fromJust) (findElement (unqual "parse") doc)

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

toMapping2 :: PosTree -> Map Int Ref
toMapping2 tr = snd (toMapping variables tr)

toMapping :: [Ref] -> PosTree -> ([Ref], Map Int Ref)
toMapping vs (Leaf _ _) = (vs, empty)

toMapping vs tr@(Phrase "NP" subs) = (vs', submaps `union` np)
	where np = setInterval empty 0 (tsize tr) v
	      (v:vs', submaps) = toMapping vs (Phrase "" subs)

toMapping vs (Phrase _ []) = (vs, empty)
toMapping vs (Phrase _ (sub:subs)) = (vs'', p `union` shifted)
	where (vs', subm) = toMapping vs (Phrase "" subs)
	      shifted = mapKeys (+tsize sub) subm
	      (vs'', p) = toMapping vs' sub

tsize :: PosTree -> Int
tsize (Leaf _ _) = 1
tsize (Phrase _ subs) = sum (map tsize subs)

setInterval :: Map Int Ref -> Int -> Int -> Ref -> Map Int Ref
setInterval m a b r = foldl (\m i -> insert i r m) m [a..b-1]

-- TODO: The idea is to first use toMapping to create a basic assignment,
--		and then impose corefs on top

------------------

data DepTree = Dep Int [(Ref, DepTree)] deriving Show

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

------------------

 -- todo: We might let unused variables go to other nps, just in case
corefs :: Element -> [Ref] -> Corefs
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

run :: [String] -> IO [(Sentence, Corefs, DepTree)]
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

runOnFile :: FilePath -> IO (Sentence, Corefs, DepTree)
runOnFile name =
	do
		f <- readFile name
		case parseXMLDoc f of
			Nothing -> error ("Unable to parse XML " ++ name)
			Just xml -> do
				return (
					lemmas xml,
					corefs xml variables,
					(deps xml "basic-dependencies") !! 0)
