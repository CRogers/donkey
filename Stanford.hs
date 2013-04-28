module Stanford (Store, IWord, run, runDry, runOnFile, PosTree(..), postrees) where

import Fol (Ref)

import Text.ParserCombinators.Parsec
import Data.Map hiding (map, (\\), mapMaybe, null)

import Data.List hiding (union, insert, lookup)
import Text.XML.Light
import Control.Monad
import Data.Maybe
import System.Process

type Index = (Int, Int)
type Word = String
type IWord = (Word, Index)
type Sentence = [Word]

type PosTag = String
data PosTree a = P PosTag [PosTree a] | L PosTag a deriving (Show)

data DepTree = Dep Int [(Ref, DepTree)] deriving (Show, Eq)

type Store = Index -> Ref

------------------

-- lemmas are standardized forms of words. E.g. did -> do, n't -> not
lemmas :: Element -> [Sentence]
lemmas doc = map lemma (findElements (unqual "sentence") sentences)
	where
		Just sentences = findElement (unqual "sentences") doc
		lemma doc' = map strContent (findElements (unqual "lemma") doc')

------------------

postrees :: Element -> [PosTree Word]
postrees doc = map rootfilter roots
	where
		parses = findElements (unqual "parse") doc
		roots = map (postreeRoot . strContent) parses
		rootfilter (P "ROOT" [n]) = n
		rootfilter root = error ("Cant parse strange sentence " ++ show root)

postreeRoot :: String -> PosTree Word
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
		return (P tag subs)
posLeaf =
	do
		tag <- many (noneOf " ")
		char ' '
		word <- many (noneOf " ()")
		return (L tag word)


cleanTrees :: [PosTree Word] -> [Sentence] -> [PosTree IWord]
cleanTrees trees ls = map cleanElements $ map cleanTags $ improveTrees trees ls

improveTrees :: [PosTree Word] -> [Sentence] -> [PosTree IWord]
improveTrees trees ls = [improve t l s | (t, l, s) <- zip3 trees ls [0..]]
	where improve t l s = substitute t (zip l (zip (repeat s) [0..]))

-- simplifies certain tags with tedious information
cleanTags :: PosTree a -> PosTree a
cleanTags = treemap f id
	where
		f "VBD" = "VB"
		f "VBG" = "VB"
		f "VBN" = "VB"
		f "VBP" = "VB"
		f "VBZ" = "VB"
		f "NNS" = "NN"
		f other = other

-- deletes certain nodes with tedious information
cleanElements :: PosTree a -> PosTree a
cleanElements tree = fromJust (deleteTag "." tree)

-- Maybe this is not useful afterall
rearrangeCCs :: PosTree a -> PosTree a
rearrangeCCs (L t w)              = L t w
rearrangeCCs (P t xs) | null bs   = P t ccxs
					  | otherwise = P t [P "CC" [P t as, P t (tail bs)]]
                      -- | otherwise = P ("CC"++t) [P t as, P t (tail bs)]
	where
		ccxs = map rearrangeCCs xs
		(as, bs) = break f ccxs
		f (L t _) = t == "CC"
		f (P _ _) = False


-- deletes all nodes with a given tag
deleteTag :: PosTag -> PosTree a -> Maybe (PosTree a)
deleteTag t1 (P t2 xs) | t1 == t2  = Nothing
                       | otherwise = Just (P t2 (mapMaybe (deleteTag t1) xs))
deleteTag t1 (L t2 w)  | t1 == t2  = Nothing
                       | otherwise = Just (L t2 w)

-- map over tags and leaf values
treemap :: (PosTag -> PosTag) -> (b -> c) -> PosTree b -> PosTree c
treemap f g (L tag word) = L (f tag) (g word)
treemap f g (P tag subs) = P (f tag) (map (treemap f g) subs)

-- Replaces every leaf value with a value from a list, in order
substitute :: PosTree a -> [b] -> PosTree b
substitute tree list = snd (substitute_ tree list)
	where
		substitute_ (L tag x) (l:ls)  = (ls, L tag l)
		substitute_ (P tag []) ls     = (ls, P tag [])
		substitute_ (P tag (t:ts)) ls = (ls'', P tag (s:ss))
			where (ls', s)       = substitute_ t ls
			      (ls'', P _ ss) = substitute_ (P "" ts) ls'

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

runDry :: [String] -> IO [(Store, [PosTree IWord])]
runDry sentences =
	do
		sequence [runOnFile (name ++ ".xml") | name <- names]
	where
		names = ["input" ++ show i | i <- [1..length sentences]]

run :: [String] -> IO [(Store, [PosTree IWord])]
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

runOnFile :: FilePath -> IO (Store, [PosTree IWord])
runOnFile name =
	do
		f <- readFile name
		case parseXMLDoc f of
			Nothing -> error ("Unable to parse XML " ++ name)
			Just xml -> do
				return (
					allRefs xml variables,
					cleanTrees (postrees xml) (lemmas xml))
