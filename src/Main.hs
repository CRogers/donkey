module Main (main) where

import Stanford (run, runDry)
import ToLogic (montague)
import qualified Visser as V
import qualified Dpl as D
import qualified Fol as F

import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import System.IO

readSentences :: FilePath -> IO [String]
readSentences path = do
		handle <- openFile path ReadMode
		contents <- hGetContents handle
		return [trimComments l | l <- lines contents, (not.null) (trimComments l)]

-- This trims a string for tabs and spaces. Also remove comment suffixes
trimComments :: String -> String
trimComments = reverse.dropWhile isSpace.reverse . dropWhile isSpace . takeWhile (/= '#')
	where
		isSpace ' '	 = True
		isSpace '\t' = True
		isSpace _    = False

main :: IO ()
main = do
		args <- getArgs
		if null args then error "Usage: donkey inputfile [--dry]" else return ()
		sentences <- readSentences (head args)
		putStrLn "Running Stanford Parser..."
		model <- (if args == ["--dry"] then runDry else run) sentences
		sequence [do {
			putStrLn s;
			putStrLn $ (V.pretty . V.simplify) p;
			putStrLn $ (D.pretty . D.simplify . V.intVisser3) p;
			putStrLn $ (F.pretty . F.simplify . D.intDpl3 . V.intVisser3) p;
			putStrLn ""
		} | (s, p) <- zip sentences (map montague model)]
		return ()
