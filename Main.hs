module Main (main) where

import Dpl ( parse, deps, corefs, Prop )

import Data.List
import Text.XML.Light
import Control.Monad
import Data.Maybe
import System.Process

super :: [String] -> IO [Prop]
super ss = do
		sequence (zipWith writeFileLn names ss)
		writeFileLn "inputlist" (intercalate "\n" names)
		(_, _, _, h) <- createProcess (shell "stanford-corenlp-full-2012-11-12/corenlp.sh -filelist inputlist")
		exitCode <- waitForProcess h 
		putStrLn ("Exit code: " ++ show exitCode)
		fs <- sequence [readFile (name ++ ".xml") | name <- names]
		print (corefs ((fromJust.parseXMLDoc) (fs !! 1)) ["x", "y", "z"])
		print (deps ((fromJust.parseXMLDoc) (fs !! 1)) "basic-dependencies")
		return (zipWith parse ss fs)
	where
		names = ["input" ++ show i | i <- [1..length ss]]
		writeFileLn path s = writeFile path (s ++ "\n")

main :: IO ()
main = do
		putStrLn "Test1"
		props <- super [
			"If a farmer owns a donkey , he beats it",
			"If a farmer owns it and he beats it , it is a donkey", -- fails because 'donkey' becomes head somehow. (It even has a nsubj, huh?)
			"If a farmer owns it and beats it , it is a donkey", -- fails because co-ref doesn't handle single occourences
			"She is a mother of five", -- mother becomes the root, because is is not a 'real' verb
			"A farmer . A Donkey . He owns it . He beats it", -- fails because we assume all sentences have a VP root
			"If you like me , you will hate my cousin"]
		putStrLn "Test2"
		sequence (map print props)
		return ()
