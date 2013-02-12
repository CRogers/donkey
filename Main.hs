module Main (main) where

import Stanford (run)

import Data.List
import Text.XML.Light
import Control.Monad
import Data.Maybe
import System.Process

super :: [String] -> IO [Prop]
super sentences = run sentences >>= (return . logify)

main :: IO ()
main = do
		putStrLn "Test1"
		props <- super [
			"If a farmer owns a donkey, he beats it",
			"If a farmer owns it and he beats it , it is a donkey", -- fails because 'donkey' becomes head somehow. (It even has a nsubj, huh?)
			"If a farmer owns it and beats it , it is a donkey", -- fails because co-ref doesn't handle single occourences
			"She is a mother of five", -- mother becomes the root, because is is not a 'real' verb
			"A farmer . A Donkey . He owns it . He beats it", -- fails because we assume all sentences have a VP root
			"If you like me , you will hate my cousin",
			"Do you still beat your wife?",
			-- "The man could not make his donkey walk.", fails because SP things walk is a noun
			"The man couldn't make his donkey see.",
			"If there's a farmer and a donkey, the farmer beats the donkey.",
			"If there's a farmer who owns a donkey, the farmer beats the donkey."]
		putStrLn "Test2"
		sequence (map print props)
		return ()
