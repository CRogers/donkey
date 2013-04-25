module Main (main) where

import Stanford (run, runDry)
import Prop (Prop, tostring)
import ToLogic (logify)

import Data.List
import Text.XML.Light
import Control.Monad
import Data.Maybe
import System.Process

super :: [String] -> IO [Prop]
--super sentences = run sentences >>= (return . map logify)
super sentences = runDry sentences >>= (return . map logify)

main :: IO ()
main = do
		putStrLn "Test1"
		props <- super [
			"If a farmer owns a donkey, he beats it",
			"If a farmer owns it and he beats it, it is a donkey", -- fails because 'donkey' becomes head somehow. (It even has a nsubj, huh?)
			"If a farmer owns it and beats it, it is a donkey", -- fails because co-ref doesn't handle single occourences
			"She is a mother of five", -- mother becomes the root, because is is not a 'real' verb
			"A farmer. A Donkey. He owns it. He beats it", -- fails because we assume all sentences have a VP root
			"If you like me, you will hate my cousin",
			"Do you still beat your wife?",
			-- "The man could not make his donkey walk.", fails because SP things walk is a noun
			"The man couldn't make his donkey see.",
			"If there's a farmer and a donkey, the farmer beats the donkey.",
			"If there's a farmer who owns a donkey, the farmer beats the donkey.",
			"James won a Nobel prize", -- commitment
			"The King of France is bold", -- unique, what if there's no king?
			"John regrets that his dog died.", -- what is the object?
			"When did you stop beating your wife",
			"A man comes in. He sees a dog. He smiles." -- very sequential
			]
		putStrLn "Test2"
		sequence (map (print.tostring) props)
		return ()

{-
These look hard (Existential non-commitment):
	Jim wants to marry a norweigan girl
	Fred was looking for a unicorn
-}