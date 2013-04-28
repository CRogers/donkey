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
			"Every farmer beats some donkey.",
			"If a farmer owns a donkey, he beats it",
			"If a farmer owns it and he beats it, it is a donkey", -- fails because 'donkey' becomes head somehow. (It even has a nsubj, huh?)
			"If a farmer owns it and beats it, it is a donkey", -- fails because co-ref doesn't handle single occourences
			"A man comes in. He sees a dog. He smiles.", -- very sequential
			"A farmer. A Donkey. He owns it. He beats it", -- fails because we assume all sentences have a VP root
			"All donkeys and a horse sing a song.",
			"A farmer starves and beats a donkey.",
			"A farmer starves a horse and beats a donkey.",
			"If a man walks, he talks.",
			"She is a mother of five", -- mother becomes the root, because is is not a 'real' verb
			"No man walks in the park. He yodels.",
			"A farmer and a donkey visit a friend.",
			--"A farmer and a donkey live together.", -- I cannot do adverbs
			--"If Mary is swimming or dancing, then Sue is.", -- I cannot do proper nouns
			-- "If John beat a horse or a donkey, his wife helped it",
			"No man walks in the park. He yodels and beats a donkey.",
			"A farmer owns a donkey. It is brown and he beats it every day.",
			"A farmer and entrepreneur beat a donkey.", -- I'm not sure how to handle 'his'
			-- "The man could not make his donkey walk.", fails because SP things walk is a noun
			"Some farmer couldn't make his donkey see.",
			"If there's a farmer and a donkey, the farmer beats the donkey.",
			"If there's a farmer who owns a donkey, the farmer beats the donkey.",

			"If you like me, you will hate my cousin",
			"Do you still beat your wife?",
			"James won a Nobel prize", -- commitment
			"The King of France is bald", -- unique, what if there's no king?
			"John regrets that his dog died.", -- what is the object?
			"When did you stop beating your wife"
			]
		putStrLn "Test2"
		sequence [putStr $ tostring p ++ "\n" | p <- props]
		return ()

{-
These look hard (Existential non-commitment):
	Jim wants to marry a norweigan girl
	Fred was looking for a unicorn
	The golden mountain does not exist
	If a farmer owns a tractor, he drices it to church
-}