module Main (main) where

import Stanford (run, runDry)
import ToLogic (montague)
import qualified Visser as V
import qualified Dpl as D
import qualified Fol as F

import Data.List
import Text.XML.Light
import Control.Monad
import Data.Maybe
import System.Process

super :: [String] -> IO [V.Visser]
super sentences = run sentences >>= (return . map montague)
--super sentences = runDry sentences >>= (return . map montague)

sentences = [
	"Every farmer beats some donkey.",
	"If a farmer owns a donkey, he beats it",
	"If a farmer owns it and he beats it, it is a donkey",
	"If a farmer owns it and beats it, it is a donkey",
	"A man comes in. He sees a donkey. He smiles.", -- very sequential
	"A farmer. A Donkey. He owns it. He beats it",
	"A farmer. A Donkey. If he owns it, he beats it.",
	"All donkeys and a horse sing a song.",
	"A farmer starves and beats a donkey.",
	"A farmer starves a horse and beats a donkey.",
	"If a man walks, he talks.",
	"She is a mother of five", -- mother becomes the root, because is is not a 'real' verb
	"No man walks in the park. He yodels.", -- coref checker screws this up, but dpl fixes it
	"A farmer and a donkey visit a friend.",
	"No man walks in the park. He yodels and beats a donkey.",
	"A farmer owns a donkey. It is brown and he beats it every day.",
	"Some farmer and entrepreneur beats a donkey.", -- I'm not sure how to handle 'his'
	"A man and a woman owned a donkey.",
	"A man owned a horse and a donkey.",
	"A man and a woman owned a horse and a donkey.",
	"A man and a woman owned a horse and a donkey. The donkey did not walk. The man beat it.",
	"A man and a woman owned a horse and a donkey. If the donkey did not walk, the man beat it.",
	"A man and a woman owned a horse and a donkey. If the donkey did not walk and the man was a farmer, he beat it.",

	"A farmer and his wife own a horse and a donkey.", -- posessives...
	"Some farmer couldn't make his donkey see.", -- couldn't... Can't do modals
	"If there's a farmer and a donkey, the farmer beats the donkey.",
	"If there's a farmer who owns a donkey, the farmer beats the donkey.",
	"If a farmer owns a donkey or a horse, he beats it.", -- stanford doesn't let us coref over a disjunction
	"A farmer and a donkey live together.", -- I cannot do adverbs
	"If Mary is swimming or dancing, then Sue is.", -- I cannot do proper nouns
	"If John beat a horse or a donkey, his wife helped it", -- proper nouns


	"If you like me, you will hate my cousin",
	"Do you still beat your wife?",
	"James won a Nobel prize", -- commitment
	"The King of France is bald", -- unique, what if there's no king?
	"John regrets that his dog died.", -- what is the object?
	"When did you stop beating your wife"
	]

main :: IO ()
main = do
		putStrLn "Running Stanford Parser..."
		props <- super sentences
		sequence [do {
			putStrLn s;
			putStrLn $ (V.pretty . V.simplify) p;
			putStrLn $ (D.pretty . D.simplify . V.intVisser3) p;
			putStrLn $ (F.pretty . F.simplify . D.intDpl3 . V.intVisser3) p;
			putStrLn ""
		} | (s,p) <- zip sentences props]
		return ()

{-
These look hard (Existential non-commitment):
	Jim wants to marry a norweigan girl
	Fred was looking for a unicorn
	The golden mountain does not exist
	If a farmer owns a tractor, he drices it to church
-}