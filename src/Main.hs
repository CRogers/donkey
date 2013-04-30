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
--super sentences = run sentences >>= (return . map montague)
super sentences = runDry sentences >>= (return . map montague)

sentences = [
	"Every farmer beats some donkey.",
	"If a farmer owns a donkey, he beats it",

	-- sequentials
	"A man comes in. He sees a donkey. He smiles.", -- very sequential
	"A farmer. A Donkey. If he owns it, he beats it.", -- sequential

	-- ifs
	"A dog barks. If it is beaten, it whines.", -- a donkey brays, but Stanford doesn't like it
	"If a farmer owns a donkey, he beats it. If he rewards it, he doesn't own it.",
	"If a farmer owns a donkey, he owns a horse. If he doesn't own it, he owns a cow.",
	"If a woman is American, she loves a soldier. If she is Dutch, she loves a bike-rider.",

	-- stringifying
	"A farmer owns a donkey. It is brown and he beats it every day.",

	-- doubles
	"A farmer starves a horse and beats a donkey.", -- double vp
	"All donkeys and a horse sing a song.", -- double np
	"Some farmer and entrepreneur beats a donkey.", -- double description
	"A man and a woman owned a horse and a donkey. If the donkey did not walk, and the man was a farmer, he beat it.", -- cross product

	-- free variables
	"She is a mother of five", -- mother becomes the root, because is is not a 'real' verb
	"If a farmer owns it and he beats it, it is a donkey",
	"No man walks in the park. He yodels.", -- coref checker screws this up, but dpl fixes it

------------- I Can do until this line

	-- suffix SBARs
	"A farmer beats a donkey, if he owns it.",
	"A dog barks. It whines, if beaten.",
	"A farmer feeds a donkey, unless he owns it.",
	"He beats it, if a farmer owns a donkey. He treats it well, if he doesn’t own it.",

	-- sentences with proper nouns
	"If Mary is swimming or dancing, then Sue is.",
	"If John beat a horse or a donkey, his wife helped it",
	"He was quite angry. John. He was MAD.",
	"The King of France is bald",

	-- sentences with disjunction.
	-- stanford doesn't let us coref over a disjunction
	"If a farmer owns a donkey or a horse, he beats it.",
	"This house has no bathroom or it is very small.",
	"A man drinks nothing or it is beer.",
	"A man drinks a beer or he doesn’t drink anything.",
	"A man drinks it or it is not a beer.",
	"A dog barks or it whines. It is nuisance.",

	-- sentences with weird uses of 'no'
	"John sees nobody.",
	"No man sees a woman.",
	"A man comes in. He sees no woman.",
	"A man saw no one on the stairs.",
	"A man comes in. He sees nobody in the room.",
	"Nobody sees nobody.",

	-- late introduction of entity
	"If he owns it, he beats it. A farmer, a donkey.",

	-- possessives
	"A farmer and his wife own a horse and a donkey.",

	-- modals
	"Some farmer couldn't make his donkey see.", -- couldn't... Can't do modals
	"If you like me, you will hate my cousin.",

	-- EX there
	"If there's a farmer and a donkey, the farmer beats the donkey.",
	"If there's a farmer who owns a donkey, the farmer beats the donkey.",

	-- adverbs
	"A farmer and a donkey live together.",
	"Do you still beat your wife?",

	-- Existential non-commitment
	"Jim wants to marry a Norweigan girl",
	"Fred was looking for a unicorn",
	"The golden mountain does not exist",

	-- Missing features in coref tagger
	"Every dog sees a cat. It chases it."
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
