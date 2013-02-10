import Text.ParserCombinators.Parsec

type Ref = String
type Word = String
type Sentence = [Word]
type Corefs = [(Int, Int, Ref)]
type PosTag = String
data PosTree = Phrase PosTag [PosTree] | Leaf PosTag Word deriving (Show)


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
