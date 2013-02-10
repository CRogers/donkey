import Stanford

import Char
import List
import Test.QuickCheck
import Text.Printf

main = sequence_ [printf "%-25s: " s >> a | (s,a) <- tests]

-- reversing twice a finite list, is the same as identity
prop_reversereverse s = (reverse . reverse) s == id s
    where _ = s :: [Int]

instance Arbitrary PosTree where
    arbitrary = do
        n <- choose (0, 3) :: Gen Int
        n <- return $ if n == 0 then 0 else 1
        tag <- oneof [elements ["NP"], arbitrary]
        case n of
            0 -> do
                k <- choose (1, 4) :: Gen Int
                subs <- sequence (replicate k arbitrary)
                return (Phrase tag subs)
            1 -> do
                word <- arbitrary
                return (Leaf tag word)

prop_size1 s = fst (toMapping s) >= 1
    where _ = s :: PosTree

-- and add this to the tests list
tests  = [
    ("reverse.reverse/id", quickCheck prop_reversereverse),
    ("mapping >= 1", verboseCheck prop_size1)]

