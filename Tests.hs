import Stanford

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.List
import Data.Map hiding (map)
import Data.Char (chr, ord)

main = defaultMain tests

tests = [
    testGroup "Stanford tests" [
        testCase "test1" test_tomapping1,
        testCase "test2" test_tomapping2,
        testProperty "test3" prop_tomapping
     ],
    testGroup "Dpl tests" [

     ]
 ]

instance Arbitrary Char where
  arbitrary = chr `fmap` oneof [choose (0,127), choose (0,255)]
  coarbitrary = coarbitrary . ord

instance Arbitrary PosTree where
    arbitrary = do
        n <- choose (0, 3) :: Gen Int
        n <- return $ if n == 0 then 0 else 1
        case n of
            0 -> do
                k <- choose (1, 4) :: Gen Int
                subs <- sequence (replicate k arbitrary)
                tag <- oneof [elements ["NP"], arbitrary]
                return (Phrase tag subs)
            1 -> do
                tag <- arbitrary
                word <- arbitrary
                return (Leaf tag word)
    coarbitrary a b = b

variables :: [Ref]
variables = [c:"" | c <- "abcdefghijklmnopqrstuvwxyz"]
toMapping2 :: PosTree -> Map Int Ref
toMapping2 tr = snd (toMapping variables tr)

test_tomapping1 = (toMapping2 . postreeS) inp @?= out
    where inp = "(NP (DT The) (NN man))"
          out = fromList [(0, "a"), (1, "a")]

test_tomapping2 = (toMapping2 . postreeS) inp @?= out
    where inp = "(ROOT (S (NP (DT The) (NN man)) (VP (VBD did) (RB not) (VP (VB make) (S (NP (PRP$ his) (NN donkey)) (VP (VB see))))) (. .)))"
          out = fromList [(0, "b"), (1, "b"), (5, "a"), (6, "a")]

prop_tomapping :: PosTree -> Bool
prop_tomapping postree = cntNp postree <= (length . nub . elems . toMapping2) postree
  where cntNp (Leaf _ _) = 0
        cntNp (Phrase "NP" subs) = 1 + sum (map cntNp subs)
        cntNp (Phrase _ subs) = sum (map cntNp subs)
