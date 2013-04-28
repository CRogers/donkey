import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import System.IO.Unsafe

import Data.List
import Data.Map hiding (map)
import Data.Char (chr, ord)

import Stanford
import Prop(Ref)

main = defaultMain tests

tests = [
    testGroup "Stanford tests" [
        testCase "test1" test_tomapping1,
        testCase "test2" test_tomapping2,
        testProperty "test3" prop_tomapping,
        testCase "test4" test_stanford
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
prop_tomapping postree = cntNp postree >= (length . nub . elems . toMapping2) postree
  where cntNp (Leaf _ _) = 0
        cntNp (Phrase "NP" subs) = 1 + sum (map cntNp subs)
        cntNp (Phrase _ subs) = sum (map cntNp subs)

test_stanford = (sentence, refs, deps) @?= (sout, rout, dout)
    where (sentence, refs, deps) = unsafePerformIO (runOnFile "test.xml")
          sout = ["if", "a", "farmer", "own", "a", "donkey", ",", "he", "beat", "it"]
          rout = fromList [(1, "e"), (2, "e"), (4, "f"), (5, "f"), (7, "e"), (9, "f")]
          dout = Dep 8 [("advcl",Dep 3 [("mark",Dep 0 []),("nsubj",Dep 2 [("det",Dep 1 [])]),("dobj",Dep 5 [("det",Dep 4 [])])]),("nsubj",Dep 7 []),("dobj",Dep 9 [])]