module Utils (wrap, replace) where

import Data.List (isPrefixOf)

-- This is a standard subtring replacement function
replace :: String -> String -> String -> String
replace pat repl target
  | target == ""            = ""
  | pat `isPrefixOf` target = repl ++ replace pat repl (drop (length pat) target)
  | otherwise               = head target : replace pat repl (tail target)

-- This is used for pretty printing
wrap :: Int -> (String, Int) -> String
wrap n (p, k) | k > n     = "(" ++ p ++ ")"
              | otherwise = p
