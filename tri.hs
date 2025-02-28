module Tri where

chars :: Char -> Int -> String
chars c n = replicate n c

triL :: Char -> Int -> String
triL c 0 = ""
triL c n = unlines [chars c i | i <- [1..n]]

triR :: Char -> Int -> String
triR c 0 = ""
triR c n = unlines [spaces (n - i) ++ chars c i | i <- [1..n]]
 where
  spaces k = replicate k ' '