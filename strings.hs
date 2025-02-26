module Strings where

--diese Zeile versteckt die vordefinierten Fonktionen--
import Prelude hiding (repeat, last, init, (++), length)
import Data.Char (isAlphaNum, toLower)

(++) :: String -> String -> String
xs ++ ys 
  | null xs = ys
  | otherwise = head xs : (tail xs ++ ys)

length :: String -> Int
length s 
  | null s = 0
  | otherwise = 1+ length (tail s)

parrot :: Int -> String -> String
parrot n s 
  | n == 0 = ""
  | null s = ""
  | otherwise = s ++ parrot (n - 1) s

quadrat :: Int -> Char -> String
quadrat n c = (parrot n (parrot n (c: "") ++ "\n"))

last :: String -> Char
last s 
  | null s = error "s is null"
  | null (tail s) = head s
  | otherwise = (last (tail s))

init1 :: String -> String
init1 s
  | null s = s
  | null (tail s) = ""
  | otherwise = head s : init1 (tail s)

init2 ::  String -> String 
init2 s
  | null s = error "String darf nicht null sein!"
  | null (tail s) = ""
  | otherwise = head s : init2 (tail s)

init3 :: String -> String
init3 s = init3 s "" where
  init3 s r | null s = error "s darf nicht null sein!"
            | null (tail s) = r
            | otherwise = init3 (tail s) (r ++ (head s: ""))

palin1 :: String -> Bool
palin1 s 
  | length s <= 1 = True
  | head s == last s = palin1 (init1 (tail s))
  | otherwise = False

palin2 :: String -> Bool
palin2 s = length s <= 1 || head s == last s && palin2 (init1 (tail s))



-- So that the classic "A man, a plan, a canal: Panama!" works.
palin3 :: String -> Bool
palin3 s = palin2 (clean s)

clean :: String -> String
clean s 
  | null s = ""
  | isAlphaNum (head s) = toLower (head s) : clean (tail s)
  | otherwise = clean (tail s)