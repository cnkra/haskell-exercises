
counter :: Char -> String -> Int
counter c s = if null s then 0
else if head s == c then 1+ counter c (tail s)
else counter c (tail s)



counter2 :: Char -> String -> Int
counter2 c s 
  | null s = 0
  | head s == c = 1+ counter2 c (tail s)
  | otherwise = counter2 c (tail s)



counter3 :: Char -> String -> Int
counter3 c s = counter3' c s 0
counter3' :: Char -> String -> Int -> Int
counter3' c s r = if null s then r
else counter3' c (tail s) (if head s == c then 1+r else r)


counter4 :: Char -> String -> Int
counter4 c s = counter4' s 0 where
  counter4' s r = 
    if (null s) then r 
    else counter4' (tail s) (if (head s == c) then 1+r else r)