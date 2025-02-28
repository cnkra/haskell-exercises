fact1 :: Integer -> Integer
fact1 n = 
  if n <= 0 then 1
  else n * fact1 (n - 1)

fact2 :: Integer -> Integer -> Integer
fact2 n r = 
  if n <= 0 then r
  else fact2 (n - 1) (n * r)

fact3 :: Integer -> Integer
fact3 n = fact2 n 1
