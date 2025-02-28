module Pie where

sumd :: Int -> Double
sumd 0 = 1
sumd n = 1 / (fromIntegral n * 2 + 1)

qpie :: Int -> Double
qpie i = 4 * sum [((-1) ** fromIntegral n) / (2 * fromIntegral n + 1) | n <- [0..i]]