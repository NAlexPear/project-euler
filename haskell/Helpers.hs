module Helpers (isPrime) where

isPrime :: Int -> Bool
isPrime num =
    let
      comparator =
        (/= 0) . mod num

      limit =
        ceiling . sqrt $ fromIntegral num
    in
       all comparator [2..limit]



