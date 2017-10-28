import Helpers(isPrime)


getPrimeSums :: Int -> Int
getPrimeSums limit =
  (+) 1 $ sum $ filter isPrime [1..limit]
