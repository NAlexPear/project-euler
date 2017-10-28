isPrime :: Int -> Bool
isPrime num =
    let
      comparator =
        (== 0) . mod num

      limit =
        ceiling . sqrt $ fromIntegral num

      set =
        [x | x <- [2..limit], comparator x]
    in
      (==) 0 $ length set


getPrimeSums :: Int -> Int
getPrimeSums limit =
  (+) 1 $ sum $ filter isPrime [1..limit]
