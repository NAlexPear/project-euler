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


getFactors :: Int -> [Int]
getFactors num =
  [x | x <- [2..(num - 1)], mod num x == 0]


getPrimeFactors :: Int -> [Int]
getPrimeFactors num =
  filter isPrime $ getFactors num


getLargestPrimeFactor =
  maximum . getPrimeFactors
