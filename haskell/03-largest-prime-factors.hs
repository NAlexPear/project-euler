import Data.Maybe

isPrime :: Int -> Bool
isPrime num =
    let
      comparator =
        (== 0) . mod num

      limit =
        ceiling . sqrt $ fromIntegral num
    in
       all comparator [2..limit]


getFactors :: Int -> [Int]
getFactors num =
  [x | x <- [2..(num - 1)], mod num x == 0]


getPrimeFactors :: Int -> [Int]
getPrimeFactors num =
  case factors of
    [] -> [num]
    _  -> factors ++ getPrimeFactors (div num $ head factors)
  where factors = take 1 $ getFactors num 
  

getSmallestPrimeFactor :: Int -> [Int] 
getSmallestPrimeFactor num =
  take 1 $ getPrimeFactors num


getLargestPrimeFactor :: Int -> Int
getLargestPrimeFactor num =
  let
    smallestPrime =
      head $ getSmallestPrimeFactor num

    limit =
      div num smallestPrime

    set =
      takeWhile (limit >) $ getPrimeFactors num
  in
    case set of
      [] -> limit
      _ -> last set
