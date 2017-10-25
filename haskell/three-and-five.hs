getMultiplesOfThreeOrFive :: Int -> [Int]
getMultiplesOfThreeOrFive limit =
  [x | x <- [1..limit], mod x 3 == 0 || mod x 5 == 0]

sumMultiples :: Int
sumMultiples =
  sum $ getMultiplesOfThreeOrFive 999
