fibonacci :: Int -> Int
fibonacci x
  | x <= 2 = x
  | otherwise = fibonacci (x - 1) + fibonacci (x - 2)

generateFibonacciSequence :: Int -> [Int]
generateFibonacciSequence maxValue =
  takeWhile (maxValue >) $ map fibonacci [1..]

sumEvens :: [Int] -> Int
sumEvens numbers =
  sum [x | x <- numbers, mod x 2 == 0]

sumToFibLimit :: Int -> Int
sumToFibLimit maxValue =
  sumEvens $ generateFibonacciSequence maxValue
