sumSquares :: Int -> Int
sumSquares limit =
  sum $ map (^2) [1..limit]

squareSum :: Int -> Int
squareSum limit =
  (^2) $ sum [1..limit]

sumSquareDifference :: Int -> Int
sumSquareDifference limit =
  squareSum limit - sumSquares limit
