isDivisible :: Int -> Int -> Bool
isDivisible limit num =
  let
    comparator =
      (== 0) . mod num
  in
    all comparator [2..limit]

getSmallestMultiple :: Int -> Int
getSmallestMultiple limit =
    (+) 1 $ last $ takeWhile (not . isDivisible limit) [1..]
  
