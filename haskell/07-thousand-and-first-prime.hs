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

getTenThousandAndFirstPrime :: Int
getTenThousandAndFirstPrime =
  last $ take 10001 [x | x <- [1..], isPrime x]
