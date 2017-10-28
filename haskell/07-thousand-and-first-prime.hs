import Helpers(isPrime)


getTenThousandAndFirstPrime :: Int
getTenThousandAndFirstPrime =
  last $ take 10001 [x | x <- [1..], isPrime x]
