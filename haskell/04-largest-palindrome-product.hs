getProducts :: Int -> [Int]
getProducts num =
  map (num *) [num, (num-1)..0]


getLikelyProducts :: [Int]
getLikelyProducts =
  [999,998..900] >>= getProducts


getDigits :: Int -> [Int]
getDigits =
  map (read . return) . show


isPalindromeList :: [Int] -> Bool
isPalindromeList numList =
  numList == reverse numList


isPalindromeProduct :: Int -> Bool
isPalindromeProduct num =
  isPalindromeList $ getDigits num


getLargestPalindromeProduct :: Int
getLargestPalindromeProduct =
  maximum $ filter isPalindromeProduct getLikelyProducts

