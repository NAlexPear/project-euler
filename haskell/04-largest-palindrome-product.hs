getProducts :: Int -> [Int]
getProducts num =
  map (num *) [num, (num-1)..0]


getLikelyProducts :: [Int]
getLikelyProducts =
  [999,998..900] >>= getProducts


getDigits :: Int -> [Int]
getDigits =
  map (read . return) . show


joinDigits :: [Int] -> Int
joinDigits =
  read . concatMap show


isPalindromeList :: [Int] -> Bool
isPalindromeList numList =
  case numList of
    [x]   -> True
    [x,xs] -> x == xs
    (x:xs) -> x == last xs && isPalindromeList (init xs)


isPalindromeProduct :: Int -> Bool
isPalindromeProduct num =
  isPalindromeList $ getDigits num


getLargestPalindromeProduct :: Int
getLargestPalindromeProduct =
  maximum $ filter isPalindromeProduct getLikelyProducts

