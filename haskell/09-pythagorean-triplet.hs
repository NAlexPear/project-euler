data Triplet = Triplet { a :: Int
                       , b :: Int
                       , c :: Int
                       } deriving (Show)


isPythagorean :: Triplet -> Bool
isPythagorean (Triplet a b c) =
  a^2 + b^2 == c^2


getInputsFromOutput :: Int -> Int -> [Triplet]
getInputsFromOutput target c =
  let
    remainder =
      target - c
    
    limit =
      div remainder 2
  in
    map (\a -> Triplet a (remainder - a) c) [1..limit]


getTriplets :: Int -> [Triplet]
getTriplets target =
  [3..target] >>= getInputsFromOutput target


getPythagoreanTriplets :: Int -> [Triplet]
getPythagoreanTriplets target =
  filter isPythagorean $ getTriplets target


getTripletProduct :: Triplet -> Int
getTripletProduct (Triplet a b c) =
  product [a,b,c]


getSpecialTripletProduct :: Int
getSpecialTripletProduct =
  getTripletProduct $ last $ getPythagoreanTriplets 1000
  
