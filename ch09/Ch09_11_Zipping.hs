module Ch09_11_Zipping where

myZip :: [a] -> [b] -> [(a, b)]
myZip xs ys =
  case (xs, ys) of
    ([], _) -> []
    (_, []) -> []
    (x:xs, y:ys) -> (x,y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys =
  case (xs, ys) of
    ([], _) -> []
    (_, []) -> []
    (x:xs, y:ys) -> f x y : myZipWith f xs ys

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)