import Data.List (sort)
import Lib
import Test.Hspec
import Test.QuickCheck

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = half (2 * x) == x

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_plusAssociative :: Integer -> Integer -> Integer -> Bool
prop_plusAssociative x y z =
  x + (y + z) == (x + y) + z

prop_plusCommutative :: Integer -> Integer -> Bool
prop_plusCommutative x y =
  x + y == y + x

prop_multAssociative :: Integer -> Integer -> Integer -> Bool
prop_multAssociative x y z =
  x * (y * z) == (x * y) * z

prop_multCommutative :: Integer -> Integer -> Bool
prop_multCommutative x y =
  x * y == y * x

prop_quotRem :: Integer -> (NonZero Integer) -> Bool
prop_quotRem x (NonZero y) =
  (quot x y) * y + (rem x y) == x

prop_divMod :: Integer -> (NonZero Integer) -> Bool
prop_divMod x (NonZero y) =
  (div x y) * y + (mod x y) == x

prop_powerAssociative :: Integer -> Integer -> Integer -> Bool
prop_powerAssociative x y z =
  (x ^ y) ^ z == x ^ (y ^ z) 


prop_powerCommutative :: Integer -> Integer -> Bool
prop_powerCommutative x y =
  x ^ y == y ^ x

prop_apply :: (Fun Integer Char) -> Integer -> Bool
prop_apply f a =
  ((applyFun f) $ a) == applyFun f a

prop_composition :: (Fun Integer Char) -> (Fun String Integer) -> String -> Bool
prop_composition f g x = 
  ((applyFun f) . (applyFun g)) x == applyFun f (applyFun g x)

prop_foldrConcat :: String -> String -> Bool
prop_foldrConcat xs ys =
  foldr (:) xs ys == (++) xs ys

prop_foldrConcat2 :: [String] -> Bool
prop_foldrConcat2 xss =
  foldr (++) [] xss == concat xss

prop_legnthOfTake :: (Positive Int) -> [Char] -> Bool
prop_legnthOfTake (Positive n) xs =
  length (take n xs) == n

prop_readShow :: Integer -> Bool
prop_readShow x =  
  (read (show x)) == x

prop_floatingPointArith :: Positive Double -> Bool
prop_floatingPointArith (Positive x) = (sqrt x) ^ 2 == x


main :: IO ()
main = do
  hspec $ do
    describe "half" $ do
      it "2 * half is the identity" $ do
        property $ prop_halfIdentity
    describe "Data.List (sort)" $ do
      it "listOrdered is True for sorted lists" $ do
        property $ (prop_listOrdered :: [Int] -> Bool) . sort
    describe "Plus (+)" $ do
      it "is associative" $ do
        property $ prop_plusAssociative
      it "is commutitive" $ do
        property $ prop_plusCommutative
    describe "Times (*)" $ do
      it "is associative" $ do
        property $ prop_multAssociative
      it "is commutitive" $ do
        property $ prop_multCommutative
    describe "quotRem" $ do
      it "(quot x y) * y + (rem x y) == x" $ do
        property $ prop_multAssociative
    describe "divMod" $ do
      it "(div x y) * y + (mod x y) == x" $ do
        property $ prop_divMod
    -- >>>> Obviously false >>>>
    -- describe "Power (^)" $ do
    --   it "is associative" $ do
    --     property $ prop_powerAssociative
    --   it "is commutitive" $ do
    --     property $ prop_powerCommutative
    describe "reverse" $ do
      it "reversing a list twice is the identity" $ do
        property $ \xs -> (reverse . reverse) xs == (xs :: [Double])
    describe "$" $ do
      it "is the same as function application" $ do
        property $ prop_apply
    describe "function composition (.)" $ do
      it "is the same as applying two functions" $ do
        property $ prop_composition
    describe "foldr" $ do
      -- >>> This is false
      -- it "foldr (:) == (++)" $ do
      --   property $ prop_foldrConcat
      it "foldr (++) [] == concat" $ do
        property $ prop_foldrConcat2
    -- >>>>> probably false for lists shorter than n
    -- describe "length of take" $ do
    --   it "f n xs = length (take n xs) == n" $ do
    --     property $ prop_legnthOfTake
    describe "show me what you read" $ do
      it "goes there and back again" $ do
        property $ prop_readShow
    -- describe "floating point arithmetic is not math" $ do
    --   it "fails to sqrt and then square" $ do
    --     property $ prop_floatingPointArith

    