module Ch10_06_Database_Processing where

import Data.Time
import Data.List (sort)

data DatabaseItem = 
    DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 9007
  , DbNumber 9008
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = 
  foldr 
    (\dbItem acc ->
      case dbItem of
        DbDate utcTime -> utcTime : acc
        _ -> acc
    ) 
    []  

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = 
  foldr
    (\dbItem acc ->
      case dbItem of
        DbNumber n -> n : acc
        _ -> acc
    )
    []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = head . reverse . sort . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sum xs) / fromIntegral (length xs)
  where xs = filterDbNumber db