module Chp10Exercises where
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbString "Hello, world!",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr addDbDate []
  where addDbDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
        addDbDate (DbDate dateVal) acc = dateVal:acc
        addDbDate _ acc = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr addDbNumber []
  where addDbNumber :: DatabaseItem -> [Integer] -> [Integer]
        addDbNumber (DbNumber val) acc = val:acc
        addDbNumber _ acc = acc

utcTimeMin :: UTCTime
utcTimeMin = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = (foldr max utcTimeMin) . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = (foldr (+) 0) . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral $ sumDb db) / (fromIntegral $ length $ filterDbNumber db)
