module CustomEnum where

enumFromTo' :: Enum a => a -> a -> [a]
enumFromTo' a z
  | fromEnum a == fromEnum z = a:[]
  | otherwise = a : enumFromTo' (succ a) z

splitOnSpaces :: String -> [String]
splitOnSpaces str = word : rest
  where word = takeWhile (/= ' ') str
        remaining = dropWhile (/= ' ') str
        rest = if remaining == [] then [] else splitOnSpaces $ tail remaining
