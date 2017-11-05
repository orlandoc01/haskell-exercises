module Chp12Exercises where

-- replaceThe :: String -> String
-- replaceThe text = unwords $ map switchThe $ words text
--   where switchThe word = if word == "the" then "a" else word

notThe :: String -> Maybe String
notThe str = if str == "the" then Nothing else Just str

vowels :: [Char]
vowels = ['A', 'E', 'I', 'O', 'U', 'a', 'e', 'i', 'o', 'u']

notVowelWord :: String -> Maybe String
notVowelWord str = if elem (head str) vowels then Nothing else Just str

notVowelChar :: Char -> Maybe Char
notVowelChar char = if elem char vowels then Nothing else Just char

replaceThe :: String -> String
replaceThe text = unwords $ map (switchThe . notThe) $ words text
  where switchThe notThe = case notThe of Nothing -> "a"; (Just str) -> str

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel "" = 0
countTheBeforeVowel str = maybeAdd1 + countTheBeforeVowel updatedStr
  where splitStr = words str
        updatedStr = unwords $ drop 1 splitStr
        firstTwo = take 2 splitStr
        appliedFirstTwo = (notThe $ head firstTwo, notVowelWord $ last firstTwo)
        maybeAdd1 = if appliedFirstTwo == (Nothing, Nothing) then 1 else 0

countVowels :: String -> Integer
countVowels word = foldr addVowels 0 word
  where addVowels char sum = if notVowelChar char == Nothing then sum + 1 else sum

countConsonants :: String -> Integer
countConsonants word = foldr addConsonants 0 word
  where addConsonants char sum = if notVowelChar char == Nothing then sum else sum + 1

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord word = if countVowels word > countConsonants word then Nothing else Just (Word' word)

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat int = if int < 0 then Nothing else Just $ foldr addSucc Zero [1..int]
  where addSucc _ acc = Succ acc

-- Maybe Library
isJust :: Maybe a -> Bool
isJust x = case x of Nothing -> True; (Just a) -> False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee acc f (Just x) = f x
mayybee acc f (Nothing) = acc

fromMaybe :: a -> Maybe a -> a
fromMaybe acc Nothing = acc
fromMaybe acc (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x acc -> case x of Nothing -> acc; (Just a) -> a:acc) []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr mergeMaybes (Just [])
  where mergeMaybes Nothing _ = Nothing
        mergeMaybes _ Nothing = Nothing
        mergeMaybes (Just a) (Just list) = Just (a:list)

-------------- Either Library
lefts' :: [Either a b] -> [a]
lefts' = foldr (\x acc -> case x of (Left a) -> a:acc; (Right _) -> acc) []

rights' :: [Either a b] -> [b]
rights' = foldr (\x acc -> case x of (Left _) -> acc; (Right a) -> a:acc) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr (\x (ls,rs) -> case x of (Left l) -> (l:ls, rs); (Right r) -> (ls, r:rs)) ([], [])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left _) = Nothing
eitherMaybe' f (Right a) = Just $ f a

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a) = f a
either' f g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (\a -> Just $ f a)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = next : myIterate f next
  where next = f a

myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f start = case next of Nothing -> []; (Just (a,b)) -> a : (myUnfoldr f b)
  where next = f start

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x


------------- Binary Tree Exercises
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case next of Nothing -> Leaf; (Just (a, b, c)) -> Node (unfold f a) b (unfold f c)
  where next = f a


treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold buildTillNum 0
  where buildTillNum x = if x == n then Nothing else Just (x + 1, x, x + 1)


