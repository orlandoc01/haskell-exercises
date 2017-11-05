module Chp11Review where
import Data.Char

-- Incomplete Vignere Cipher
-- upper :: String -> String
-- upper = map toUpper
--
-- vigenereCipher :: String -> String -> String
-- vigenereCipher key message = fst $ foldl encode ("" , 0) message
--   where 
--     encode (encoding, keyIndex) char
--       | isAlpha char = (encoding ++ addChars char keyIndex, keyIndex + 1)
--       | otherwise = (encoding ++ char, keyIndex);
--     addChars char keyIndex = chr $ mod (ord char + ord $ getKeyChar keyIndex) 26 + 65
--     getKeyChar a = (concat $ repeat key) !! a


isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf (x:restX) ys
  | isSubsetOf x ys = isSubsequenceOf restX ys
  | otherwise = False
  where isSubsetOf x ys = foldr (\y acc -> if acc then acc else y == x) False ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = (map toCapitalTuple) . words
  where toCapitalTuple word@(l:rest) = ((toUpper l : rest), word)

capitalizeWord :: String -> String
capitalizeWord (l:rest) = (toUpper l):rest

capitalizeParagraph :: String -> String
capitalizeParagraph = consumeParagraph ""

consumeParagraph :: String -> String -> String
consumeParagraph acc [] = acc
consumeParagraph acc ('.':' ':word) = consumeParagraph (acc ++ ". " ++ fst (grabWord word)) (snd (grabWord word))
  where grabWord word = (capitalizeWord $ takeWhile isNotSpace word, dropWhile isNotSpace word)
        isNotSpace = ((/=) ' ')
consumeParagraph acc (x:xs) = consumeParagraph (acc ++ [x]) xs

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add expr1 expr2) = eval expr1 + eval expr2

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add expr1 expr2) = printExpr expr1 ++ " + " ++ printExpr expr2




