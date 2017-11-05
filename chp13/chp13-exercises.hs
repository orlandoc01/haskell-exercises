import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, isAlpha)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case isPalindrome line1 of
    True -> putStrLn "It's a palindrome"
    False -> do 
              putStrLn "Nope!"
              exitSuccess

isPalindrome :: String -> Bool
isPalindrome line = chars == reverse chars
  where chars = map toLower $ filter isAlpha line


type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnkown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnkown $ "Name was " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please provide a name: "
  name <- getLine
  putStrLn "Please provide an age: "
  ageStr <- getLine
  let age = (read ageStr :: Integer) in 
    case (mkPerson name age) of
      (Right person) -> putStrLn $ "Yay! Successfully got a person: " ++ show person
      (Left invalid) -> putStrLn $ "An error occurred: " ++ show invalid
