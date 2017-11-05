module CowApplicative where

newtype Name = Name String deriving (Eq, Show)
newtype Age = Age Int deriving (Eq, Show)
newtype Weight = Weight Int deriving (Eq, Show)

data Cow = Cow { 
  name :: Name, 
  age :: Age, 
  weight :: Weight 
  } deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = case s of "" -> Nothing; n -> Just (Name n);

mkAge :: Int -> Maybe Age
mkAge n = if n >= 0 then Just (Age n) else Nothing

mkWeight :: Int -> Maybe Weight
mkWeight n = if n >= 0 then Just (Weight n) else Nothing

mkCow :: String -> Int -> Int -> Maybe Cow
mkCow n a w = Cow <$> mkName n <*> mkAge a <*> mkWeight w


