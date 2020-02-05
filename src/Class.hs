module Class where

type Datatype = String

data Property = Property { dataType :: Datatype, name :: String } deriving (Show)

data Visibility = Protected | Private | Public deriving (Show)

data Class = Class { usings :: [String]
                   , namespace :: String
                   , visibility :: Visibility
                   , props :: [Property]
                   } deriving (Show)