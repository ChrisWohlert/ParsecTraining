module Class where

type Datatype = String
type ClassName = String
type BaseClass = String

data Property = Property { dataType :: Datatype, name :: String } deriving (Show)

data Visibility = Protected | Private | Public deriving (Show)

data Class = Class { usings :: [String]
                   , namespace :: String
                   , visibility :: Visibility
                   , className :: ClassName
                   , baseClasses :: [BaseClass]
                   , props :: [Property]
                   } deriving (Show)