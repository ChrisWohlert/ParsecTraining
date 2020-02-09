module Class where

type Datatype = String
type ClassName = String
type BaseClass = String
type ReturnType = Datatype
type Name = String
type Content = String

data Readonly = Readonly | Mutable deriving (Show)

data Static = Static | NonStatic deriving (Show)

data Visibility = Protected | Private | Public deriving (Show)

data Parameter = Parameter Datatype String deriving (Show)

data Member = Property Datatype String String Visibility Readonly Static
            | Constructor Visibility [Parameter] Content
            | Method Visibility ReturnType Name [Parameter] Content
            deriving (Show)

data Class = Class { usings :: [String]
                   , namespace :: String
                   , visibility :: Visibility
                   , className :: ClassName
                   , baseClasses :: [BaseClass]
                   , members :: [Member]
                   } deriving (Show)