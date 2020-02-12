module Class where

type ClassName = String
type BaseClass = String
type ReturnType = Datatype
type Name = String
type Content = String

data Datatype = Single String | Generic String Datatype | List Datatype deriving (Show, Eq)

data Readonly = Readonly | Mutable deriving (Show, Eq)

data Static = Static | NonStatic deriving (Show, Eq)

data Visibility = Protected | Private | Public deriving (Show, Eq)

data Parameter = Parameter Datatype String deriving (Show, Eq)

data Member = Property Datatype String String Visibility Readonly Static
            | Constructor Visibility [Parameter] Content
            | Method Visibility ReturnType Name [Parameter] Content
            deriving (Show, Eq)

data Class = Class { usings :: [String]
                   , namespace :: String
                   , visibility :: Visibility
                   , className :: ClassName
                   , baseClasses :: [BaseClass]
                   , members :: [Member]
                   } deriving (Show, Eq)