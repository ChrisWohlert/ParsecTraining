module Class where

type BaseClass = String
type ReturnType = Datatype
type Name = String
type Content = String
type Attribute = String
type Abstract = Bool
type Constraints = String

data Datatype = Single String | Generic Datatype String | List Datatype deriving (Show, Eq)

data Readonly = Readonly | Mutable deriving (Show, Eq)

data Static = Static | NonStatic deriving (Show, Eq)

data Visibility = Protected | Private | Public deriving (Show, Eq)

data ClassName = ClassName Name | GenericClassName Name String deriving (Show, Eq)

data MethodName = MethodName Name | GenericMethodName Name String deriving (Show, Eq)

data Parameter = Parameter Datatype String deriving (Show, Eq)

data MethodSignature = MethodSignature Visibility ReturnType MethodName [Parameter] [Attribute] deriving (Show, Eq)

data Method = Concrete MethodSignature Content | Abstract MethodSignature deriving (Show, Eq)

data Member = Property Datatype String String Visibility Readonly Static [Attribute]
            | Constructor Visibility [Parameter] Content
            | Method Method
            deriving (Show, Eq)

data Class = Class { usings :: [String]
                   , namespace :: String
                   , visibility :: Visibility
                   , abstract :: Abstract
                   , className :: ClassName
                   , baseClasses :: [BaseClass]
                   , constraints :: Constraints
                   , members :: [Member]
                   , attributes :: [Attribute]
                   } deriving (Show, Eq)