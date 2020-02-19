module Class where

type ReturnType = Datatype
type Name = String
type Content = String
type Attribute = String
type Abstract = Bool
type Constraints = String
type Value = String
type Extension = Bool
type GetSet = String

data Datatype = Single String | Generic Datatype String | List Datatype deriving (Show, Eq)

type BaseClass = Datatype

data Readonly = Readonly | Mutable deriving (Show, Eq)

data Static = Static | NonStatic deriving (Show, Eq)

data Visibility = Protected | Private | Public | Internal | Unset deriving (Show, Eq)

data ClassName = ClassName Name | GenericClassName Name String deriving (Show, Eq)

data MethodName = MethodName Name | GenericMethodName Name String deriving (Show, Eq)

data Parameter = Parameter Datatype Name (Maybe Value) Extension deriving (Show, Eq)

data MethodSignature = MethodSignature Visibility Static ReturnType MethodName [Parameter] [Attribute] deriving (Show, Eq)

data Method = Concrete MethodSignature Content | Abstract MethodSignature | Override MethodSignature Content deriving (Show, Eq)

data Member = Property Datatype Name GetSet Value Visibility Readonly Static [Attribute]
            | Constructor Visibility [Parameter] Content
            | Method Method
            deriving (Show, Eq)

data Type = Class { class_usings :: [String]
                  , class_namespace :: String
                  , class_visibility :: Visibility
                  , class_abstract :: Abstract
                  , class_name :: ClassName
                  , class_baseClasses :: [BaseClass]
                  , class_constraints :: Constraints
                  , class_members :: [Member]
                  , class_attributes :: [Attribute]
                  } |
            Enum { enum_usings :: [String]
                 , enum_namespace :: String
                 , enum_visibility :: Visibility
                 , enum_name :: String
                 , elements :: [String]
                 , class_attributes :: [Attribute]
            }
            deriving (Show, Eq)
