module Class where

type Datatype = String
type ClassName = String
type BaseClass = String

data Property = Property { dataType :: Datatype, name :: String, value :: String, prop_visibility :: Visibility, isReadonly :: Bool, isStatic :: Bool } deriving (Show)

data Visibility = Protected | Private | Public deriving (Show)

data Constructor = Constructor Visibility [Parameter] String deriving (Show)

data Parameter = Parameter Datatype String deriving (Show)

data Class = Class { usings :: [String]
                   , namespace :: String
                   , class_visibility :: Visibility
                   , className :: ClassName
                   , baseClasses :: [BaseClass]
                   , props :: [Property]
                   , ctor :: [Constructor]
                   } deriving (Show)