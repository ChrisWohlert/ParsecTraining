module Class where

type ReturnType = Datatype
type Name = String
type Content = String
type Attribute = String
type Abstract = Bool
type Constraints = String
type Value = String
type Extension = Bool

data Region = StartRegion Name | EndRegion deriving (Show, Eq)

data GetSet = GetSet String | ArrowGet String deriving (Show, Eq)

data Datatype = Single String | Generic Datatype [Datatype] | List Datatype deriving (Show, Eq)

data Modifier = Override | Virtual deriving (Show, Eq)

type BaseClass = Datatype

data Readonly = Readonly | Mutable deriving (Show, Eq)

data Static = Static | NonStatic deriving (Show, Eq)

data Visibility = Protected | Private | Public | Internal | Unset deriving (Show, Eq)

data ClassName = ClassName Name | GenericClassName Name String deriving (Show, Eq)

data MethodName = MethodName Name | GenericMethodName Name String deriving (Show, Eq)

data Ref = Ref | Out | NoRef deriving (Show, Eq)

data Params = Params | NoParams deriving (Show, Eq)

data Parameter = Parameter Ref Params Datatype Name (Maybe Value) Extension deriving (Show, Eq)

data OperatorOverload = Implicit | Explicit | Unary ReturnType deriving (Show, Eq)

data MethodSignature = MethodSignature Visibility Static (Maybe Modifier) ReturnType MethodName [Parameter] Constraints [Attribute] deriving (Show, Eq)

data Method = Concrete MethodSignature Content 
            | Abstract MethodSignature
            | Interface MethodSignature
            | External MethodSignature
            | ArrowFunction MethodSignature Content
            | OperatorOverload OperatorOverload Visibility MethodName [Parameter] Constraints [Attribute] Content 
            deriving (Show, Eq)

data CtorCall = CtorCall String Content deriving (Show, Eq)

data PropertyName = PropertyName Name | MultiName [Name] deriving (Show, Eq)

data Member = Property (Maybe Modifier) Datatype PropertyName (Maybe GetSet) Value Visibility Readonly Static [Attribute]
            | Constructor Visibility [Parameter] (Maybe CtorCall) Content
            | StaticConstructor [Parameter] Content
            | Desctructor [Parameter] Content
            | Method Method
            | InnerType Type
            | Event Visibility Datatype Name
            | Region Region
            deriving (Show, Eq)

data Safe = Safe | Unsafe deriving (Show, Eq)

data Type = Class { class_usings :: [String]
                  , class_namespace :: String
                  , class_visibility :: Visibility
                  , class_safe :: Safe
                  , class_abstract :: Abstract
                  , class_isInterface :: Bool
                  , class_name :: ClassName
                  , class_baseClasses :: [BaseClass]
                  , class_constraints :: Constraints
                  , class_members :: [Member]
                  , class_attributes :: [Attribute]
                  } |
            Enum { enum_usings :: [String]
                 , enum_namespace :: String
                 , enum_visibility :: Visibility
                 , enum_name :: Name
                 , elements :: [String]
                 , enum_attributes :: [Attribute]
                 }
            deriving (Show, Eq)

data Solution = Solution [Type]

class HasDatatypes a where
     getDatatypes :: a -> [Datatype]

instance HasDatatypes Type where
     getDatatypes (Class _ _ _ _ _ _ _ _ _ members _) = concat $ map getDatatypes members
     getDatatypes _ = []

instance HasDatatypes Parameter where
     getDatatypes (Parameter _ _ d _ _ _) = [d]

instance HasDatatypes MethodSignature where
     getDatatypes (MethodSignature _ _ _ returnType _ parameters _ _) =  returnType : (concat $ map getDatatypes parameters)

instance HasDatatypes Method where
     getDatatypes (Concrete methodSignature _) =  getDatatypes methodSignature
     getDatatypes (Abstract methodSignature) =  getDatatypes methodSignature
     getDatatypes (Interface methodSignature) =  getDatatypes methodSignature
     getDatatypes (External methodSignature) =  getDatatypes methodSignature
     getDatatypes (ArrowFunction methodSignature _) =  getDatatypes methodSignature
     getDatatypes (OperatorOverload _ _ _ parameters _ _ _) = concat $ map getDatatypes parameters

instance HasDatatypes Member where
     getDatatypes (Property _ d _ _ _ _ _ _ _) = [d]
     getDatatypes (Constructor _ parameters _ _) = concat $ map getDatatypes parameters
     getDatatypes (StaticConstructor parameters _) = concat $ map getDatatypes parameters
     getDatatypes (Desctructor parameters _) = concat $ map getDatatypes parameters
     getDatatypes (Method method) = getDatatypes method