module Class where

type Datatype = String

data Property = Property { dataType :: Datatype, name :: String }

data Class = Class { usings :: [String]
                   , props :: [Property]
                   }