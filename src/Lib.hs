module Lib
    ( training
    , parseUsing
    ) where

import qualified Text.Parsec as Parsec
import Control.Applicative
import Control.Monad.Identity (Identity)
import Class

training :: String -> String
training text = 
    case Parsec.parse (Parsec.many Parsec.letter) "(source)" text of
        Left err -> "Error"
        Right a -> a

parseUsing :: Parsec.Parsec String () String
parseUsing = Parsec.string "Using" >> Parsec.space >> Parsec.manyTill Parsec.letter (Parsec.char ';')