module Lib
    ( run_parseClass
    ) where
        
import Text.ParserCombinators.Parsec
import qualified Class as C
import System.IO

run_usings = run_parse usings

run_parse :: (GenParser Char () a) -> String -> Either ParseError a
run_parse rule input = parse rule "(Source)" input

usings = many using

using :: GenParser Char st String
using = do string "using" >> space >> manyTill anyChar (char ';') <* many newline

namespace :: GenParser Char st String
namespace = string "namespace" >> space >> manyTill anyChar ((char '\n') <|> space <|> char '{')

curlyStart :: GenParser Char st String
curlyStart = char '{' >> trim

curlyEnd = char '}' >> trim

parseVisibility :: GenParser Char st C.Visibility
parseVisibility = 
    (try (string "protected") >> return C.Protected) <|> 
    (try (string "private") >> return C.Protected) <|> 
    (string "public" >> return C.Protected)

parseClassName :: GenParser Char st String
parseClassName = manyTill anyChar space

parseBaseClasses :: GenParser Char st [String]
parseBaseClasses = char ':' >> space >> sepBy (many alphaNum) (string ", ")

parseProperties = many $ do
    vis <- parseVisibility <* trim
    static <- exists (string "static") <* trim
    readonly <- exists (string "readonly") <* trim
    datatype <- many alphaNum <* trim
    name <- manyTill anyChar $ char ';' <|> space <|> char '='
    value <- trim >> ((char '=' >> try space >> manyTill anyChar newline) <|> return "")
    return $ C.Property datatype name value vis readonly static

parseParameters = sepBy (do
    datatype <- many alphaNum <* trim
    name <- many alphaNum <* trim
    return $ C.Parameter datatype name) (string ", ")

parseContent = many (noneOf "{") >> many (noneOf "}")

parseConstructor = do
    vis <- parseVisibility <* trim
    many alphaNum
    char '('
    parameters <- parseParameters
    content <- parseContent
    return $ C.Constructor vis parameters content

exists :: (GenParser Char st a) -> GenParser Char st Bool
exists rule = (rule >> return True) <|> return False

trim :: GenParser Char st String
trim = many $ oneOf " \n{}"

parseClass :: GenParser Char st C.Class
parseClass = do
    us <- usings <* trim
    ns <- namespace <* trim
    visibility <- trim >> parseVisibility
    className <- space >> string "class" >> space >> parseClassName
    baseClasses <- try parseBaseClasses
    properties <- trim >> parseProperties
    ctors <- trim >> many1 parseConstructor
    return $ C.Class us ns visibility className baseClasses properties ctors

run_parseClass = run_parse parseClass


test :: IO ()
test = do
    handle <- openFile "../test-data/SomeClass.cs" ReadMode  
    contents <- hGetContents handle
    case run_parseClass contents of
        Left err -> print err
        Right c -> print c
    hClose handle 