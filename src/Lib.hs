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
    (try (string "private") >> return C.Private) <|> 
    (string "public" >> return C.Public)

parseClassName :: GenParser Char st String
parseClassName = manyTill anyChar space

parseBaseClasses :: GenParser Char st [String]
parseBaseClasses = char ':' >> space >> sepBy (many alphaNum) (string ", ")

parseProperty = do
    vis <- parseVisibility <* trim
    static <- (string "static" >> return C.Static) <|> return C.NonStatic
    trim
    readonly <- (string "readonly" >> return C.Readonly) <|> return C.Mutable
    trim
    datatype <- many alphaNum <* trim
    name <- manyTill legalName $ char ';' <|> space <|> char '='
    value <- trim >> ((char '=' >> try space >> manyTill anyChar newline) <|> return "") <* trim
    return $ C.Property datatype name value vis readonly static

parseParameters = sepBy (do
    datatype <- many alphaNum <* trim
    name <- many alphaNum <* trim
    return $ C.Parameter datatype name) (string ", ")

parseContent = many (noneOf "{") >> char '{' >> many (noneOf "}")

parseConstructor = do
    vis <- parseVisibility <* trim
    many alphaNum
    char '('
    parameters <- parseParameters
    content <- parseContent <* trim
    return $ C.Constructor vis parameters content

parseMethod = do
    vis <- parseVisibility <* trim
    returnType <- many legalDatatype <* trim
    name <- many legalName
    parameters <- char '(' >> parseParameters
    content <- parseContent <* trim
    return $ C.Method vis returnType name parameters content

parseMembers = many1 $ try parseProperty <|> try parseConstructor <|> try parseMethod

exists :: (GenParser Char st a) -> GenParser Char st Bool
exists rule = (rule >> return True) <|> return False

trim :: GenParser Char st String
trim = many $ oneOf " \n{}"

legalName = alphaNum <|> char '_'

legalDatatype = alphaNum <|> oneOf "<>"

parseClass :: GenParser Char st C.Class
parseClass = do
    us <- usings <* trim
    ns <- namespace <* trim
    visibility <- trim >> parseVisibility
    className <- space >> string "class" >> space >> parseClassName
    baseClasses <- try parseBaseClasses <* trim
    members <- parseMembers
    return $ C.Class us ns visibility className baseClasses members

run_parseClass = run_parse parseClass


test :: IO ()
test = do
    handle <- openFile "../test-data/SomeClass.cs" ReadMode  
    contents <- hGetContents handle
    case run_parseClass contents of
        Left err -> print err
        Right c -> print c
    hClose handle 