module Lib
    ( run_parse
    , run_parseClass
    , parseClass
    , parseProperty
    , test
    , getFilesFromDir
    , dirTest
    , parseAbstractMethod
    , parseMethod
    , parseParameters
    , parseMembers
    , parseContent
    , parseConcreteMethod
    , parseConstructor
    ) where
        
import Text.ParserCombinators.Parsec
import qualified Class as C
import System.IO
import System.Directory
import Control.Monad

run_parse :: (GenParser Char () a) -> String -> String -> Either ParseError a
run_parse rule input source = parse rule source input

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

parseClassName :: GenParser Char st C.ClassName
parseClassName = try parseGenericClassName <|> parseSimpleClassName

parseGenericClassName = do 
    name <- many alphaNum
    generic <- fromTo '<' '>'
    return $ C.GenericClassName name generic

parseSimpleClassName = do
    name <- many alphaNum
    return $ C.ClassName name

parseBaseClasses :: GenParser Char st [String]
parseBaseClasses =  try (char ':' >> space >> sepBy (many alphaNum) (string ", ")) <|> return []

parseProperty = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    static <- (string "static" >> return C.Static) <|> return C.NonStatic
    trim
    readonly <- (string "readonly" >> return C.Readonly) <|> return C.Mutable
    trim
    datatype <- parseDatatype <* trim
    name <- manyTill legalName $ char ';' <|> space <|> char '='
    value <- trim >> (try (char '=' >> try space >> manyTill anyChar newline) <|> return "") <* trim
    return $ C.Property datatype name value vis readonly static attrs

parseAttributes = many $ fromTo '[' ']'

parseParameters = do
    parameters <- manyTill (do
        datatype <- parseDatatype <* trim
        name <- many1 alphaNum <* trim
        try (string ", ") <|> return ""
        return $ C.Parameter datatype name) (char ')')
    return parameters

parseContent d = parseContentStart

parseContentStart = do
    start <- char '{'
    middle <- many $ noneOf "{}"

    

parseConstructor = do
    vis <- parseVisibility <* trim
    many alphaNum
    char '('
    parameters <- parseParameters <* trim
    content <- parseContent 0 <* trim
    return $ C.Constructor vis parameters content

parseMethod = try parseConcreteMethod <|> parseAbstractMethod

parseConcreteMethod = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    returnType <- parseDatatype <* trim
    name <- parseMethodName <* trim
    parameters <- char '(' >> parseParameters <* trim
    content <- parseContent 0 <* trim
    return $ C.Method $ C.Concrete (C.MethodSignature vis returnType name parameters attrs) content

parseAbstractMethod = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    abstr <- parseAbstract <* trim
    returnType <- parseDatatype <* trim
    name <- parseMethodName <* trim
    parameters <- char '(' >> parseParameters <* string ";" <* trim
    return $ C.Method $ C.Abstract (C.MethodSignature vis returnType name parameters attrs)

parseMethodName = try parseGenericMethodName <|> parseSimpleMethodName

parseGenericMethodName = do 
    name <- many alphaNum
    generic <- fromTo '<' '>'
    return $ C.GenericMethodName name generic

parseSimpleMethodName = do
    name <- many alphaNum
    return $ C.MethodName name

parseMembers = many1 ((try parseMethod <|> try parseConstructor <|> try parseProperty) <* trim)

parseDatatype :: GenParser Char st C.Datatype
parseDatatype = try parseList <|> try parseArray <|> try parseGenericDatatype <|> parseSingle

parseSingle = do
    datatype <- (try parseAbstract >> fail "abstract is not a datatype") <|> many (noneOf " =\n<>[")
    return $ C.Single datatype

parseList = do
    string "List<"
    datatype <- parseSingle <* char '>'
    return $ C.List datatype

parseArray = do
    datatype <- parseSingle <* trim
    char '[' >> char ']'
    return $ C.List datatype

parseGenericDatatype = do
    datatype <- parseSingle <* trim
    generic <- fromTo '<' '>'
    return $ C.Generic datatype generic

parseAbstract = string "abstract"

parseConstraints = string "where" >> trim >> manyTill anyChar (char '\n')

exists :: (GenParser Char st a) -> GenParser Char st Bool
exists rule = (try rule >> return True) <|> return False

trim :: GenParser Char st String
trim = many $ oneOf " \n"

fromTo :: Char -> Char -> GenParser Char st String
fromTo start end = (char start) >> many (noneOf [end]) <* char end

legalName = alphaNum <|> char '_'

parseClass :: GenParser Char st C.Class
parseClass = do
    removeBom
    us <- usings <* trim
    ns <- namespace <* trim
    char '{' <* trim
    attrs <- parseAttributes <* trim
    visibility <- trim >> parseVisibility <* trim
    abstract <- exists parseAbstract <* trim
    className <- string "class" >> trim >> parseClassName <* trim
    baseClasses <- try parseBaseClasses <* trim
    constraints <- parseConstraints <* trim
    char '{' <* trim
    members <- parseMembers
    return $ C.Class us ns visibility abstract className baseClasses constraints members attrs

removeBom = many $ oneOf "\180\9559\9488"

removeComments :: GenParser Char st String
removeComments = do 
    withoutComments <- manyTill (removeSimpleComment <|> (eof >> return [])) $ try eof
    return $ concat withoutComments

removeSimpleComment = manyTill anyChar $ (try $ string "//") <* manyTill anyChar newline <|> (try $ string "/*") <* manyTill anyChar (try $ string "*/") <|> (eof >> return [])

run_parseClass :: String -> String -> Either ParseError C.Class
run_parseClass contents source = case run_parse removeComments contents source of
    Right text -> run_parse parseClass text source
    Left err -> Left err

test :: IO ()
test = do
    files <- getFilesFromDir "D:/haskell/ParsecTraining"
    mapM getContent $ take 1 files
    print "Done."

getContent :: String -> IO ()
getContent file = do
    handle <- openFile file ReadMode  
    contents <- hGetContents handle
    case run_parseClass contents file of
        Left err -> print err
        Right c -> print c
    hClose handle

getFilesFromDir :: FilePath -> IO [String]
getFilesFromDir p = do
    dir <- listDirectory p
    folders <- filterM doesPathExist . map ((p ++ "/") ++) . map (++ "/") $ dir
    let files = map ((p ++ "/") ++) . filter (not . null) . filter isCsFile $ dir
    filesFromFolders <- mapM getFilesFromDir folders
    return $ files ++ (concat filesFromFolders)

dirTest = listDirectory

isCsFile :: String -> Bool
isCsFile = (".cs" ==) . dropWhile (/= '.')