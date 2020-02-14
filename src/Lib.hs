module Lib
    ( run_parse
    , run_parseClass
    , parseClass
    , parseProperty
    , test
    , getFilesFromDir
    , dirTest
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
    name <- many letter
    generic <- fromTo '<' '>'
    return $ C.GenericClassName name generic

parseSimpleClassName = do
    name <- many letter
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

parseParameters = sepBy (do
    datatype <- parseDatatype <* trim
    name <- many alphaNum <* trim
    return $ C.Parameter datatype name) (string ", ")

parseContent = many (noneOf "{") >> fromTo '{' '}'

parseConstructor = do
    vis <- parseVisibility <* trim
    many alphaNum
    char '('
    parameters <- parseParameters
    content <- parseContent <* trim
    return $ C.Constructor vis parameters content

parseMethod = do
    vis <- parseVisibility <* trim
    returnType <- parseDatatype <* trim
    name <- many legalName
    parameters <- char '(' >> parseParameters
    content <- parseContent <* trim
    return $ C.Method vis returnType name parameters content

parseMembers = many1 $ try parseProperty <|> try parseConstructor <|> try parseMethod

parseDatatype :: GenParser Char st C.Datatype
parseDatatype = try parseList <|> try parseArray <|> parseSingle

parseSingle = do
    datatype <- many $ noneOf " =\n<>["
    return $ C.Single datatype

parseList = do
    string "List<"
    datatype <- parseSingle <* char '>'
    return $ C.List datatype

parseArray = do
    datatype <- parseSingle <* trim
    char '[' >> char ']'
    return $ C.List datatype

parseAbstract = string "abstract"

parseConstraints = string "where" >> trim >> manyTill anyChar (char '\n')

exists :: (GenParser Char st a) -> GenParser Char st Bool
exists rule = (rule >> return True) <|> return False

trim :: GenParser Char st String
trim = many $ oneOf " \n{}"

fromTo :: Char -> Char -> GenParser Char st String
fromTo start end = (char start) >> many (noneOf [end]) <* char end

legalName = alphaNum <|> char '_'

parseClass :: GenParser Char st C.Class
parseClass = do
    removeBom
    us <- usings <* trim
    ns <- namespace <* trim    
    attrs <- parseAttributes <* trim
    visibility <- trim >> parseVisibility <* trim
    abstract <- exists parseAbstract <* trim
    className <- string "class" >> trim >> parseClassName <* trim
    baseClasses <- try parseBaseClasses <* trim
    constraints <- parseConstraints <* trim
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
    files <- getFilesFromDir "C:/Users/CWO/source/github/ParsecTraining"
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