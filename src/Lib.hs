module Lib
    ( run_parse
    , run_parseClass
    , parseProperty
    , test
    , getFilesFromDir
    , dirTest
    ) where
        
import Text.ParserCombinators.Parsec
import qualified Class as C
import System.IO
import System.Directory
import System.Path
import Control.Monad

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
    datatype <- parseDatatype <* trim
    name <- manyTill legalName $ char ';' <|> space <|> char '='
    value <- trim >> (try (char '=' >> try space >> manyTill anyChar newline) <|> return "") <* trim
    return $ C.Property datatype name value vis readonly static

parseParameters = sepBy (do
    datatype <- parseDatatype <* trim
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

exists :: (GenParser Char st a) -> GenParser Char st Bool
exists rule = (rule >> return True) <|> return False

trim :: GenParser Char st String
trim = many $ oneOf " \n{}"

legalName = alphaNum <|> char '_'

parseClass :: GenParser Char st C.Class
parseClass = do
    us <- usings <* trim
    ns <- namespace <* trim    
    visibility <- trim >> parseVisibility
    className <- space >> string "class" >> space >> parseClassName
    baseClasses <- try parseBaseClasses <* trim
    members <- parseMembers
    return $ C.Class us ns visibility className baseClasses members

removeComments :: GenParser Char st String
removeComments = do 
    withoutComments <- manyTill (removeSimpleComment <|> (eof >> return [])) $ try eof
    return $ concat withoutComments

removeSimpleComment = manyTill anyChar $ (try $ string "//") <* manyTill anyChar newline <|> (try $ string "/*") <* manyTill anyChar (try $ string "*/") <|> (eof >> return [])

run_parseClass :: String -> Either ParseError C.Class
run_parseClass contents = case run_parse removeComments contents of
    Right text -> run_parse parseClass text
    Left err -> Left err


test :: IO ()
test = do
    dir <- getFilesFromDir "C:/Users/CWO/source/github/ParsecTraining"
    print dir
    content <- mapM getContent dir
    print "asd"

getContent :: String -> IO (Either ParseError C.Class)
getContent file = do
    handle <- openFile file ReadMode  
    contents <- hGetContents handle
    run_parseClass contents
    hClose handle

parseFile :: String -> Either ParseError C.Class
parseFile file = do
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    hClose handle
    run_parseClass contents

getFilesFromDir :: FilePath -> IO [String]
getFilesFromDir p = do
    exists <- doesPathExist p
    if exists then do
        dir <- listDirectory p
        let folders = map ((p ++ "/") ++) . map (++ "/") . filter (not . elem '.') $ dir
        let files = map ((p ++ "/") ++) . filter (not . null) . filter isCsFile $ dir
        filesFromFolders <- mapM getFilesFromDir folders
        return $ files ++ (concat filesFromFolders)
    else
        return []

dirTest = listDirectory

isCsFile :: String -> Bool
isCsFile = (".cs" ==) . dropWhile (/= '.')