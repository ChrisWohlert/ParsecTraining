module Lib
    ( run_parse
    , run_parseType
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
    , parseEnum
    ) where
        
import Text.ParserCombinators.Parsec
import qualified Class as C
import System.IO
import System.Directory
import Control.Monad
import System.Exit

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
    (try (string "internal") >> return C.Internal) <|> 
    (try (string "public") >> return C.Public) <|>
    return C.Unset

parseClassName :: GenParser Char st C.ClassName
parseClassName = try parseGenericClassName <|> parseSimpleClassName

parseGenericClassName = do 
    name <- many alphaNum
    generic <- fromTo '<' '>'
    return $ C.GenericClassName name generic

parseSimpleClassName = do
    name <- many alphaNum
    return $ C.ClassName name

parseBaseClasses :: GenParser Char st [C.BaseClass]
parseBaseClasses =  try (char ':' >> space >> sepBy (parseDatatype) (string ", ")) <|> return []

parseProperty = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    static <- parseStatic <* trim
    readonly <- parseReadonly <* trim
    datatype <- parseDatatype <* trim
    names <- parsePropertyName <* trim
    getset <- parseGetSet <* trim
    value <- (try (char '=' >> try space >> manyTill anyChar (char ';')) <|> return "") <* trim
    return $ C.Property datatype names getset value vis readonly static attrs

parseStatic = (try (string "static") >> return C.Static) <|> return C.NonStatic

parseReadonly = (try (string "readonly") >> return C.Readonly) <|> return C.Mutable

parseSafe = (try (string "unsafe") >> return C.Unsafe) <|> return C.Safe

parseGetSet = try parseNormalGetSet <|> try parseArrowGet <|> return Nothing

parseNormalGetSet = do
    getset <- fromTo '{' '}'
    return $ Just $ C.GetSet getset

parseArrowGet = do
    string "=>" <* trim
    get <- manyTill anyChar (char ';')
    return $ Just $ C.ArrowGet get

parseAttributes = many $ fromTo '[' ']'

parseCtorCall = try (do 
    char ':' >> trim
    name <- many1 letter <* trim
    char '('
    names <- sepBy (many (noneOf ",)")) (string ", ")
    char ')'
    return $ Just (C.CtorCall name names)) <|> return Nothing

parsePropertyName = try parseSinglePropertyName <|> parseMultiPropertyName

parseSinglePropertyName = do
    name <- manyTill legalName $ oneOf " ;={\n"
    return $ C.PropertyName name

parseMultiPropertyName = do
    names <- sepBy (many1 legalName) $ string ", "
    oneOf " ;={\n"
    return $ C.MultiName names

parseParameters = do
    parameters <- manyTill (do
        extension <- exists $ string "this " <* trim
        ref <- parseRef
        params <- parseParams
        datatype <- parseDatatype <* trim
        name <- many1 alphaNum <* trim
        value <- try (char '=' >> trim >> many (noneOf ",)")) <|> return ""
        try (string ", ") <|> return ""
        return $ C.Parameter ref params datatype name (if value /= [] then Just value else Nothing) extension) (char ')')
    return parameters

parseRef = (try (string "ref ") >> return C.Ref) <|> (try (string "out ") >> return C.Out) <|> return C.NoRef

parseParams = (try (string "params ") >> return C.Params) <|> return C.NoParams

parseContent = parseContentStart '{' 1

parseContentStart _ 0 = string "}"
parseContentStart c d = do
    start <- char c
    middle <- many $ noneOf "{}"
    content <- try (parseContentStart '{' (d + 1)) <|> parseContentStart '}' (d - 1)
    return $ start : middle ++ content
    
fromTo :: Char -> Char -> GenParser Char st String
fromTo start end = fromToDepth start end 1

fromToDepth :: Char -> Char -> Int -> GenParser Char st String
fromToDepth start end 0 = string [end]
fromToDepth start end d = do --(char start) >> many (noneOf [end]) <* char end
    start <- char start
    middle <- many $ noneOf [start, end]
    rest <- try (fromToDepth start end (d + 1)) <|> fromToDepth end start (d - 1)
    return $ start : middle ++ rest

parseConstructor = do
    vis <- parseVisibility <* trim
    many1 alphaNum
    char '('
    parameters <- parseParameters <* trim
    ctorCall <- parseCtorCall <* trim
    content <- parseContent <* trim
    return $ C.Constructor vis parameters ctorCall content

parseDesctructor = do
    char '~'
    many1 legalName
    char '('
    parameters <- parseParameters <* trim
    content <- parseContent <* trim
    return $ C.Desctructor parameters content


parseMethod = (try parseConcreteMethod <|> try parseOverrideMethod <|> try parseAbstractMethod <|> try parseInterfaceMethod <|> parseExternal) <* trim

parseConcreteMethod = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    static <- parseStatic <* trim
    returnType <- parseDatatype <* trim
    name <- parseMethodName <* trim
    parameters <- char '(' >> parseParameters <* trim
    constraints <- parseConstraints <* trim
    content <- parseContent <* trim
    return $ C.Method $ C.Concrete (C.MethodSignature vis static returnType name parameters constraints attrs) content
    
parseOverrideMethod = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    static <- parseStatic <* trim
    override <- parseOverride <* trim
    returnType <- parseDatatype <* trim
    name <- parseMethodName <* trim
    parameters <- char '(' >> parseParameters <* trim
    constraints <- parseConstraints <* trim
    content <- parseContent <* trim
    return $ C.Method $ C.Override (C.MethodSignature vis static returnType name parameters constraints attrs) content

parseAbstractMethod = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    static <- parseStatic <* trim
    abstr <- parseAbstract <* trim
    returnType <- parseDatatype <* trim
    name <- parseMethodName <* trim
    parameters <- char '(' >> parseParameters <* string ";" <* trim
    constraints <- parseConstraints <* trim
    return $ C.Method $ C.Abstract (C.MethodSignature vis static returnType name parameters constraints attrs)

parseInterfaceMethod = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    returnType <- parseDatatype <* trim
    name <- parseMethodName <* trim
    parameters <- char '(' >> parseParameters <* string ";" <* trim
    constraints <- parseConstraints <* trim
    return $ C.Method $ C.Interface (C.MethodSignature vis C.NonStatic returnType name parameters constraints attrs)

parseExternal = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    static <- parseStatic <* trim
    string "extern" <* trim
    returnType <- parseDatatype <* trim
    name <- parseMethodName <* trim
    parameters <- char '(' >> parseParameters <* string ";" <* trim
    constraints <- parseConstraints <* trim
    return $ C.Method $ C.External (C.MethodSignature vis C.NonStatic returnType name parameters constraints attrs)

parseMethodName = try parseGenericMethodName <|> parseSimpleMethodName

parseGenericMethodName = do 
    name <- many1 legalName
    generic <- fromTo '<' '>'
    return $ C.GenericMethodName name generic

parseSimpleMethodName = do
    name <- many1 legalName
    return $ C.MethodName name

parseMembers = (try (char '}') >> return []) <|> manyTill ((try parseMethod <|> try parseConstructor <|> try parseDesctructor <|> try parseProperty) <* trim) (char '}')

parseDatatype :: GenParser Char st C.Datatype
parseDatatype = try parseList <|> try parseArray <|> try parseGenericDatatype <|> parseSingle

parseSingle = do
    datatype <- (try parseAbstract >> fail "abstract is not a datatype") <|> many1 (noneOf " =\n<>[,")
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
    generic <- char '<' >> sepBy parseDatatype (string ", ") <* char '>'
    return $ C.Generic datatype generic


parseAbstract = string "abstract"

parseOverride = string "override"

parseConstraints = (try (string "where") >> trim >> many1 (noneOf "\n{")) <|> return ""

exists :: (GenParser Char st a) -> GenParser Char st Bool
exists rule = (try rule >> return True) <|> return False

trim :: GenParser Char st String
trim = many $ oneOf " \n\t"

legalName = alphaNum <|> char '_'

parseType = do
    removeBom
    us <- usings <* trim
    ns <- namespace <* trim 
    char '{' <* trim
    try (parseEnum us ns) <|> (parseClass us ns)

parseClass :: [String] -> String -> GenParser Char st C.Type
parseClass us ns = do
    attrs <- parseAttributes <* trim
    visibility <- trim >> parseVisibility <* trim
    safe <- parseSafe <* trim
    static <- parseStatic <* trim
    abstract <- exists parseAbstract <* trim
    isInterface <- ((try (string "class") >> return False) <|> (try (string "interface") >> return True)) <* trim
    className <- parseClassName <* trim
    baseClasses <- try parseBaseClasses <* trim
    constraints <- try (parseConstraints <* trim) <|> return ""
    char '{' <* trim
    members <- parseMembers
    return $ C.Class us ns visibility safe abstract isInterface className baseClasses constraints members attrs

parseEnum us ns = do
    attrs <- parseAttributes <* trim
    visibility <- trim >> parseVisibility <* trim
    string "enum" <* trim
    name <- many1 alphaNum <* trim
    char '{' <* trim
    elements <- parseEnumElements
    return $ C.Enum us ns visibility name elements attrs

parseEnumElements = many1 parseEnumElement

parseEnumElement = many1 alphaNum <* many (noneOf ",}") <* (try (char ',') <|> char '}') <* trim

removeBom = many $ oneOf "\180\9559\9488"

removeComments :: GenParser Char st String
removeComments = do 
    withoutComments <- manyTill (removeSimpleComment <|> (eof >> return [])) $ try eof
    return $ concat withoutComments

removeSimpleComment = manyTill anyChar $ (try $ string "//") <* manyTill anyChar newline <|> (try $ string "/*") <* manyTill anyChar (try $ string "*/") <|> (eof >> return [])

run_parseType :: String -> String -> Either ParseError C.Type
run_parseType contents source = case run_parse removeComments contents source of
    Right text -> run_parse parseType text source
    Left err -> Left err

test :: IO ()
test = do
    files <- getFilesFromDir "C:/Users/CWO/source/github/ParsecTraining"
    mapM getContent files
    print "Done."

getContent :: String -> IO ()
getContent file = do
    handle <- openFile file ReadMode  
    contents <- hGetContents handle
    case run_parseType contents file of
        Left err -> do
            print err
            exitSuccess
        Right c -> do 
            print "."
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