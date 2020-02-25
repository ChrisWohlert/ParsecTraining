module Lib
    ( run_parse
    , run_parseType
    , parseClass
    , parseProperty
    , test
    , getFilesFromDir
    , dirTest
    , parseMethod
    , parseParameters
    , parseMembers
    , parseContent
    , parseConcreteMethod
    , parseAbstractMethod
    , parseConstructor
    , parseEnum
    , parseOperatorOverload
    , parseMember
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
    modifier <- parseModifier <* trim
    readonly <- parseReadonly <* trim
    datatype <- parseDatatype <* trim
    names <- parsePropertyName <* trim
    getset <- parseGetSet <* trim
    value <- (try (char '=' >> try space >> manyTill anyChar (char ';')) <|> return "") <* trim
    return $ C.Property modifier datatype names getset value vis readonly static attrs

parseStatic = (try (string "static") >> return C.Static) <|> return C.NonStatic

parseReadonly = (try (string "readonly") >> return C.Readonly) <|> return C.Mutable

parseSafe = (try (string "unsafe") >> return C.Unsafe) <|> return C.Safe

parseModifier = (try (string "override") >> return (Just C.Override)) <|> (try (string "virtual") >> return (Just C.Virtual)) <|> return Nothing

parseGetSet = try parseNormalGetSet <|> try parseArrowGet <|> return Nothing

parseNormalGetSet = do
    getset <- fromTo '{' '}'
    return $ Just $ C.GetSet getset
    
parseArrowGet = do
    string "=>" <* trim
    get <- manyTill anyChar (char ';')
    return $ Just $ C.ArrowGet get

parseAttributes = many $ fromTo '[' ']' <* trim

parseCtorCall = try (do 
    char ':' >> trim
    name <- many1 letter <* trim
    content <- fromTo '(' ')'
    return $ Just (C.CtorCall name content)) <|> return Nothing

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
        try (string ",") <* trim <|> return ""
        return $ C.Parameter ref params datatype name (if value /= [] then Just value else Nothing) extension) (char ')')
    return parameters

parseRef = (try (string "ref ") >> return C.Ref) <|> (try (string "out ") >> return C.Out) <|> return C.NoRef

parseParams = (try (string "params ") >> return C.Params) <|> return C.NoParams

parseContent = parseContentStart '{' 1

parseContentStart _ 0 = string "}"
parseContentStart c d = do
    content <- fromTo '{' '}'
    return $ "{" ++ content ++ "}"
    
fromTo :: Char -> Char -> GenParser Char st String
fromTo start end = do 
    (_:content) <- fromToDepth start start end 1
    return $ init content

fromToDepth :: Char -> Char -> Char -> Int -> GenParser Char st String
fromToDepth current start end 0 = string [end]
fromToDepth current s e d = do
    start <- char current
    middle <- many $ noneOf [s, e]
    rest <- try (fromToDepth s s e (d + 1)) <|> fromToDepth e s e (d - 1)
    return $ start : middle ++ rest

parseConstructor = do
    vis <- parseVisibility <* trim
    many1 alphaNum
    char '('
    parameters <- parseParameters <* trim
    ctorCall <- parseCtorCall <* trim
    content <- parseContent <* trim
    return $ C.Constructor vis parameters ctorCall content

parseStaticConstructor = do
    string "static" <* trim
    many1 alphaNum <* trim
    char '('
    parameters <- parseParameters <* trim
    content <- parseContent <* trim
    return $ C.StaticConstructor parameters content

parseDesctructor = do
    char '~' <* trim
    many1 legalName <* trim
    char '('
    parameters <- parseParameters <* trim
    content <- parseContent <* trim
    return $ C.Desctructor parameters content


parseMethod = (try parseConcreteMethod <|> try parseAbstractMethod <|> try parseInterfaceMethod <|> try parseOperatorOverload <|> parseExternal) <* trim

parseConcreteMethod = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    static <- parseStatic <* trim
    modifier <- parseModifier <* trim
    returnType <- parseDatatype <* trim
    name <- parseMethodName <* trim
    parameters <- char '(' >> parseParameters <* trim
    constraints <- parseConstraints <* trim
    content <- parseContent <* trim
    return $ C.Method $ C.Concrete (C.MethodSignature vis static modifier returnType name parameters constraints attrs) content

parseAbstractMethod = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    static <- parseStatic <* trim
    abstr <- parseAbstract <* trim
    returnType <- parseDatatype <* trim
    name <- parseMethodName <* trim
    parameters <- char '(' >> parseParameters <* string ";" <* trim
    constraints <- parseConstraints <* trim
    return $ C.Method $ C.Abstract (C.MethodSignature vis static Nothing returnType name parameters constraints attrs)

parseInterfaceMethod = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    returnType <- parseDatatype <* trim
    name <- parseMethodName <* trim
    parameters <- char '(' >> parseParameters <* string ";" <* trim
    constraints <- parseConstraints <* trim
    return $ C.Method $ C.Interface (C.MethodSignature vis C.NonStatic Nothing returnType name parameters constraints attrs)

parseExternal = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    static <- parseStatic <* trim
    string "extern" <* trim
    returnType <- parseDatatype <* trim
    name <- parseMethodName <* trim
    parameters <- char '(' >> parseParameters <* string ";" <* trim
    constraints <- parseConstraints <* trim
    return $ C.Method $ C.External (C.MethodSignature vis C.NonStatic Nothing returnType name parameters constraints attrs)

parseArrowFunction = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    static <- parseStatic <* trim
    modifier <- parseModifier <* trim
    returnType <- parseDatatype <* trim
    name <- parseMethodName <* trim
    parameters <- char '(' >> parseParameters <* trim
    constraints <- parseConstraints <* trim
    string "=>" <* trim
    content <- manyTill anyChar (char ';')
    return $ C.Method $ C.ArrowFunction (C.MethodSignature vis static modifier returnType name parameters constraints attrs) content

parseOperatorOverload = try parseImplicit <|> try parseExplicit <|> parseUnary

parseImplicit = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    string "static" <* trim
    string "implicit operator" <* trim
    name <- parseMethodName <* trim
    parameters <- char '(' >> parseParameters <* trim
    constraints <- parseConstraints <* trim
    content <- parseContent <* trim
    return $ C.Method $ C.OperatorOverload C.Implicit vis name parameters constraints attrs content

parseExplicit = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    string "static" <* trim
    string "explicit operator" <* trim
    name <- parseMethodName <* trim
    parameters <- char '(' >> parseParameters <* trim
    constraints <- parseConstraints <* trim
    content <- parseContent <* trim
    return $ C.Method $ C.OperatorOverload C.Explicit vis name parameters constraints attrs content

parseUnary = do
    attrs <- parseAttributes <* trim
    vis <- parseVisibility <* trim
    string "static" <* trim
    returnType <- parseDatatype <* trim
    string "operator" <* trim
    operator <- manyTill anyChar $ char '('
    parameters <- parseParameters <* trim
    constraints <- parseConstraints <* trim
    content <- parseContent <* trim
    return $ C.Method $ C.OperatorOverload (C.Unary returnType) vis (C.MethodName operator) parameters constraints attrs content

parseEvent = do
    vis <- parseVisibility <* trim
    string "event" <* trim
    datatype <- parseDatatype <* trim
    name <- manyTill anyChar $ char ';'
    return $ C.Event vis datatype name

parseClassAsMember = do
    c <- parseClass [] []
    return $ C.InnerType c

parseMethodName = try parseGenericMethodName <|> parseSimpleMethodName

parseGenericMethodName = do 
    name <- many1 legalName
    generic <- fromTo '<' '>'
    return $ C.GenericMethodName name generic

parseSimpleMethodName = do
    name <- many1 legalName
    return $ C.MethodName name

parseRegion = try parseStartRegion <|> parseEndRegion

parseStartRegion = do 
    string "#region" <* trim
    name <- manyTill anyChar newline
    return $ C.Region (C.StartRegion name)

parseEndRegion = string "#endregion" <* trim >> return (C.Region C.EndRegion)

parseMembers = (try (char '}') >> return []) <|> manyTill parseMember (char '}')

parseMember = (
    (try parseClassAsMember
    <|> try parseMethod 
    <|> try parseConstructor 
    <|> try parseStaticConstructor 
    <|> try parseDesctructor 
    <|> try parseArrowFunction
    <|> try parseEvent
    <|> try parseProperty
    <|> try parseRegion
    ) <* trim)

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
    datatype <- (try parseList <|> try parseGenericDatatype <|> parseSingle) <* trim
    char '[' >> char ']'
    return $ C.List datatype

parseGenericDatatype = do
    datatype <- parseSingle <* trim
    generic <- char '<' >> sepBy parseDatatype (string ", ") <* char '>'
    return $ C.Generic datatype generic

parseAbstract = string "abstract"

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
    files <- getFilesFromDir "D:/haskell/ParsecTraining"
    types <- mapM getContent files
    mapM (print) $ take 1 $ drop 2 types
    print "Done."

getContent :: String -> IO (Either ParseError C.Type)
getContent file = do
    handle <- openFile file ReadMode  
    contents <- hGetContents handle
    let parsed = run_parseType contents file
    return parsed

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