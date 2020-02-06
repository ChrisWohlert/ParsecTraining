module Lib
    ( parseCSV
    , run_parseClass
    ) where
        
import Text.ParserCombinators.Parsec
import qualified Class as C
import System.IO

{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
csvFile :: GenParser Char st [[String]]
csvFile = 
    do result <- many line
       eof
       return result

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line = 
    do result <- cells
       eol                       -- end of line
       return result
       
-- Build up a list of cells.  Try to parse the first cell, then figure out 
-- what ends the cell.
cells :: GenParser Char st [String]
cells = 
    do first <- cellContent
       next <- remainingCells
       return (first : next)

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells :: GenParser Char st [String]
remainingCells =
    (char ',' >> cells)            -- Found comma?  More cells coming
    <|> (return [])                -- No comma?  Return [], no more cells

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
cellContent :: GenParser Char st String
cellContent = 
    many (noneOf ",\n")
       

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input



-- CS PARSER ---------------------------------------------------------------------------------------

run_usings = run_parse usings

run_parse :: (GenParser Char () a) -> String -> Either ParseError a
run_parse rule input = parse rule "(Source)" input

usings = many using

using :: GenParser Char st String
using = do string "using" >> space >> manyTill anyChar (char ';') <* many newline

namespace :: GenParser Char st String
namespace = string "namespace" >> space >> manyTill anyChar ((char '\n') <|> space <|> char '{')

curlyStart :: GenParser Char st String
curlyStart = char '{' >> many newline

parseVisibility :: GenParser Char st C.Visibility
parseVisibility = 
    (try (string "protected") >> return C.Protected) <|> 
    (try (string "private") >> return C.Protected) <|> 
    (string "public" >> return C.Protected)

parseClassName :: GenParser Char st String
parseClassName = manyTill anyChar space

parseBaseClasses :: GenParser Char st [String]
parseBaseClasses = char ':' >> space >> sepBy (manyTill anyChar <* newline ) (char ',')

parseClass :: GenParser Char st C.Class
parseClass = do
    us <- usings
    many newline
    ns <- namespace
    many newline
    curlyStart
    spaces
    visibility <- parseVisibility
    space
    string "class"
    space
    className <- parseClassName
    baseClasses <- try parseBaseClasses
    return $ C.Class us ns visibility className baseClasses []

run_parseClass = run_parse parseClass


test :: IO ()
test = do
    handle <- openFile "../test-data/SomeClass.cs" ReadMode  
    contents <- hGetContents handle
    case run_parseClass contents of
        Left err -> print err
        Right c -> print c
    hClose handle 