module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import System.Environment
import Control.Monad
import Numeric (readHex, readOct, readFloat)
import Text.Read.Lex (readIntP)
import Data.Char (ord)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                     Left err -> String $ "No match: " ++ show err
                     Right val -> val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many ((char '\\' >> (char '"'
                        <|> char 'n'
                        <|> char 't'
                        <|> char 'r'
                        <|> char '\\'))
          <|> noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
           "#t" -> Bool True
           "#f" -> Bool False
           _    -> Atom atom

binDigit :: Parser Char
binDigit = oneOf "01"

parseNumber :: Parser LispVal
parseNumber = try (string("#") >> (parseHex <|> parseBin <|> parseOct))
                <|> parseDecimal where
              parseHex = ((string "x") >> many1 hexDigit)
                >>= (return . Number . fst . head . readHex)
              parseOct = ((string "o") >> many1 octDigit)
                >>= (return . Number . fst . head . readOct)
              parseBin = ((string "b") >> many1 binDigit)
                >>= (return . Number . fst . head . readBin)
              parseDecimal = (optional(string "#d") >> many1 digit)
                >>= (return . Number . read)

readIntP' :: (Eq a, Num a) => a -> ReadP a
readIntP' base = readIntP base isDigit valDigit
  where
    isDigit c = maybe False (const True) (valDig base c)
    valDigit c = maybe 0     id          (valDig base c)

readBinP :: (Eq a, Num a) => ReadP a
readBinP = readIntP' 2

readBin :: (Eq a, Num a) => ReadS a
readBin = readP_to_S readBinP

valDig :: (Eq a, Num a) => a -> Char -> Maybe Int
valDig 2 c
  | '0' == c || '1' == c = Just(ord c - ord '0')
  | otherwise              = Nothing

-- dipping into the beautiful world of unicode escape sequences
parseCharacter :: Parser LispVal
parseCharacter = string("#\\") >>
            (try  (try parseRubout <|> parseReturn <|> parseNul <|> parsePage
                <|> parseSpace <|> parseTab <|> parseAlarm <|> parseBackspace
                <|> parseLinefeed <|> parseNewline <|> parseVtab
                <|> parseEsc <|> parseDelete)
                <|> parseChar)
                        where
                          parseNul       = string("nul")
                              >> (return $ Character '\NUL')
                          parseSpace     = string("space")
                              >> (return $ Character '\SP')
                          parseReturn    = string("return")
                              >> (return $ Character '\CR')
                          parsePage      = string("page")
                              >> (return $ Character '\FF')
                          parseTab       = string("tab")
                              >> (return $ Character '\BS')
                          parseAlarm     = string("alarm")
                              >> (return $ Character '\BEL')
                          parseBackspace = string("backspace")
                              >> (return $ Character '\BS')
                          parseLinefeed  = string("linefeed")
                              >> (return $ Character '\n')
                          parseNewline   = string("newline")
                              >> (return $ Character '\r')
                          parseVtab      = string("vtab")
                              >> (return $ Character '\VT')
                          parseEsc       = string("esc")
                              >> (return $ Character '\ESC')
                          parseDelete    = string("delete")
                              >> (return $ Character '\DEL')
                          parseRubout    = string("rubout")
                              >> (return $ Character '\DEL')
                          parseChar      =
                            (symbol <|> letter <|> digit)
                            >>= (return . Character)

parseFloat :: Parser LispVal
parseFloat = do
    x1 <- many(digit)
    char('.')
    x2 <- many(digit)
    return $ Float (fst . head $ readFloat (x1 ++ "." ++ x2))

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> try (try parseFloat <|> parseNumber)
            <|> parseCharacter
            <|> parseQuoted
            <|> do  char '('
                    x <- try parseList <|> parseDottedList
                    char ')'
                    return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#t"
showVal (Character char) = [char]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++
                                  showVal tail ++ ")"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Float _) = val
eval val@(Character _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                               then 0
                               else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
