module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import System.Environment
import Control.Monad
import Control.Monad.Error
import Numeric (readHex, readOct, readFloat)
import Text.Read.Lex (readIntP)
import Data.Char (ord)
import Data.Ratio (Ratio, (%))
import Data.Complex (Complex(..))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                     Left err -> throwError $ Parser err
                     Right val -> return val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Complex (Complex Double)
             | Rational (Ratio Integer)
             | Integer Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double

parseRational :: Parser LispVal
parseRational = do
  x1 <- many1 digit
  char '/'
  x2 <- many1 digit
  return $ Rational ((read x1) % (read x2))

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Integer n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do
  x1 <- (try parseFloat <|> parseDecimal)
  char '+'
  x2 <- (try parseFloat <|> parseDecimal)
  char 'i'
  return $ Complex ((toDouble x1) :+ (toDouble x2))

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

parseDecimal :: Parser LispVal
parseDecimal = (optional(string "#d") >> many1 digit)
    >>= (return . Integer . read)

parseInteger :: Parser LispVal
parseInteger = try (string("#") >> (parseHex <|> parseBin <|> parseOct))
                <|> parseDecimal where
              parseHex = ((string "x") >> many1 hexDigit)
                >>= (return . Integer . fst . head . readHex)
              parseOct = ((string "o") >> many1 octDigit)
                >>= (return . Integer . fst . head . readOct)
              parseBin = ((string "b") >> many1 binDigit)
                >>= (return . Integer . fst . head . readBin)

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
            <|> try parseComplex
            <|> try parseRational
            <|> try parseFloat
            <|> parseInteger
            <|> parseCharacter
            <|> parseQuoted
            <|> parseQuasiquote
            <|> parseUnQuote
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

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

-- parseArray :: Parser LispVal
-- parseArray = 

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Integer contents) = show contents
showVal (Float contents) = show contents
showVal (Rational contents) = show contents
showVal (Complex contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#t"
showVal (Character char) = [char]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++
                                  showVal tail ++ ")"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> ThrowsError LispVal
-- eval val@(String _) = val
-- eval val@(Integer _) = val
-- eval val@(Bool _) = val
-- eval val@(Float _) = val
-- eval val@(Character _) = val
-- eval val@(Rational _) = val
-- eval val@(Complex _) = val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval val@(_) = return val

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Integer . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Integer n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                        ++ " args; found values " 
                                        ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " 
                                        ++ expected
                                        ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError
instance Error LispError where
  noMsg  = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled --getArgs >>= print . eval . readExpr . head
