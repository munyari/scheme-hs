module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                     Left err -> "No match: " ++ show err
                     Right val -> "Found value"

-- FLASH: an algebraic data type is just an enumerated type
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

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

-- FLASH: function composition
parseNumber :: Parser LispVal
parseNumber = (many1 digit) >>= (return . Number . read)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
