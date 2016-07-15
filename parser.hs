module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readHex, readOct, readFloat)

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

-- TODO: implement parseBin
parseNumber :: Parser LispVal
parseNumber = parseHex <|> parseOct <|> parseDecimal where -- <|> parseBin where
              parseHex = ((string "#x") >> many1 hexDigit)
                >>= (return . Number . fst . head . readHex)
              parseOct = ((string "#o") >> many1 octDigit)
                >>= (return . Number . fst . head . readOct)
              -- parseBin = ((string "#b") >> many1 (oneOf "01"))
              --   >>= (return . Number . readBin)
              parseDecimal = (optional(string "#d") >> many1 digit)
                >>= (return . Number . read)


-- dipping into the beautiful world of unicode escape sequences
parseCharacter :: Parser LispVal
parseCharacter = parseNul <|> parsePage <|> parseReturn <|> parseRubout
                <|> parseSpace <|> parseTab <|> parseAlarm <|> parseBackspace
                <|> parseLinefeed <|> parseNewline <|> parseVtab
                <|> parseEsc <|> parseDelete 
                <|> parseChar
                        where
                          parseNul       = string("#\\nul") >> (return $ Character '\NUL')
                          parseSpace     = string("#\\space") >> (return $ Character '\SP')
                          parseReturn    = string("#\\return") >> (return $ Character '\CR')
                          parsePage      = string("#\\page")  >> (return $ Character '\FF')
                          parseTab       = string("#\\tab") >> (return $ Character '\BS')
                          parseAlarm     = string("#\\alarm") >> (return $ Character '\BEL')
                          parseBackspace = string("#\\backspace")  >> (return $ Character '\BS')
                          parseLinefeed  = string("#\\linefeed") >> (return $ Character '\n')
                          parseNewline   = string("#\\newline") >> (return $ Character '\r')
                          parseVtab      = string("#\\vtab") >> (return $ Character '\VT')
                          parseEsc       = string("#\\esc") >> (return $ Character '\ESC')
                          parseDelete    = string("#\\delete") >> (return $ Character '\DEL')
                          parseRubout    = string("#\\rubout") >> (return $ Character '\DEL')
                          parseChar      = do
                            string("\\#")
                            x1 <- symbol <|> letter <|> digit
                            return $ Character x1

parseFloat :: Parser LispVal
parseFloat =
    (many(digit) >> char('.') >> many1(digit))
        >>= (return . Float . fst . head . readFloat)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseFloat <|> parseNumber
            <|> parseCharacter


main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
