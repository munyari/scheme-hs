module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readHex, readOct, readFloat)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                     Left err -> "No match: " ++ show err
                     Right val -> "Found value " ++ show val

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

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
