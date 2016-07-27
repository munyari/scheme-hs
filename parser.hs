{-# LANGUAGE ExistentialQuantification #-}
module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import System.Environment
import Control.Monad
import Control.Monad.Error
import Numeric (readHex, readOct, readFloat)
import Text.Read.Lex (readIntP)
import Data.Char (ord)
import Data.Ratio (Rational, (%), numerator, denominator)
import Data.Complex (Complex(..))
import System.IO
import Data.IORef

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Complex (Complex Double)
             | Rational Rational
             | Integer Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"



unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Integer _) = return val
eval env val@(Bool _) = return val
eval env val@(Float _) = return val
eval env val@(Character _) = return val
eval env val@(Rational _) = return val
eval env val@(Complex _) = return val
eval env val@(Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
          Bool False -> eval env alt
          otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
      then throwError $ NumArgs (num params) args
      else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
          Nothing -> return env
apply (IOFunc func) args = func args

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", numBoolBinop (==)),
              ("string<?", numBoolBinop (<)),
              ("string<=?", numBoolBinop (<=)),
              ("string>=?", numBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackInteger params >>= return . Integer . foldl1 op

unpackInteger :: LispVal -> ThrowsError Integer
unpackInteger (Integer n) = return n
unpackInteger notInteger = throwError $ TypeMismatch "integer" notInteger

unpackRational :: LispVal -> ThrowsError Rational
unpackRational (Rational n) = return n
unpackRational (Integer n) = return (n % 1)
unpackRational notRational = throwError $ TypeMismatch "rational" notRational

unpackFloat :: LispVal -> ThrowsError Double
unpackFloat (Float n) = return n
unpackFloat (Integer n) = return $ fromIntegral n
unpackFloat (Rational n) = return (x / y)
  where x = fromIntegral $ numerator n
        y = fromIntegral $ denominator n
unpackFloat notFloat = throwError $ TypeMismatch "float" notFloat

unpackComplex :: LispVal -> ThrowsError (Complex Double)
unpackComplex (Complex n) = return n
unpackComplex (Float n) = return (n :+ 0.0)
unpackComplex (Rational n) = return (x / y :+ 0.0)
  where x = fromIntegral $ numerator n
        y = fromIntegral $ denominator n
unpackComplex (Integer n) = return (fromIntegral n :+ 0.0)
unpackComplex notComplex = throwError $ TypeMismatch "complex" notComplex

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s)  = return s
unpackStr (Complex s) = return $ show s
unpackStr (Rational s) = return $ show s
unpackStr (Float s) = return $ show s
unpackStr (Integer s) = return $ show s
unpackStr (Bool s)    = return $ show s
unpackStr notString   = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

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


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] ->
  ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackInteger
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]             = return $ List [x1]
cons [x, List xs]              = return $ List $ x : xs
cons [x, DottedList xs xlast]  = return $ DottedList (x : xs) xlast
cons [x1, x2]                  = return $ DottedList [x1] x2
cons badArgList                = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]            = return $ Bool $ arg1 == arg2
eqv [(Complex arg1), (Complex arg2)]      = return $ Bool $ arg1 == arg2
eqv [(Rational arg1), (Rational arg2)]    = return $ Bool $ arg1 == arg2
eqv [(Integer arg1), (Integer arg2)]      = return $ Bool $ arg1 == arg2
eqv [(Float arg1), (Float arg2)]          = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]        = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]            = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]            = return $ Bool $ (length arg1 == length arg2) &&
                                            (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                             Left err -> False
                             Right (Bool val) -> val
eqv [_,_]                                  = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
     `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackInteger, AnyUnpacker unpackStr,
                     AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
      then return ()
      else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
      then setVar envRef var value >> return value
      else liftIO $ do
        valueRef <- newIORef value
        env <- readIORef envRef
        writeIORef envRef ((var, valueRef) : env)
        return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne $ args
