module V where
import Text.ParserCombinators.Parsec
import Control.Exception hiding (try)
import Monad
import VData
import VPrim
import Prelude hiding (catch)

-- | parseTest symbol "$"
-- '$'
symbol :: Parser Char
symbol = oneOf ".!$%&|*+-/:<=?>@-_~#"

cSlash = '\\'
cQuote = '"'

-- | parseTest escChars  "\\$"
-- "$"
escChars :: Parser String
escChars
  = do char cSlash
       x <- anyChar
       return $ case x of
            't' -> ['\t']
            '\\' -> ['\\']
            '"' -> ['"']
            otherwise -> "<^" ++ [x] ++ ">"

normalChars :: Parser String
normalChars
  = do x <- noneOf ([cQuote, cSlash])
       return [x]

-- |  parseTest parseString  "\"223 \\t  \\   \\\" \\\\ aa\""
-- {"223 \t  <^ >  \" \\ aa"}
parseString :: Parser VVal
parseString
  = do readquote
       x <- many $ escChars <|> normalChars
       readquote
       return $ VStr_ (concat x)
  where readquote = char cQuote

-- | parseTest parseSymbol "aa"
-- $aa
parseSymbol :: Parser VVal
parseSymbol
  = do first <- letter <|> symbol
       rest <- many $ letter <|> digit <|> symbol
       let sym = [first] ++ rest
       return $
         case sym of
             "#t" -> VBool_ True
             "#f" -> VBool_ False
             otherwise -> VSym_ sym

-- |  parseTest parseExpr  "22 \"223 \\t  \\   \\\" \\\\ aa\" 1 2"
-- #22
-- |  parseTest parseExprList  "22 \"223 \\t  \\   \\\" \\\\ aa\" 1 2"
-- [#22,{"223 \t  <^ >  \" \\ aa"},#1,#2]
parseExprList :: Parser [VVal]
parseExprList
  = do skipMany space
       x <- parseExpr `sepEndBy` spaces
       skipMany space
       return $ x

parseQuote
  = do char '['
       skipMany space
       x <- parseExprList
       skipMany space
       char ']'
       return $ VQuote_ x

parseNumber :: Parser VVal
parseNumber
  = do x <- many1 digit
       return $ VInt_ $ read x

parseExpr :: Parser VVal
parseExpr
  = parseNumber <|> parseSymbol <|> parseString <|> parseQuote


-- Reads Expression and returns the result parsed expresson
-- | readExpr "[1 2 + puts]"
--   Right [[[#1,#2,$+,$puts]]]

-- parse tases three arguments 
--      the type of parser
--              of type Parse (result)
--      the input file
--      the string to be parsed.
-- The return type is of (result)
readExpr :: String -> Either ParseError [VVal]
readExpr = parse parseExprList "main.v"


-- VContext contains vval-stack and venv
v :: String -> IO String
v s
  = do result <- case readExpr s of
                      Left err -> return (show err)
                      Right aval -> do w <- walkerV aval (VContext_ [] prim)
                                       case w of
                                            VContext_ str env -> return (show str)
       return result
                         -- VContext_ str env -> (show str) ++ "\n>> " ++ (show env)
vmain :: IO ()
vmain = do v "1 2 +"
           return ()

