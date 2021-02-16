module Parse where
  import Control.Applicative
  import Data.Char
  
  newtype Parser a = P (String -> [(a, String)])
  
  instance Functor Parser where
  --fmap : (a -> b) -> Parser a -> Parser b
    fmap g p = P(\inp -> case parse p inp of
                  []          -> []
                --[(a, State)] -> [(b, State)] s.t. State = String
                  [(v, out)]  -> [(g v, out)])
  
  instance Applicative Parser where
  --pure :: a -> Parser a
    pure v = P(\inp -> [(v, inp)])
  --(<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P(\inp -> case parse pg inp of
                      []        -> []
                      [(g, out)]-> parse (fmap g px) out)
  
  instance Monad Parser where
  --(>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P(\inp -> case parse p inp of
                  [] -> []
                  [(v, out)] -> parse (f v) out) 
            
  instance Alternative Parser where
  --empty :: Parser a
    empty = P(const [])
  -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P(\inp -> case parse p inp of
                [] -> parse q inp
                [(v, out)] -> [(v, out)])
  
  digit :: Parser Char 
  digit = sat isDigit
  
  lower :: Parser Char 
  lower = sat isLower 
  
  upper :: Parser Char
  upper = sat isUpper
  
  letter :: Parser Char 
  letter = sat isAlpha
  
  alphaNum :: Parser Char 
  alphaNum = sat isAlphaNum
  
  sat :: (Char -> Bool) -> Parser Char
  sat p = do
            x <- item
            if p x then return x else empty
  
  parse :: Parser a -> String -> [(a,String)]
  parse (P p) = p 
  
  item :: Parser Char 
  item = P(\inp -> case inp of
              []      -> []
              (x:xs)  -> [(x,xs)])
              
  char :: Char -> Parser Char 
  char x = sat (== x)
  
  -- recognizes a string (or a piece of it)
  string :: String -> Parser String
  string []     = return []
  string (x:xs) = do
                    char x -- check if x is first in the string
                    string xs --va avanti
                    return (x:xs) 
  
  ident :: Parser String
  ident = do 
            x  <- lower
            xs <- many alphaNum
            return (x:xs)
  
  nat :: Parser Int 
  nat = do
          xs <- some digit
          return (read xs)

  space :: Parser ()
  space = do
            many (sat isSpace)
            return()
  
  int :: Parser Int 
  int = do
          char '-'
          n <- nat
          return (-n) 
          <|> nat
  
  token :: Parser a -> Parser a 
  token p = do
              space
              v <- p
              space
              return v
  
  identifier :: Parser String
  identifier = token ident
  
  natural :: Parser Int 
  natural = token nat
  
  integer :: Parser Int 
  integer = token int
  
  symbol :: String -> Parser String
  symbol = token . string
  
  prova :: String -> Parser Int
  prova xs = do return (read xs)
  
  character :: Char -> Parser Char
  character a = token (char a)
                
  expr :: Parser Int 
  expr =  do 
            t <- term
            (do character '+'
                e <- expr
                return (t + e)
              <|> return t)
  
  term :: Parser Int 
  term = do 
            f <- factor
            (do character '*'
                t <- term
                return (f * t)
              <|> return f)
  
  factor :: Parser Int 
  factor = do character '('
              e <- expr
              character ')'
              return e
           <|> natural
  
  eval :: String -> Int 
  eval xs = case parse expr xs of
    [(n, [])]  -> n
    [(_, out)] -> error ("Unused input" ++ out)
    []         -> error "Invalid input"
  
  nats :: Parser [Int]
  nats = do
          symbol "["
          n <- natural
          ns <- many numbers
          symbol "]"
          return (n:ns)
    where
      numbers = do
                  symbol ","
                  natural
  
  relop :: Parser String
  relop = do symbol "==" 
             <|> do symbol "~="
             <|> do symbol ">"
             <|> do symbol ">="
             <|> do symbol "<"
             <|> do symbol "<="
  
  arithop :: Parser Char
  arithop = do character '+'
               <|> do character '-'
               <|> do character '*'
               <|> do character '/'
  
  boolop :: Parser Char 
  boolop = do character '&'
              <|> do character '|'
  
  operators :: Parser String
  operators = do relop
                 <|> do op <- arithop
                        return [op]
                 <|> do op <- boolop
                        return [op]