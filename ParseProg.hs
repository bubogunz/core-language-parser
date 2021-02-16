module ParseProg where
  import Parse
  import GHC.Base

  type Name = String 
  data Expr a
    = EVar Name               -- Variables
    | ENum Int                -- Numbers
    | EConstr Int Int         -- Constructor tag arity
    | EAp (Expr a) (Expr a)   -- Applications
    | ELet                    -- Let(rec) expressions {in Expr using IsRec you use the constructor ELet for modelling both let and letrec}
        IsRec                 --   boolen with True = recursive
        [(a, Expr a)]         --   Definitions
        (Expr a)              --   Body of let(rec)
    | ECase                   -- Case expressions
        (Expr a)              --   Expression to scrutinise
        [Alter a]             --   Alternatives
    | ELam [a] (Expr a)       -- Lambda abstractions
      deriving Show

  type ScDef a = (Name, [a], Expr a)
  type CoreScDefN = ScDef Name
  type Program a = [ScDef a] -- Program is defined polymorphic but will always be Program Name = list of SuperCombinators Name
  type CoreProgram = Program Name

  type Def a = (a, Expr a)  -- for let(rec)
  type Alter a = (Int, [a], Expr a) -- for case
  data IsRec = NonRecursive | Recursive  deriving (Eq, Show)

  keywords :: [String]
  keywords = ["let", "letrec", "where", "in", "case", "of"]

  parseProg :: Parser (Program Name)
  parseProg = do p <- parseScDef
                 do character ';'
                    ps <- parseProg
                    return (p:ps)
                    <|> return [p]

  parseScDef :: Parser (ScDef Name)
  parseScDef = do v  <- identifier 
                  pf <- many identifier
                  character  '='
                  body <- parseExpr 
                  return (v, pf, body)

  parseExpr :: Parser (Expr Name)
  parseExpr = do parseLet
                 <|> do parseCase
                 <|> do parseLambda
                 <|> do parseExpr1

  parseLet :: Parser (Expr Name)
  parseLet = do rc <- isRec
                defs <- parseDefs
                symbol "in"
                ELet rc defs <$> parseExpr
  
  parseCase :: Parser (Expr Name)
  parseCase = do symbol "case"
                 expr <- parseExpr
                 symbol "of"
                 ECase expr <$> parseAlts

  parseLambda :: Parser (Expr Name)
  parseLambda = do character '\\'
                   vars <- some identifier
                   character '.'
                   ELam vars <$> parseExpr

  parseAExpr :: Parser (Expr Name)
  parseAExpr = do parseVar
                  <|> do parseNum
                  <|> do parseConstr
                  <|> do parsePar

  parseNum :: Parser (Expr Name )
  parseNum = do ENum <$> integer

  parseConstr :: Parser (Expr Name)
  parseConstr = do symbol "Pack"
                   character '{'
                   tag <- natural
                   character ','
                   arity <- natural
                   character '}'
                   return (EConstr tag arity)
  
  parsePar :: Parser (Expr Name)
  parsePar = do character '('
                expr <- parseExpr
                character ')'
                return expr

  parseVar :: Parser (Expr Name)
  parseVar = do var <- identifier
                if var `notElem` keywords
                  then return (EVar var) 
                else empty

  isRec :: Parser IsRec
  isRec = do prefix <- symbol "let"
             do suffix <- symbol "rec"
                return Recursive
                <|> return NonRecursive

  parseDefs :: Parser [Def Name]
  parseDefs = do def <- parseDef
                 do character ';'
                    defs <- parseDefs
                    return (def:defs)
                    <|> return [def]

  parseDef :: Parser (Def Name)
  parseDef = do id <- identifier
                character '=' 
                expr <- parseExpr
                return (id, expr)

  parseAlts :: Parser [Alter Name]
  parseAlts = do alt1 <- parseAlt
                 do character ';'
                    alts <- parseAlts
                    return (alt1:alts)
                    <|> return [alt1]

  parseAlt :: Parser (Alter Name)  
  parseAlt = do character '<'
                n <- natural 
                character '>'
                vars <- many identifier 
                symbol "->"
                expr <- parseExpr
                return (n, vars, expr)

  parseExpr1 :: Parser (Expr Name)  
  parseExpr1 = do expr2 <- parseExpr2
                  do character '|'
                     compose (EVar "|") expr2 <$> parseExpr1
                     <|> return expr2

  parseExpr2 :: Parser (Expr Name)
  parseExpr2 = do expr3 <- parseExpr3
                  do character '&'
                     compose (EVar "&") expr3 <$> parseExpr2
                     <|> return expr3

  parseExpr3 :: Parser (Expr Name)
  parseExpr3 = do expr4 <- parseExpr4
                  rel <- relop
                  compose (EVar rel) expr4 <$> parseExpr4
                  <|> do parseExpr4

  parseExpr4 :: Parser (Expr Name)
  parseExpr4 = do expr5 <- parseExpr5
                  do character '+'
                     compose (EVar "+") expr5 <$> parseExpr4
                     <|> do character '-'
                            compose (EVar "-") expr5 <$> parseExpr5
                   <|> return expr5

  parseExpr5 :: Parser (Expr Name)
  parseExpr5 = do expr6 <- parseExpr6
                  do character '*'
                     compose (EVar "*") expr6 <$> parseExpr5
                     <|> do character '/'
                            compose (EVar "/") expr6 <$> parseExpr6
                   <|> return expr6                      

  parseExpr6 :: Parser (Expr Name)
  parseExpr6 = do exprs <- some parseAExpr
                  return (foldl1 EAp exprs)

  compose :: Expr a -> Expr a -> Expr a -> Expr a
  compose expr1 expr2 = EAp (EAp expr1 expr2)