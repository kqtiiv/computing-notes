module Parser where

import Types
import Lexer
import Examples

import Data.Maybe

------------------------------------------------------------------------------
-- Given...

showToken :: Token -> String
showToken (Ident v) = v
showToken (Nat v) = show v
showToken WhileTok = "while"
showToken t = [head [c | (c, t') <- tokenTable, t == t']]

printParse :: String -> IO ()
printParse input = either printError printOK (parse input)
  where
    printOK prog = putStrLn "Parse successful..." >> print prog
    printError err = putStr "Parse error: " >> printError' err
    printError'' t s = putStrLn (s ++ " expected, but " ++
                                 maybe "nothing" showToken t ++ " found")
    printError' (BadChar c) = do putStr "Unrecognised character: "
                                 putStrLn [c]
    printError' (Unexpected t t') = printError'' t (showToken t')
    printError' (StmtNotFound t) = printError'' t "Statement"
    printError' (ExprNotFound t) = printError'' t "Expression"
    printError' (IntNotFound t) = printError'' t "Integer literal"
    printError' (UnparsedInput toks) = putStrLn ("Unparsed input: " ++
                                                 unwords (map showToken toks))

------------------------------------------------------------------------------

-- Given...
mHead :: [a] -> Maybe a
mHead (x : _) = Just x
mHead _ = Nothing

checkTok :: Token -> [Token] -> Either Error [Token]
checkTok t ts = case mHead ts of 
  Just x -> 
    if x==t then Right (tail ts) else Left (Unexpected (Just x) t)
  _ -> Left (Unexpected Nothing t)

--parseAtom :: [Token]-> Either Error ([Token], Expr)
parseAtom :: Parser Expr
parseAtom ((Ident x):ts) = Right (ts, Var x)
parseAtom ((Nat x):ts) = Right (ts, Val x) 
parseAtom ((Minus):(Nat x):ts) = Right (ts, Val (negate x))
parseAtom ((Minus):ts) = Left (IntNotFound (mHead ts))
parseAtom (LParen:ts) = do 
  (rem, atom) <- parseExpr ts 
  rem' <- checkTok RParen rem
  pure (rem', atom)
parseAtom ts = Left (ExprNotFound (mHead ts))

-- original

-- parseBody :: (Parser Expr -> Expr -> Parser Expr) -> Parser Expr -> Parser Expr
-- parseBody internalParser parser ts = do 
--   (toks, t) <- parser ts 
--   internalParser parser t toks

-- parseBody' :: Token -> (Expr -> Expr -> Expr) -> Parser Expr -> Expr -> Parser Expr 
-- parseBody' tok cnstr parser t toks = case checkTok tok toks of
--   Left _ -> Right (toks, t)
--   Right toks' -> do 
--     (toks'', x) <- parser toks'
--     parseBody' tok cnstr parser (cnstr t x) toks'' 

parseTerm :: Parser Expr
parseTerm = parseBody (parseBody' Times Mul) parseAtom

parseExpr :: Parser Expr
parseExpr = parseBody (parseBody' Plus Add) parseTerm

-- new better code 

parseBody :: Parser a -> Parser (a->a->a) -> Parser a 
parseBody parser' parser toks = do 
  (toks', t) <- parser toks
  rest t toks' 
  where 
    rest t toks' = case parser' toks of 
      Left _ -> Right (toks, t) 
      Right (toks', f) -> do 
        (toks'', t') <- parser toks'
        rest (f t t') toks''

parseBody' :: Token -> (Expr -> Expr -> Expr) -> Parser (Expr -> Expr -> Expr)
parseBody' tok f toks = case checkTok tok toks of 
  Left _ -> Right (toks, f)
  Right toks' -> Right (toks', f)

--parseStmt :: [Token]-> Either Error ([Token], Stmt)
parseStmt :: Parser Stmt
parseStmt ((Ident x):ets) = do 
  ts <- checkTok Eq ets
  (ts', exp) <- parseExpr ts 
  pure (ts', Asgn x exp)
parseStmt (WhileTok:ets) = do 
  (ts, exp) <- parseExpr ets 
  ts' <- checkTok LBrace ts 
  (ts'', block) <- parseBlock ts' 
  ts''' <- checkTok RBrace ts'' 
  pure (ts''', While exp block)
parseStmt ts = Left (StmtNotFound (mHead ts))

-- original 
-- parseBlock :: Parser Block
-- parseBlock ts = do 
--   (rem, block) <- parseBlock' [] ts
--   pure (rem, reverse block)
--   where 
--     parseBlock' :: [Stmt] -> Parser Block
--     parseBlock' stmts toks = do 
--       (toks', s) <- parseStmt toks 
--       case checkTok Semi toks' of 
--         Right toks'' -> parseBlock' (s:stmts) toks'' 
--         _ -> pure (toks', (s:stmts))

-- better 
parseBlock :: Parser Block
parseBlock = parseBlock' []
  where 
    parseBlock' :: [Stmt] -> Parser Block
    parseBlock' stmts toks = do 
      (toks', s) <- parseStmt toks 
      case checkTok Semi toks' of 
        Right toks'' -> parseBlock' (s:stmts) toks'' 
        _ -> pure (toks', reverse (s:stmts))

parse :: String -> Either Error Program
parse input = do 
  toks <- tokenise input 
  (rem, block) <- parseBlock toks 
  case rem of 
    [] -> pure block 
    _ -> Left (UnparsedInput rem)