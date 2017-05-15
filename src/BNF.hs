module BNF where


import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Text.Megaparsec
import Lexer


type Identifier = String

data MiniJava = MiniJava
    { _mainClass    :: Class
    , _otherClasses :: [Class]
    }  deriving (Eq)

instance Show MiniJava where
  show (MiniJava mc oc) = concatMap (\x -> show x ++ "\n\n") (mc : oc)

-- class <name> (extends <name>) { [methods]; [attributes] }
data Class = Class
    { _className  :: Identifier
    , _extends    :: Identifier
    , _variables  :: [Variable]
    , _methods    :: [Method]
    } deriving (Show, Eq)


-- public <type> <name> (<argument>) { <body> }
data Method = Method
    { _methodName      :: Identifier
    , _methodRetType   :: Type
    , _methodArguments :: [Variable]
    , _methodBody      :: [Statement]
    } deriving (Show, Eq)

data Statement =
    If       Expression [Statement] (Maybe [Statement])   -- if exp { stm } (else {})
  | While    Expression [Statement]
  | PrintLn  Expression
  | Print    Expression
  | StmExp   Expression
  | BlockStm [Statement]
  deriving (Show, Eq)


data Expression =
    LitBool   Bool
  | LitInt    Integer
  | LitVar    Variable
  | IntArr    Expression
  | LitIdent  Identifier
  | NewObject Identifier  [Expression]
  | IndexGet  Expression  Expression      -- x[expr]
  | MemberGet Expression  Identifier      -- x.var
  | MethodGet Expression  Identifier [Expression] -- x.method(a,b,c)
  | Assign    Expression  Expression        -- [<type>] <name> = <exp>;
  | BinOp   Expression BinaryOp Expression  -- <exp> *,/... <exp>
  | UnOp    UnaryOp    Expression                   -- ! <exp>
  | Length  Expression
  | This
  | BlockExp [Expression]
  | Return    Expression
  deriving (Show, Eq)


data UnaryOp =
    NOT       -- (!)   2
  deriving (Show, Eq)

data BinaryOp =
      MUL     -- (*)   3
    | DIV     -- div   3
    | MOD     -- mod   3
    | PLUS    -- (+)   4
    | MINUS   -- (-)   4
    | LEQ     -- (<=)  5
    | LE      -- (<)   5
    | GEQ     -- (>=)  5
    | GE      -- (>)   5
    | EQS     -- (==)  6
    | NEQS    -- (/=)  6
    | AND     -- (&&)  7
    | OR      -- (||)  8
  deriving (Show, Eq)


data Variable = Variable
    { _variable     :: Type
    , _variableName :: Identifier
    } deriving (Show, Eq)

data Type =
      IntArrT       -- int[]
    | IntT          -- int
    | BoolT         -- boolean
    | IdT   String  -- <string>
    | VoidT         -- void
    | StringT       -- String
    | StringArrT    -- String[]
  deriving (Show, Eq)

testParser = miniJavaParser

miniJavaParser :: Parser MiniJava
miniJavaParser = MiniJava <$> mainClassP
                          <*> many usualClassP

mainClassP :: Parser Class
mainClassP = do
  sc    -- TODO: This is somehow needed for examples/TreeVisitor.java
  symbol "class"
  id <- identifier
  (vars, main, methods) <- braces $ (,,) <$> many varDeclarationP
                                         <*> mainMethodP
                                         <*> many methodP
  return $ Class id "Object" vars (main:methods)

usualClassP :: Parser Class
usualClassP = do
  symbol "class"
  id     <- identifier
  extends <- option "Object" $ symbol "extends" *> identifier
  (vars, methods) <- braces $ (,) <$> many varDeclarationP
                                  <*> many methodP
  return $ Class id extends vars methods


-- | Method Parser
--
mainMethodP :: Parser Method
mainMethodP = do
  symbol "public"
  symbol "static"
  typ <- pure VoidT <* symbol "void"
  id  <- symbol "main"
  vars <- parens $ variableP `sepBy` comma
  stms <- braces $ many statementP
  return $ Method id typ vars stms

methodP :: Parser Method
methodP = do
  symbol "public"
  typ  <- typeP
  id   <- identifier
  vars <- parens $ variableP `sepBy` comma
  stms <- braces $ many statementP
  return $ Method id typ vars stms


-- | Statement Parser
--
statementP :: Parser Statement
statementP = try ifP <|> whileP  <|> try printLnP <|> printP
                     <|> stmExpP <|> blockStmP

ifP :: Parser Statement 
ifP = do
  symbol "if"
  ifexp  <- parens expressionP
  stms <- (braces $ many statementP) <|> many statementP
  elseexp <- optional $ symbol "else" *> (braces (many statementP) <|> (many statementP))
  return $ If ifexp stms elseexp 

whileP :: Parser Statement 
whileP = do
  symbol "while"
  ifexp <- parens expressionP
  stms  <- braces $ many statementP
  return $ While ifexp stms

printLnP :: Parser Statement 
printLnP = do
  symbol "System" *> dot *> symbol "out" *> dot <* symbol "println"
  PrintLn <$> parens expressionP <* semi

printP :: Parser Statement 
printP = do
  symbol "System" *> dot *> symbol "out" *> dot <* symbol "print"
  Print <$> parens ((parens $ symbol "char") *> expressionP) <* semi

stmExpP :: Parser Statement 
stmExpP = StmExp <$> expressionP <* semi

blockStmP :: Parser Statement
blockStmP = BlockStm <$> (braces $ many statementP)

-- | Expression Parser
--
expressionP :: Parser Expression
expressionP = makeExprParser primaryP opTable

primaryP :: Parser Expression
primaryP = litBoolP <|> try litVarP <|> litIntP <|> litIdentP
       <|> thisP    <|> try intArrP <|> newObjP <|> blockExpP
       <|> returnP

returnP :: Parser Expression
returnP = Return <$> (symbol "return" *> expressionP)

blockExpP :: Parser Expression
blockExpP = BlockExp <$> parens (some expressionP)

litVarP :: Parser Expression
litVarP = LitVar <$> variableP

litBoolP :: Parser Expression
litBoolP = LitBool <$> boolP

litIntP :: Parser Expression
litIntP = LitInt <$> integer

thisP :: Parser Expression
thisP = symbol "this" *> return This

litIdentP :: Parser Expression
litIdentP = LitIdent <$> identifier

intArrP :: Parser Expression
intArrP = IntArr <$> (symbol "new" *> symbol "int" *> brackets expressionP)

newObjP :: Parser Expression
newObjP = do
  symbol "new"
  id <- identifier
  args <- parens (expressionP `sepBy` comma)
  return $ NewObject id args

boolP :: Parser Bool
boolP =  symbol "true" *> return True
     <|> symbol "false" *> return False

opTable =
    [
       [ Postfix (flip IndexGet <$> brackets expressionP)
       , Postfix (try $ (\mname args obj -> MethodGet obj mname args)
                     <$ dot <*> identifier <*> parens (expressionP `sepBy` comma))
       , Postfix (flip MemberGet <$ dot <*> identifier)
       ]
    ,  [ Prefix (unaryOps [("!", NOT)]) ]
    ,  [ InfixL (binaryOps [("*", MUL), ("/", DIV), ("%", MOD)]) ]
    ,  [ InfixL (binaryOps [("+", PLUS), ("-", MINUS)] )]
    ,  [ InfixL (binaryOps [("<=", LEQ), ("<", LE), (">=", GEQ), (">", GE)] )]
    ,  [ InfixL (binaryOps [("==", EQS), ("!=", NEQS)] )]
    ,  [ InfixL (binaryOps [("&&", AND)] )]
    ,  [ InfixL (binaryOps [("||", OR)] )]
    ,  [ InfixR (Assign <$ symbol "=")]
    ]

unaryOps :: [(String, UnaryOp)] -> Parser (Expression -> Expression)
unaryOps ops = foldr1 (<|>) $ map (\(s, op) -> (\e1 -> UnOp op e1) <$ try (symbol s)) ops

binaryOps :: [(String, BinaryOp)] -> Parser (Expression -> Expression -> Expression)
binaryOps ops = foldr1 (<|>) $ map (\(s, op) -> (\e1 e2 -> BinOp e1 op e2) <$ try (symbol s)) ops

-- | Variable Parser
--
variableP :: Parser Variable
variableP = Variable <$> typeP <*> identifier

varDeclarationP :: Parser Variable
varDeclarationP = variableP <* semi

typeP :: Parser Type
typeP = do
      try $   symbol "int[]"    *> return IntArrT
  <|>         symbol "String[]" *> return StringArrT
  <|>         symbol "String"   *> return StringT
  <|>         symbol "int"      *> return IntT
  <|>         symbol "boolean"  *> return BoolT
  <|>         symbol "void"     *> return VoidT
  <|>                     IdT  <$> identifier

