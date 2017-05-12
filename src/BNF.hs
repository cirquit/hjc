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
    If      Expression Statement (Maybe Statement)   -- if exp { stm } (else {})
  | While   Expression Statement
  | PrintLn Expression
  | Print   Expression
  | StmExp  Expression
 -- | MulStm  [Statement]                               -- stm; \n stm;
  deriving (Show, Eq)

statementP :: Parser Statement
statementP = ifP <|> whileP <|> try printLnP <|> printP
                 <|> stmExpP -- <|> whileP <|> printLnP
      --- <|> printP <|> blockP <|> stmExpP

ifP :: Parser Statement 
ifP = do
  symbol "if"
  ifexp  <- parens expressionP
  stms <- braces statementP
  elseexp <- optional $ symbol "else" *> braces statementP
  return $ If ifexp stms elseexp 

whileP :: Parser Statement 
whileP = do
  symbol "while"
  ifexp <- parens expressionP
  stms  <- braces statementP
  return $ While ifexp stms

printLnP :: Parser Statement 
printLnP = do
  symbol "System" *> dot *> symbol "out" *> dot <* symbol "println"
  PrintLn <$> parens expressionP <* semi

printP :: Parser Statement 
printP = do
  symbol "System" *> dot *> symbol "out" *> dot <* symbol "print"
  Print <$> parens ((parens $ symbol "char") *> expressionP) <* semi

blockP :: Parser Statement 
blockP = undefined

stmExpP :: Parser Statement 
stmExpP = StmExp <$> expressionP

expressionP :: Parser Expression
expressionP = makeExprParser primaryP opTable 

primaryP :: Parser Expression
primaryP = litBoolP <|> litVarP <|> litIntP 

litVarP :: Parser Expression
litVarP = LitVar <$> varDeclarationP

litBoolP :: Parser Expression
litBoolP = LitBool <$> boolP

litIntP :: Parser Expression
litIntP = LitInt <$> integer

lengthP :: Parser Expression
lengthP = do
  exp <- expressionP
  dot *> symbol "length"
  return $ Length exp

boolP :: Parser Bool
boolP =  symbol "true" *> return True
     <|> symbol "false" *> return False


opTable = [
    [ InfixL (binaryOps [("*", MUL), ("/", DIV), ("%", MOD)]) ]
        ]

data Expression =
    LitBool  Bool
  | LitInt   Integer
  | LitVar   Variable
  | IndexGet Expression                  -- x[expr]
  | NewObject Identifier
  | Assign  Variable Expression          -- <type> <name> = <exp>;
  | BinOp   Expression BinaryOp Expression  -- <exp> *,/... <exp>
  | NotOp   Expression                   -- ! <exp>
  | Length  Expression                   -- <exp> . length
  | This
--  | MemberGet Expression Identifier
--  | VariableGet Identifier
  deriving (Show, Eq)

data BinaryOp =
      MUL     -- (*)   1
    | DIV     -- div   1
    | MOD     -- mod   1
    | PLUS    -- (+)   2
    | MINUS   -- (-)   2
    | LEQ     -- (<=)  3
    | LE      -- (<)   3
    | GEQ     -- (>=)  3
    | GE      -- (>)   3
    | EQ      -- (==)  4
    | NEQ     -- (/=)  4
    | AND     -- (&&)  5
    | OR      -- (||)  6
  deriving (Show, Eq)

binaryOps :: [(String, BinaryOp)] -> Parser (Expression -> Expression -> Expression)
binaryOps ops = foldr1 (<|>) $ map (\(s, op) -> (\e1 e2 -> BinOp e1 op e2) <$ try (symbol s)) ops


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
  symbol "class"
  id <- identifier
  (vars, main, methods) <- braces $ (,,) <$> many varDeclarationP
                                         <*> mainMethodP
                                         <*> many methodP
  return $ Class id vars (main:methods)

usualClassP :: Parser Class
usualClassP = do
  symbol "class"
  id     <- identifier
  (vars, methods) <- braces $ (,) <$> many varDeclarationP
                                  <*> many methodP
  return $ Class id vars methods

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