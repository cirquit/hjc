module ASTParser where

import AST

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Text.Megaparsec
import Lexer

testParser = miniJavaParser

miniJavaParser :: Parser MiniJava
miniJavaParser = MiniJava <$> mainClassP <*> many usualClassP

mainClassP :: Parser Class
mainClassP = do
    sc
    symbol "class"
    id <- identifier
    (vars, main, methods) <-
        braces $ (,,) <$> many varDeclarationP <*> mainMethodP <*> many methodP
    return $ Class id "Object" vars (main : methods)

usualClassP :: Parser Class
usualClassP = do
    symbol "class"
    id <- identifier
    extends <- option "Object" $ symbol "extends" *> identifier
    (vars, methods) <- braces $ (,) <$> many varDeclarationP <*> many methodP
    return $ Class id extends vars methods

-- | Method Parser
--
mainMethodP :: Parser Method
mainMethodP = do
    symbol "public"
    symbol "static"
    typ <- pure VoidT <* symbol "void"
    id <- symbol "main"
    vars <- parens $ variableP `sepBy` comma
    stms <- braces $ many statementP
    return $ Method id typ vars stms

methodP :: Parser Method
methodP = do
    symbol "public"
    typ <- typeP
    id <- identifier
    vars <- parens $ variableP `sepBy` comma
    stms <- braces $ many statementP
    return $ Method id typ vars stms

-- | Statement Parser
--
statementP :: Parser Statement
statementP = try ifP <|> whileP <|> try printLnP <|> printP <|> stmExpP <|> blockStmP

ifP :: Parser Statement
ifP = do
    symbol "if"
    ifexp <- parens expressionP
    stms <- (braces $ many statementP) <|> (:[]) <$> statementP
    elseexp <-
        optional $
        symbol "else" *> (braces (many statementP) <|> ((:[]) <$> statementP))
    return $ If ifexp stms elseexp

whileP :: Parser Statement
whileP = do
    symbol "while"
    ifexp <- parens expressionP
    stms <- braces $ many statementP
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
primaryP = 
    litBoolP <|> try litVarP <|> litIntP <|> litIdentP <|> thisP <|> try intArrP <|>
    try strArrP <|>
    blockExpP <|>
    returnP <|>
    newObjP <|>
    litStrP

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

litStrP :: Parser Expression
litStrP = LitStr <$> stringLiteral

thisP :: Parser Expression
thisP = symbol "this" *> return This

litIdentP :: Parser Expression
litIdentP = LitIdent <$> identifier

intArrP :: Parser Expression
intArrP = IntArr <$> (symbol "new" *> symbol "int" *> brackets expressionP)

strArrP :: Parser Expression
strArrP = StrArr <$> (symbol "new" *> symbol "String" *> brackets expressionP)

newObjP :: Parser Expression
newObjP = do
    symbol "new"
    id <- identifier
    args <- parens (expressionP `sepBy` comma)
    return $ NewObject id args

boolP :: Parser Bool
boolP = symbol "true" *> return True <|> symbol "false" *> return False



opTable =
    [ [ Postfix (flip IndexGet <$> brackets expressionP)
      , Postfix $ foldr1 (flip (.)) <$> some (methodGetP <|> memberGetP)
      ]
    , [Prefix (unaryOps [("!", NOT)])]
    , [InfixL (binaryOps [("*", MUL), ("/", DIV), ("%", MOD)])]
    , [InfixL (binaryOps [("+", PLUS), ("-", MINUS)])]
    , [InfixL (binaryOps [("<=", LEQ), ("<", LE), (">=", GEQ), (">", GE)])]
    , [InfixL (binaryOps [("==", EQS), ("!=", NEQS)])]
    , [InfixL (binaryOps [("&&", AND)])]
    , [InfixL (binaryOps [("||", OR)])]
    , [InfixR (Assign <$ symbol "=")]
    ]

methodGetP :: Parser (Expression -> Expression)
methodGetP = try $ do
    dot
    id <- identifier
    exps <- parens (expressionP `sepBy` comma)
    return (\obj -> MethodGet obj id exps)

memberGetP :: Parser (Expression -> Expression)
memberGetP = do 
    dot
    id <- identifier
    return (\e -> MemberGet e id)

unaryOps :: [(String, UnaryOp)] -> Parser (Expression -> Expression)
unaryOps ops =
    foldr1 (<|>) $ map (\(s, op) -> (\e1 -> UnOp op e1) <$ try (symbol s)) ops

binaryOps :: [(String, BinaryOp)]
          -> Parser (Expression -> Expression -> Expression)
binaryOps ops =
    foldr1 (<|>) $
    map (\(s, op) -> (\e1 e2 -> BinOp e1 op e2) <$ try (symbol s)) ops

-- | Variable Parser
--
variableP :: Parser Variable
variableP = Variable <$> typeP <*> identifier

varDeclarationP :: Parser Variable
varDeclarationP = variableP <* semi

typeP :: Parser Type
typeP =
    try (symbol "int[] " *> return IntArrT) <|>
    try (symbol "int [] " *> return IntArrT) <|>
    symbol "String[] " *> return StringArrT <|>
    symbol "String [] " *> return StringArrT <|>
    symbol "String " *> return StringT <|>
    symbol "int " *> return IntT <|>
    symbol "boolean " *> return BoolT <|>
    symbol "void " *> return VoidT <|>
    IdT <$> identifier
