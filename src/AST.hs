module AST where

type Identifier = String

data MiniJava = MiniJava
    { _mainClass :: Class
    , _otherClasses :: [Class]
    } deriving (Eq)

instance Show MiniJava where
    show (MiniJava mc oc) = concatMap (\x -> show x ++ "\n\n") (mc : oc)

-- class <name> (extends <name>) { [methods]; [attributes] }
data Class = Class
    { _className :: Identifier
    , _extends :: Identifier
    , _variables :: [Variable]
    , _methods :: [Method]
    } deriving (Show, Eq)

-- public <type> <name> (<argument>) { <body> }
data Method = Method
    { _methodName :: Identifier
    , _methodRetType :: Type
    , _methodArguments :: [Variable]
    , _methodBody :: [Statement]
    } deriving (Show, Eq)

data Statement
    = If Expression
         [Statement]
         (Maybe [Statement]) -- if exp { stm } (else {})
    | While Expression
            [Statement]
    | PrintLn Expression
    | Print Expression
    | StmExp Expression
    | BlockStm [Statement]
    deriving (Show, Eq)

data Expression
    = LitBool Bool
    | LitInt Integer
    | LitVar Variable
    | LitStr String
    | StrArr Expression
    | IntArr Expression
    | LitIdent Identifier
    | NewObject Identifier
                [Expression]
    | IndexGet Expression
               Expression -- x[expr]
    | MemberGet Expression
                Identifier -- x.var
    | MethodGet Expression
                Identifier
                [Expression] -- x.method(a,b,c)
    | Assign Expression
             Expression -- [<type>] <name> = <exp>;
    | BinOp Expression
            BinaryOp
            Expression -- <exp> *,/... <exp>
    | UnOp UnaryOp
           Expression -- ! <exp>
    | Length Expression
    | This
    | BlockExp [Expression]
    | Return Expression
    deriving (Show, Eq)

data UnaryOp =
    NOT -- (!)   2
    deriving (Show, Eq)

data BinaryOp
    = MUL -- (*)   3
    | DIV -- div   3
    | MOD -- mod   3
    | PLUS -- (+)   4
    | MINUS -- (-)   4
    | LEQ -- (<=)  5
    | LE -- (<)   5
    | GEQ -- (>=)  5
    | GE -- (>)   5
    | EQS -- (==)  6
    | NEQS -- (/=)  6
    | AND -- (&&)  7
    | OR -- (||)  8
    deriving (Show, Eq)

data Variable = Variable
    { _variable :: Type
    , _variableName :: Identifier
    } deriving (Show, Eq)

data Type
    = IntArrT -- int[]
    | IntT -- int
    | BoolT -- boolean
    | IdT String -- <string>
    | VoidT -- void
    | StringT -- String
    | StringArrT -- String[]
    deriving (Show, Eq)
