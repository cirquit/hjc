module AST where

import Data.Maybe
import Data.List

type Identifier = String

data MiniJava = MiniJava
    { _mainClass :: Class
    , _otherClasses :: [Class]
    } deriving (Eq)

instance Show MiniJava where
    show (MiniJava mc oc) = concatMap (\x -> show x ++ "\n\n") (mc : oc)

class ShowJava a where
    showJC :: a -> String

instance ShowJava MiniJava where
    showJC (MiniJava mc oc) = "// autogenerated code\n" ++ showJC mc ++ concatMap showJC oc

-- class <name> (extends <name>) { [methods]; [attributes] }
data Class = Class
    { _className :: Identifier
    , _extends :: Identifier
    , _variables :: [Variable]
    , _methods :: [Method]
    } deriving (Eq)

instance Show Class where
    show (Class className extends variables methods) =
        "class " ++ className ++ " extends " ++ extends ++ "{\n"
        ++ concatMap (\x -> "\t" ++ show x ++ ";\n") variables
        ++ concatMap (\x -> "\t" ++ show x ++ "\n") methods
        ++ "}\n"


instance ShowJava Class where
    showJC (Class className extends variables methods) =
        "class " ++ className ++ " extends " ++ extends ++ "{\n"
        ++ concatMap (\x -> "\t" ++ showJC x ++ ";\n") variables
        ++ concatMap (\x -> "\t" ++ showJC x ++ "\n") methods
        ++ "}\n"

-- public <type> <name> (<argument>) { <body> }
data Method = Method
    { _methodName :: Identifier
    , _methodRetType :: Type
    , _methodArguments :: [Variable]
    , _methodBody :: [Statement]
    } deriving (Eq)



instance Show Method where
    show (Method methodName methodRetType methodArguments methodBody) =
        show methodRetType ++ ' ' : methodName
        ++ "( " ++ concat (intersperse "," (map show methodArguments)) ++ ") {\n"
        ++ concatMap (\x -> "\t\t" ++ show x ++ "\n") methodBody
        ++ "\t}"

instance ShowJava Method where
    showJC (Method methodName methodRetType methodArguments methodBody) =
        "public " ++ showJC methodRetType ++ " " ++ methodName
        ++ "( " ++ concat (intersperse "," (map showJC methodArguments)) ++ ") {\n"
        ++ concatMap (\x -> "\t\t" ++ showJC x) methodBody
        ++ "\t}"

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

instance ShowJava Statement where
    showJC (Print x)     = "System.out.print( (char) " ++ (showJC x) ++" );\n"
    showJC (PrintLn x)   = "System.out.println( " ++ (showJC x) ++ " );\n"
    showJC (BlockStm ss) = "\t{ " ++ concatMap (\x -> showJC x ++ ";\n") ss ++ "\t}"
    showJC (StmExp x)    = showJC x ++ ";\n"
    showJC (While x s)   = "while ( " ++ showJC x ++ " ) {\n" ++ concatMap showJC s ++ "\t}\n"
    showJC (If x ss ms)  = "if ( " ++ showJC x ++ " ) {\n" ++ concatMap showJC ss ++ "} " ++ (maybe "\n" mElse ms)
        where mElse xs = "else {\n" ++ concatMap showJC xs ++ "}\n"

data Expression
    = LitBool Bool
    | LitInt Integer
    | LitVar Variable
    | LitStr String
    | StrArr Expression
    | IntArr Expression
    | LitIdent Identifier
    | NewObject Identifier  -- new Id( exps )
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
    | This
    | BlockExp [Expression]
    | Return Expression
    deriving (Show, Eq)

instance ShowJava Expression where
    showJC (LitBool x) = showJC x
    showJC (LitInt x) = showJC x
    showJC (LitVar x) = showJC x
    showJC (StrArr x) = showJC x
    showJC (IntArr x) = showJC x
    showJC (LitIdent x) = x
    showJC (NewObject id xs) = "new " ++ id ++ "( " ++ concat (intersperse "," (map showJC xs)) ++ " )"
    showJC (IndexGet x x') = showJC x ++ "[" ++ showJC x' ++ "]"
    showJC (MemberGet x id) = showJC x ++ "." ++ id
    showJC (MethodGet x id xs) = showJC x ++ "." ++ id ++ "( " ++ concat (intersperse "," (map showJC xs)) ++ " )"
    showJC (Assign x x') = showJC x ++ " = " ++ showJC x'
    showJC (BinOp x b x') = showJC x ++ " " ++ showJC b ++ " " ++ showJC x'
    showJC (UnOp u x) = showJC x
    showJC This = "this"
    showJC (BlockExp xs) = "( " ++ concat (intersperse "," (map showJC xs)) ++ " )"
    showJC (Return x) = "return " ++ showJC x
    showJC (LitStr x) = show x

instance ShowJava Bool where
    showJC True = "true"
    showJC False = "false"

instance ShowJava Integer where
    showJC x = show x

data UnaryOp =
      NOT -- (!)   2
    | UNDEFINEDUOP       -- added to remove overlapping patterns warning in TypeCheck.unify
    deriving (Show, Eq)

instance ShowJava UnaryOp where
    showJC NOT = "!"

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
    | UNDEFINEDBOP       -- added to remove overlapping patterns warning in TypeCheck.unify
    deriving (Show, Eq)

instance ShowJava BinaryOp where
    showJC MUL   = " * "
    showJC DIV   = " / "
    showJC MOD   = " % "
    showJC PLUS  = " + "
    showJC MINUS = " - "
    showJC LEQ   = " <= "
    showJC LE    = " < "
    showJC GEQ   = " >= "
    showJC GE    = " > "
    showJC EQS   = " == "
    showJC NEQS  = " != "
    showJC AND   = " && "
    showJC OR    = " || "

data Variable = Variable
    { _type         :: Type
    , _variableName :: Identifier
    } deriving (Show, Eq)

instance ShowJava Variable where
    showJC (Variable var name) = showJC var ++ " " ++ name

data Type
    = IntArrT -- int[]
    | IntT -- int
    | BoolT -- boolean
    | IdT String -- <string>
    | VoidT -- void
    | StringT -- String
    | StringArrT -- String[]
    deriving (Show, Eq)

instance ShowJava Type where
    showJC IntArrT    = "int[]"
    showJC IntT       = "int"
    showJC BoolT      = "boolean"
    showJC (IdT str)  = str
    showJC VoidT      = "void"
    showJC StringT    = "String"
    showJC StringArrT = "String[]"

objectType :: Type
objectType = IdT "Object"