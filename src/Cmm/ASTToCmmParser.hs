module Cmm.ASTToCmmParser where

import qualified Data.Map             as Map
import qualified Data.List            as DL
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import           Control.Lens
import           Control.Monad                      (when, zipWithM_)

import           AST
import           Cmm.LabelGenerator
import           Cmm.CAST
import qualified SymbolTable          as ST

ast2cmms :: MiniJava -> IO String
ast2cmms ast = cmm2str <$> ast2cmm ast


data CmmScope = CmmScope
    { _curClass  :: Maybe Class
    , _curMethod :: Maybe Method
    , _cmm       :: Cmm
    }

curClass :: Lens' CmmScope (Maybe Class) 
curClass = lens _curClass (\x y -> x { _curClass = y })

curMethod :: Lens' CmmScope (Maybe Method) 
curMethod = lens _curMethod (\x y -> x { _curMethod = y })

cmm   :: Lens' CmmScope Cmm
cmm = lens _cmm (\x y -> x { _cmm = y })

io :: MonadIO m => IO a -> m a 
io = liftIO


-- | main intro function
--
ast2cmm :: MiniJava -> IO Cmm
ast2cmm ast = (view cmm . snd) <$> runStateT (parseMiniJavaCmm ast) cmmScope
    where
        -- | running state
        --
        cmmScope :: CmmScope
        cmmScope = CmmScope
            { _curClass  = Nothing
            , _curMethod = Nothing
            , _cmm       = Cmm [] 
            }

parseMiniJavaCmm :: MiniJava -> StateT CmmScope IO ()
parseMiniJavaCmm (MiniJava mc oc) = do
    mapM_ parseClassCmm oc
    parseMainClassCmm mc

-- | currently a single method is supported
--
parseMainClassCmm :: Class -> StateT CmmScope IO ()
parseMainClassCmm cls = do
    curClass .= Just cls
    m <- parseMethodCmm (head $ _methods cls)
    let m1 = (m `withName` "Lmain") `withArgLen` 0
    addMethod m1

parseClassCmm :: Class -> StateT CmmScope IO ()
parseClassCmm cls = do
    curClass .= Just cls
    return ()

parseMethodCmm :: Method -> StateT CmmScope IO CmmMethod
parseMethodCmm meth = do
    curMethod .= Just meth
    CmmMethod <$> methodNameCmm
              <*> methodArgLength
              <*> methodBodyCmm
              <*> (return $ mkNamedTemp "mainRet")

methodNameCmm :: StateT CmmScope IO String
methodNameCmm = do
    (Just cls)  <- view curClass  <$> get
    (Just meth) <- view curMethod <$> get
    return $ (_className cls) ++ '$' : (_methodName meth)

methodArgLength :: StateT CmmScope IO Int
methodArgLength =  do
    (Just meth) <- view curMethod <$> get
    return . length $ _methodArguments meth

methodBodyCmm :: StateT CmmScope IO [CmmStm]
methodBodyCmm = return []

-- | utils
--
withName :: CmmMethod -> String -> CmmMethod
withName m name = m { cmmMethodName = name }

withArgLen :: CmmMethod -> Int -> CmmMethod
withArgLen m l = m { cmmArgLength = l } 


-- | monadic utils
--
addMethod :: CmmMethod -> StateT CmmScope IO ()
addMethod m = do
    cmm' <- view cmm <$> get
    let cmm'' = Cmm { cmmMethods = (cmmMethods cmm') ++ [m] }
    cmm .= cmm''
