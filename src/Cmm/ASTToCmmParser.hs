module Cmm.ASTToCmmParser where

import qualified Data.Map                   as Map
import qualified Data.List                  as DL
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.IO.Class
import           Control.Lens
import           Control.Monad                      (when, zipWithM_)

import           AST
import           Cmm.LabelGenerator                 ( Temp, Label, mkLabel, mkNamedTemp, MonadNameGen(..)
                                                    , NameGen, runNameGen, NameGenT, runNameGenT
                                                    )
import           Cmm.CAST
import qualified SymbolTable                as ST

ast2cmms :: MiniJava -> IO String
ast2cmms ast = cmm2str <$> ast2cmm ast

data CmmScope = CmmScope
    { _curClass       :: Maybe Class
    , _curMethod      :: Maybe Method
    , _cmm            :: Cmm
    }

-- | C-- Monad
--
--  Based on the NameGenT so we can use the `nextLabel` with internal state without contaminating the CmmScope
--  Outer Transformer is the StateT which holds the CmmScope
--
--
type CM m a = StateT CmmScope (NameGenT m) a 

-- | main intro function
--
ast2cmm :: MiniJava -> IO Cmm
ast2cmm ast = (view cmm . snd) <$> runNameGenT (runStateT (parseMiniJavaCmm ast) cmmScope)
    where
        -- | running state
        --
        cmmScope :: CmmScope
        cmmScope = CmmScope
            { _curClass       = Nothing
            , _curMethod      = Nothing
            , _cmm            = []
            }

parseMiniJavaCmm :: MiniJava -> CM IO ()
parseMiniJavaCmm (MiniJava mc oc) = do
    mapM_ parseClassCmm oc
    parseMainClassCmm mc

-- | currently a single method is supported
--
parseMainClassCmm :: Class -> CM IO ()
parseMainClassCmm cls = do
    curClass .= Just cls
    m <- withArgLen 0 . withName "Lmain" <$> parseMethodCmm (head $ _methods cls)
    addMethod m

parseClassCmm :: Class -> CM IO ()
parseClassCmm cls = do
    curClass .= Just cls
    return ()

parseMethodCmm :: Method -> CM IO CmmMethod
parseMethodCmm meth = do
    curMethod .= Just meth
    CmmMethod <$> methodNameCmm
              <*> methodArgLength
              <*> methodBodyCmm
              <*> nextTemp

methodNameCmm :: CM IO String
methodNameCmm = do
    (Just cls)  <- view curClass  <$> get
    (Just meth) <- view curMethod <$> get
    return $ (_className cls) ++ '$' : (_methodName meth)

methodArgLength :: CM IO Int
methodArgLength =  do
    (Just meth) <- view curMethod <$> get
    return . length $ _methodArguments meth

methodBodyCmm :: CM IO [CmmStm]
methodBodyCmm = return []

-- | utils
--
withName :: String -> CmmMethod -> CmmMethod
withName name m = m { cmmMethodName = name }

withArgLen :: Int -> CmmMethod -> CmmMethod
withArgLen l m = m { cmmArgLength = l } 


-- | monadic utils
--
addMethod :: CmmMethod -> CM IO ()
addMethod m = do
    cmm %= (++ [m])

-- | defined in LabelGenerator.hs
--
-- | Generates a fresh temporary. The returned temporary is
--   guaranteed to be different from all the ones returned previously
--   and the ones give to 'avoid'.nextTemp :: Monad m => CM m Temp
nextTemp :: CM IO Temp
nextTemp = lift nextTemp' 

-- | Generates a fresh label.
nextLabel :: CM IO Label
nextLabel = lift nextLabel'

-- | Declare that a list of temps must be avoided by 'nextTemp'.
--  'nextTemp' will not return a temp that was passed to 'avoid'.
avoid :: [Temp] -> CM IO ()
avoid = lift . avoid'

-- | boilerplate

curClass :: Lens' CmmScope (Maybe Class) 
curClass = lens _curClass (\x y -> x { _curClass = y })

curMethod :: Lens' CmmScope (Maybe Method) 
curMethod = lens _curMethod (\x y -> x { _curMethod = y })

cmm   :: Lens' CmmScope Cmm
cmm = lens _cmm (\x y -> x { _cmm = y })

io :: MonadIO m => IO a -> m a 
io = liftIO
