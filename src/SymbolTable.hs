module SymbolTable where

import qualified Data.Map      as Map
import           Data.List               (find)
import           Control.Lens
import           Debug.Trace (trace)

import           AST

type MiniJavaTable = Map.Map Identifier ClassSymbols
type MethodTable   = Map.Map Identifier MethodSymbols

data ClassSymbols = ClassSymbols
    {   _extendsSym :: Maybe Identifier
      , _classType  :: Type
      , _varSymbols :: [Variable]
      , _metSymbols :: MethodTable
    } deriving (Eq, Show)

data MethodSymbols = MethodSymbols
    {   _returnType  :: Type
      , _arguments   :: [Variable]
    } deriving (Eq, Show)

-- | lens boilerplate
--
extendsSym :: Lens' ClassSymbols (Maybe Identifier)
extendsSym = lens _extendsSym (\x y -> x { _extendsSym = y })

classType :: Lens' ClassSymbols Type
classType = lens _classType (\x y -> x { _classType = y })

varSymbols :: Lens' ClassSymbols [Variable]
varSymbols = lens _varSymbols (\x y -> x { _varSymbols = y })

metSymbols :: Lens' ClassSymbols MethodTable 
metSymbols = lens _metSymbols (\x y -> x { _metSymbols = y })

returnType :: Lens' MethodSymbols Type
returnType = lens _returnType (\x y -> x { _returnType = y })

arguments :: Lens' MethodSymbols [Variable]
arguments = lens _arguments (\x y -> x { _arguments = y })

-- | object and all PODs are the default classes
--
defaultMJTable :: MiniJavaTable
defaultMJTable = foldl (\m (id,cls) -> Map.insert id cls m) Map.empty defaultClasses
    where
        defaultClasses = 
           [ (objectIdentifier, objectClass)
           , (intIdentifier   , podClass IntT)
           , (strIdentifier   , podClass StringT)
           , (boolIdentifier  , podClass BoolT)
           , (intArrIdentifier, podArrayClass IntArrT)
           , (strArrIdentifier, podArrayClass StringArrT)
           ]

createSymbolTable :: MiniJava -> MiniJavaTable
createSymbolTable (MiniJava mClass oClasses) = foldl updateTableByClass defaultMJTable (mClass:oClasses)

updateTableByClass :: MiniJavaTable -> Class -> MiniJavaTable
updateTableByClass mjt cl = Map.insert (_className cl) (classToSymbols cl) mjt

classToSymbols :: Class -> ClassSymbols
classToSymbols (Class id extends vars mets) = ClassSymbols
     {    _extendsSym = Just extends
        , _classType  = IdT id 
        , _varSymbols = vars
        , _metSymbols = foldl updateTableByMethod Map.empty mets
     }

updateTableByMethod :: MethodTable -> Method -> MethodTable
updateTableByMethod mt m = Map.insert (_methodName m) (metToSymbols m) mt

metToSymbols :: Method -> MethodSymbols
metToSymbols (Method _ retType args body) = MethodSymbols
    {
        _returnType  = retType
      , _arguments   = args
    }

-- | utility functions
--
classExistsIn :: Identifier -> MiniJavaTable -> Bool
classExistsIn id table =
    case Map.lookup id table of
        (Just _) -> True
        _        -> False

lookupType :: Identifier -> MiniJavaTable -> Maybe Type
lookupType id table = _classType <$> Map.lookup id table

lookupClassSymbols :: Identifier -> MiniJavaTable -> Maybe ClassSymbols
lookupClassSymbols id table = Map.lookup id table

lookupMethodSymbols :: Identifier -> ClassSymbols -> Maybe MethodSymbols
lookupMethodSymbols id cls = Map.lookup id (view metSymbols cls)


-- | reverse the variable list to get the "newest" definition of a variable
--
getGlobalMemberType :: Identifier -> ClassSymbols -> Maybe Type
getGlobalMemberType id cls = _type <$> (find (\v -> _variableName v == id) . reverse $ view varSymbols cls)

-- | default classes
--
objectIdentifier :: Identifier
objectIdentifier = showJC objectType

objectClass :: ClassSymbols
objectClass = ClassSymbols
    {   _extendsSym = Nothing
      , _classType  = objectType
      , _varSymbols = []
      , _metSymbols = Map.empty
    }

-- | PODs
--
intIdentifier :: Identifier
intIdentifier = showJC IntT

intArrIdentifier :: Identifier
intArrIdentifier = showJC IntArrT

boolIdentifier :: Identifier
boolIdentifier = showJC BoolT

strIdentifier :: Identifier
strIdentifier = showJC StringT

strArrIdentifier :: Identifier
strArrIdentifier = showJC StringArrT

-- | every plain old datatype extends the object-class
--
podClass :: Type -> ClassSymbols
podClass t = ClassSymbols
    {   _extendsSym = Nothing -- Just objectIdentifier
      , _classType = t
      , _varSymbols = []
      , _metSymbols = Map.empty
    }

podArrayClass :: Type -> ClassSymbols
podArrayClass t = ClassSymbols
    {
        _extendsSym = Nothing -- Just objectIdentifier
      , _classType = t
      , _varSymbols = [ Variable IntT "length" ]
      , _metSymbols = Map.empty
    }

