{-
 - Canonization of intermediate code
 -}
module Cmm.Canon(
--  cmm2canon,
--  cmm2canons,
  canPrg,
  canMethod
  ) where

import Cmm.LabelGenerator
import Cmm.CAST

-- cmm2canon :: Cmm -> IO Cmm
-- cmm2canon cmm = runNameGenT $ canPrg cmm

-- cmm2canons :: Cmm -> IO String
-- cmm2canons cmm = cmm2str <$> cmm2canon cmm

-- | Type for canonized statments
-- This is just a type of lists (in reverse order). We do not use standard
-- lists to avoid possible errors due to inverted ordering
data CanonStm = Empty | Then CanonStm CmmStm

append :: CanonStm -> CanonStm -> CanonStm
append xs Empty = xs
append xs (ys `Then` y) = append xs ys `Then` y

asList :: CanonStm -> [CmmStm]
asList = reverse . asListRev
  where
    asListRev Empty = []
    asListRev (xs `Then` x) = x : asListRev xs

-- | Type for canonized expressions
type CanonExp = (CanonStm, CmmExp)

isConstant :: CmmExp -> Bool
isConstant (CONST _) = True
isConstant (NAME _ ) = True
isConstant _ = False

extend :: (MonadNameGen m) => CanonExp -> CanonStm -> m CanonExp
extend ce Empty = return ce
extend (cs, ce) ss =
  -- This can be optimized: it uses temps more often than needed.
  if isConstant ce then
    return (append cs ss, ce)
  else do
    t <- nextTemp'
    return (append (cs `Then` MOVE (TEMP t) ce) ss, TEMP t)

join :: (MonadNameGen m) => CanonExp -> (CanonStm, a) -> m (CanonStm, (CmmExp, a))
join ce1 (s2, e2) = do
  (s1', e1') <- extend ce1 s2
  return (s1', (e1', e2))

joinList :: (MonadNameGen m) => [CanonExp] -> m (CanonStm, [CmmExp])
joinList [] = return (Empty, [])
joinList (e : es) = do
  jes <- joinList es
  (s, (e, je)) <- join e jes
  return (s, e : je)

lift :: (MonadNameGen m) => (a -> b) -> (CanonStm, a) -> m (CanonStm, b)
lift f (s, e) = return (s, f e)

lift2 :: (MonadNameGen m) => (CmmExp -> a -> b) -> CanonExp -> (CanonStm, a) -> m (CanonStm, b)
lift2 f ce1 ce2 = join ce1 ce2 >>= lift (uncurry f)

-- canonization of expression without CALL in the final expression
canExp' :: (MonadNameGen m) => CmmExp -> m CanonExp
canExp' e =
  do (ss, e') <- canExp e
     case e' of
       CALL _ _ -> do t <- nextTemp'
                      return (ss `Then` MOVE (TEMP t) e', TEMP t)
       _ -> return (ss, e')

-- | Canonization of expressions (may contain one toplevel CALL)
canExp :: (MonadNameGen m) => CmmExp -> m CanonExp

canExp e@(CONST _) = return (Empty, e)

canExp e@(NAME _) = return (Empty, e)

canExp e@(TEMP _) = return (Empty, e)

canExp e@(PARAM _) = return (Empty, e)

canExp (BINOP o e1 e2) =
  do ce1 <- canExp' e1
     ce2 <- canExp' e2
     lift2 (BINOP o) ce1 ce2

canExp (MEM e) =
  do ce1 <- canExp' e
     lift MEM ce1

canExp (CALL f es) =
  do ce <- canExp' f
     ses <- mapM canExp' es >>= joinList
     lift2 CALL ce ses

canExp (ESEQ s e) =
  do cs <- canStm s
     (cse, e') <- canExp e
     return (append cs cse, e')

-- | Canonization of statements
canStm :: (MonadNameGen m) => CmmStm -> m CanonStm

canStm (MOVE (MEM e1) e2) =
  do ce1 <- canExp' e1
     ce2 <- canExp e2
     (ss, s) <- lift2 (\e1' e2' -> MOVE (MEM e1') e2')  ce1 ce2
     return $ ss `Then` s

canStm (MOVE (TEMP t) e2) =
  do ce2 <- canExp e2
     (ss, s) <- lift (MOVE (TEMP t)) ce2
     return $ ss `Then` s

canStm (MOVE (PARAM i) e2) =
  do ce2 <- canExp e2
     (ss, s) <- lift (MOVE (PARAM i)) ce2
     return $ ss `Then` s

canStm (MOVE (ESEQ s1 e1) e2) =
  do cs1 <- canStm s1
     cs2 <- canStm (MOVE e1 e2)
     return $ append cs1 cs2

canStm (JUMP e ls) =
  do ce <- canExp' e
     (ss, s) <- lift (`JUMP` ls) ce
     return $ ss `Then` s

canStm (CJUMP o e1 e2 l1 l2) =
  do ce1 <- canExp' e1
     ce2 <- canExp' e2
     (ss, s) <- lift2 (\e1 e2 -> CJUMP o e1 e2 l1 l2) ce1 ce2
     return $ ss `Then` s

canStm (SEQ []) =
  return Empty

canStm (SEQ (s1 : ss)) =
  do cs1 <- canStm s1
     css <- canStm (SEQ ss)
     return (append cs1 css)

canStm (LABEL l) = return (Empty `Then` LABEL l)

-- | Canonization of methods
--
canMethod :: (MonadNameGen m) => CmmMethod -> m CmmMethod
canMethod m = do
    body' <- mapM canStm (cmmBody m)
    return $ m { cmmBody = concatMap asList body' }

-- | Canonization of programs
--
canPrg :: (MonadNameGen m) => Cmm -> m Cmm
canPrg p = mapM canMethod p
