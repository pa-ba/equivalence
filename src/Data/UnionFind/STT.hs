-- | An implementation of Tarjan's UNION-FIND algorithm.  (Robert E
-- Tarjan. \"Efficiency of a Good But Not Linear Set Union Algorithm\", JACM
-- 22(2), 1975)
--
-- The algorithm implements three operations efficiently (all amortised
-- @O(1)@):
--
--  1. Check whether two elements are in the same equivalence class.
--
--  2. Create a union of two equivalence classes.
--
--  3. Look up the descriptor of the equivalence class.
-- 
-- The implementation is based on mutable references.  Each
-- equivalence class has exactly one member that serves as its
-- representative element.  Every element either is the representative
-- element of its equivalence class or points to another element in
-- the same equivalence class.  Equivalence testing thus consists of
-- following the pointers to the representative elements and then
-- comparing these for identity.
--
-- The algorithm performs lazy path compression.  That is, whenever we
-- walk along a path greater than length 1 we automatically update the
-- pointers along the path to directly point to the representative
-- element.  Consequently future lookups will be have a path length of
-- at most 1.
--
module Data.UnionFind.STT
  ( emptyPartition
  , equate
  , equivalent
  , equivalenceClass
  , Partition
  )
where

import Control.Monad.ST.Trans
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

newtype Entry s a = Entry (STRef s (EntryData s a))
    deriving (Eq)

data EntryData s a = EntryData {
      entryParent :: Maybe (Entry s a),
      entryClass :: [a],
      entryWeight :: Int,
      entryValue :: a
    }

data Partition s a = Partition {
      entries :: STRef s (Map a (Entry s a))
      }

modifySTRef :: (Monad m) => STRef s a -> (a -> a) -> STT s m ()
modifySTRef r f = readSTRef r >>= (writeSTRef r . f)


emptyPartition :: Monad m => STT s m (Partition s a)
emptyPartition = liftM Partition $ newSTRef Map.empty


-- | /O(1)/. @repr point@ returns the representative point of
-- @point@'s equivalence class or @Nothing$ if it itself is the
-- representative of its class.
--
-- This method performs the path compresssion.
representative' :: Monad m => Entry s a -> STT s m (Maybe (Entry s a))
representative' (Entry e) = do
  ed <- readSTRef e
  case entryParent ed of
    Nothing -> return Nothing
    Just parent -> do
      mparent' <- representative' parent
      case mparent' of
        Nothing -> return $ Just parent
        Just parent' -> writeSTRef e ed{entryParent = Just parent'} >> return (Just parent')


-- | /O(1)/. @repr point@ returns the representative point of
-- @point@'s equivalence class.
--
-- This method performs the path compresssion.
representative :: Monad m => Entry s a -> STT s m (Entry s a)
representative entry = do
  mrepr <- representative' entry
  case mrepr of
    Nothing -> return entry
    Just repr -> return repr


getEntry' :: (Monad m, Ord a) => Partition s a -> a -> STT s m (Entry s a)
getEntry' (Partition mref) val = do
  m <- readSTRef mref
  case Map.lookup val m of
    Nothing -> do
      e <- newSTRef EntryData
            { entryParent = Nothing,
              entryClass = [val],
              entryWeight = 1,
              entryValue = val
            }
      let entry = Entry e
      writeSTRef mref (Map.insert val entry m)
      return entry
    Just entry -> return entry


getEntry :: (Monad m, Ord a) => Partition s a -> a -> STT s m (Maybe (Entry s a))
getEntry (Partition mref) val = do
  m <- readSTRef mref
  case Map.lookup val m of
    Nothing -> return Nothing
    Just entry -> return $ Just entry

equate :: (Monad m, Ord a) => Partition s a -> a -> a -> STT s m ()
equate part x y = do
  ex <- getEntry' part x
  ey <- getEntry' part  y
  equate' ex ey

equate' :: (Monad m, Ord a) => Entry s a -> Entry s a -> STT s m ()
equate' x y = do
  repx@(Entry rx) <- representative x
  repy@(Entry ry) <- representative y
  when (rx /= ry) $ do
    dx@EntryData{entryWeight = wx, entryClass = chx} <- readSTRef rx
    dy@EntryData{entryWeight = wy, entryClass = chy} <- readSTRef ry
    if  wx >= wy
      then do
        writeSTRef ry dy{entryParent = Just repx}
        writeSTRef rx dx{entryWeight = wx + wy, entryClass = chx ++ chy}
      else do
       writeSTRef rx dx{entryParent = Just repy}
       writeSTRef ry dy{entryWeight = wx + wy, entryClass = chx ++ chy}

equivalenceClass :: (Monad m, Ord a) => Partition s a -> a -> STT s m [a]
equivalenceClass p val = do
  mentry <- getEntry p val
  case mentry of
    Nothing -> return [val]
    Just entry -> equivalenceClass' entry

equivalenceClass' :: (Monad m) => Entry s a -> STT s m [a]
equivalenceClass' entry = do
  Entry e <- representative entry
  ed <- readSTRef e
  return $ entryClass ed

-- | /O(1)/. Return @True@ if both points belong to the same
-- | equivalence class.
equivalent :: (Monad m, Ord a) => Partition s a -> a -> a -> STT s m Bool
equivalent p v1 v2 = do
  me1 <- getEntry p v1
  me2 <- getEntry p v2
  case (me1,me2) of
    (Just e1, Just e2) -> equivalent' e1 e2
    (Nothing, Nothing) -> return $ v1 == v2
    _ -> return False
    

equivalent' :: (Monad m, Ord a) => Entry s a -> Entry s a -> STT s m Bool
equivalent' e1 e2 = liftM2 (==) (representative e1) (representative e2)

