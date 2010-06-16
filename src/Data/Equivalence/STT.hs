--------------------------------------------------------------------------------
-- |
-- Module      : Data.Equivalence.STT
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This is an implementation of Tarjan's Union-Find algorithm (Robert
-- E. Tarjan. "Efficiency of a Good But Not Linear Set Union
-- Algorithm", JACM 22(2), 1975) in order to maintain an equivalence
-- relation. 
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
--
--------------------------------------------------------------------------------

module Data.Equivalence.STT
  ( leastEquiv
  , equate
  , equivalent
  , classDesc
  , Equiv
  ) where

import Control.Monad.ST.Trans
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

newtype Entry s c a = Entry (STRef s (EntryData s c a))
    deriving (Eq)

data EntryData s c a = Node {
      entryParent :: Entry s c a,
      entryValue :: a
    }
                     | Root {
      entryDesc :: c,
      entryWeight :: Int,
      entryValue :: a
    }

data Equiv s c a = Equiv {
      entries :: STRef s (Map a (Entry s c a)),
      singleDesc :: a -> c,
      combDesc :: c -> c -> c
      }

modifySTRef :: (Monad m) => STRef s a -> (a -> a) -> STT s m ()
modifySTRef r f = readSTRef r >>= (writeSTRef r . f)


leastEquiv :: Monad m => (a -> c) -> (c -> c -> c) -> STT s m (Equiv s c a)
leastEquiv mk com = do 
  es <- newSTRef Map.empty
  return Equiv {entries = es, singleDesc = mk, combDesc = com}


-- | /O(1)/. @repr point@ returns the representative point of
-- @point@'s equivalence class or @Nothing$ if it itself is the
-- representative of its class.
--
-- This method performs the path compresssion.
representative' :: Monad m => Entry s c a -> STT s m (Maybe (Entry s c a))
representative' (Entry e) = do
  ed <- readSTRef e
  case ed of
    Root {} -> return Nothing
    Node { entryParent = parent} -> do
      mparent' <- representative' parent
      case mparent' of
        Nothing -> return $ Just parent
        Just parent' -> writeSTRef e ed{entryParent = parent'} >> return (Just parent')


-- | /O(1)/. @repr point@ returns the representative point of
-- @point@'s equivalence class.
--
-- This method performs the path compresssion.
representative :: Monad m => Entry s c a -> STT s m (Entry s c a)
representative entry = do
  mrepr <- representative' entry
  case mrepr of
    Nothing -> return entry
    Just repr -> return repr


getEntry' :: (Monad m, Ord a) => Equiv s c a -> a -> STT s m (Entry s c a)
getEntry' Equiv {entries = mref, singleDesc = mkDesc} val = do
  m <- readSTRef mref
  case Map.lookup val m of
    Nothing -> do
      e <- newSTRef Root
            { entryDesc = mkDesc val,
              entryWeight = 1,
              entryValue = val
            }
      let entry = Entry e
      writeSTRef mref (Map.insert val entry m)
      return entry
    Just entry -> return entry


getEntry :: (Monad m, Ord a) => Equiv s c a -> a -> STT s m (Maybe (Entry s c a))
getEntry Equiv { entries = mref} val = do
  m <- readSTRef mref
  case Map.lookup val m of
    Nothing -> return Nothing
    Just entry -> return $ Just entry

equate :: (Monad m, Ord a) => Equiv s c a -> a -> a -> STT s m ()
equate equiv x y = do
  ex <- getEntry' equiv x
  ey <- getEntry' equiv  y
  equate' equiv ex ey

equate' :: (Monad m, Ord a) => Equiv s c a -> Entry s c a -> Entry s c a -> STT s m ()
equate' Equiv {combDesc = mkDesc} x y = do
  repx@(Entry rx) <- representative x
  repy@(Entry ry) <- representative y
  when (rx /= ry) $ do
    dx@Root{entryWeight = wx, entryDesc = chx, entryValue = vx} <- readSTRef rx
    dy@Root{entryWeight = wy, entryDesc = chy, entryValue = vy} <- readSTRef ry
    if  wx >= wy
      then do
        writeSTRef ry Node {entryParent = repx, entryValue = vy}
        writeSTRef rx dx{entryWeight = wx + wy, entryDesc = mkDesc chx chy}
      else do
       writeSTRef rx Node {entryParent = repy, entryValue = vx}
       writeSTRef ry dy{entryWeight = wx + wy, entryDesc = mkDesc chx chy}

classDesc :: (Monad m, Ord a) => Equiv s c a -> a -> STT s m c
classDesc eq val = do
  mentry <- getEntry eq val
  case mentry of
    Nothing -> return $ singleDesc eq val
    Just entry -> classDesc' entry

classDesc' :: (Monad m) => Entry s c a -> STT s m c
classDesc' entry = do
  Entry e <- representative entry
  liftM entryDesc $ readSTRef e

-- | /O(1)/. Return @True@ if both points belong to the same
-- | equivalence class.
equivalent :: (Monad m, Ord a) => Equiv s c a -> a -> a -> STT s m Bool
equivalent eq v1 v2 = do
  me1 <- getEntry eq v1
  me2 <- getEntry eq v2
  case (me1,me2) of
    (Just e1, Just e2) -> equivalent' e1 e2
    (Nothing, Nothing) -> return $ v1 == v2
    _ -> return False
    

equivalent' :: (Monad m, Ord a) => Entry s c a -> Entry s c a -> STT s m Bool
equivalent' e1 e2 = liftM2 (==) (representative e1) (representative e2)

