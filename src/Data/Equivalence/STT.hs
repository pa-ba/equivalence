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
-- This implementation is a port of the /union-find/ package using the
-- ST monad transformer (instead of the IO monad).
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
-- Each equivalence class remains a descriptor, i.e. some piece of
-- data attached to an equivalence class which is combined when two
-- classes are unioned.
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

{-| This type represents a reference to an entry in the tree data
structure. An entry of type 'Entry' @s c a@ lives in the state space
indexed by @s@, contains equivalence class descriptors of type @c@ and
has elements of type @a@.-}

newtype Entry s c a = Entry (STRef s (EntryData s c a))
    deriving (Eq)

{-| This type represents entries (nodes) in the tree data
structure. Entry data of type 'EntryData' @s c a@ lives in the state space
indexed by @s@, contains equivalence class descriptors of type @c@ and
has elements of type @a@.  -}

data EntryData s c a = Node {
      entryParent :: Entry s c a,
      entryValue :: a
    }
                     | Root {
      entryDesc :: c,
      entryWeight :: Int,
      entryValue :: a
    }

{-| This is the top-level data structure that represents an
equivalence relation. An equivalence relation of type 'Equiv' @s c a@
lives in the state space indexed by @s@, contains equivalence class
descriptors of type @c@ and has elements of type @a@. -}

data Equiv s c a = Equiv {
      -- | maps elements to their entry in the tree data structure
      entries :: STRef s (Map a (Entry s c a)), 
      -- | constructs an equivalence class descriptor for a singleton class
      singleDesc :: a -> c,
      -- | combines the equivalence class descriptor of two classes
      --   which are meant to be combined.
      combDesc :: c -> c -> c
      }
{-
   not used

{-|
  This function modifies the content of a reference cell.
-}

modifySTRef :: (Monad m) => STRef s a -> (a -> a) -> STT s m ()
modifySTRef r f = readSTRef r >>= (writeSTRef r . f)

-}

{-| This function constructs the initial data structure for
maintaining an equivalence relation. That is it represents, the fines
(or least) equivalence class (of the set of all elements of type
@a@). The arguments are used to maintain equivalence class
descriptors. -}

leastEquiv :: Monad m
          -- | used to construct an equivalence class descriptor for a singleton class
           => (a -> c)
          -- | used to combine the equivalence class descriptor of two classes
          --   which are meant to be combined.
           -> (c -> c -> c)
           -> STT s m (Equiv s c a)
leastEquiv mk com = do 
  es <- newSTRef Map.empty
  return Equiv {entries = es, singleDesc = mk, combDesc = com}



{-| This function returns the representative entry of the argument's
equivalence class (i.e. the root of its tree) or @Nothing@ if it is
the representative itself.

This function performs path compression.  -}

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




{-| This function returns the representative entry of the argument's
equivalence class (i.e. the root of its tree).

This function performs path compression.  -}
representative :: Monad m => Entry s c a -> STT s m (Entry s c a)
representative entry = do
  mrepr <- representative' entry
  case mrepr of
    Nothing -> return entry
    Just repr -> return repr


{-| This function looks up the entry of the given element in the given
equivalence relation representation. If there is none yet, then a
fresh one is constructed which then represents a new singleton
equivalence class! -}

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

{-| This function looks up the entry of the given element in the given
equivalence relation representation or @Nothing@ if there is none,
yet.  -}

getEntry :: (Monad m, Ord a) => Equiv s c a -> a -> STT s m (Maybe (Entry s c a))
getEntry Equiv { entries = mref} val = do
  m <- readSTRef mref
  case Map.lookup val m of
    Nothing -> return Nothing
    Just entry -> return $ Just entry

{-| This function equates the two given elements. That is, it unions
the equivalence classes of the two elements and combines their
descriptor. -}

equate :: (Monad m, Ord a) => Equiv s c a -> a -> a -> STT s m ()
equate equiv x y = do
  ex <- getEntry' equiv x
  ey <- getEntry' equiv  y
  equate' equiv ex ey


{-| This function equates the two given entries. That is, it performs
a weighted union of their trees combines their descriptor. -}

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

{-| This function returns the descriptor of the given element's
equivalence class. -}

classDesc :: (Monad m, Ord a) => Equiv s c a -> a -> STT s m c
classDesc eq val = do
  mentry <- getEntry eq val
  case mentry of
    Nothing -> return $ singleDesc eq val
    Just entry -> classDesc' entry

{-| This function returns the descriptor of the given entry's tree. -}

classDesc' :: (Monad m) => Entry s c a -> STT s m c
classDesc' entry = do
  Entry e <- representative entry
  liftM entryDesc $ readSTRef e

{-| This function decides whether the two given elements are in the
same equivalence class according to the given equivalence relation
representation. -}

equivalent :: (Monad m, Ord a) => Equiv s c a -> a -> a -> STT s m Bool
equivalent eq v1 v2 = do
  me1 <- getEntry eq v1
  me2 <- getEntry eq v2
  case (me1,me2) of
    (Just e1, Just e2) -> equivalent' e1 e2
    (Nothing, Nothing) -> return $ v1 == v2
    _ -> return False
    
{-| This function decides whether the two given entries are in the
same tree (by comparing their roots).-}

equivalent' :: (Monad m, Ord a) => Entry s c a -> Entry s c a -> STT s m Bool
equivalent' e1 e2 = liftM2 (==) (representative e1) (representative e2)

