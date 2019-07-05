{-# LANGUAGE MultiParamTypeClasses #-}

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
  ( 
   -- * Equivalence Relation
    Equiv
  , Class
  , leastEquiv
  -- * Operations on Equivalence Classes
  , getClass
  , combine
  , combineAll
  , same
  , desc
  , remove
  -- * Operations on Elements
  , equate
  , equateAll
  , equivalent
  , classDesc
  , removeClass
  ) where

import Control.Monad.ST.Trans
import Control.Monad
import Control.Applicative
import qualified Control.Monad.Fail as Fail

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map

newtype Class s c a = Class (STRef s (Entry s c a))


{-| This type represents a reference to an entry in the tree data
structure. An entry of type 'Entry' @s c a@ lives in the state space
indexed by @s@, contains equivalence class descriptors of type @c@ and
has elements of type @a@.-}

newtype Entry s c a = Entry {unentry :: STRef s (EntryData s c a)}

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
      entryValue :: a,
      entryDeleted :: Bool
    }

type Entries s c a = STRef s (Map a (Entry s c a))

{-| This is the top-level data structure that represents an
equivalence relation. An equivalence relation of type 'Equiv' @s c a@
lives in the state space indexed by @s@, contains equivalence class
descriptors of type @c@ and has elements of type @a@. -}

data Equiv s c a = Equiv {
      -- | maps elements to their entry in the tree data structure
      entries :: Entries s c a, 
      -- | constructs an equivalence class descriptor for a singleton class
      singleDesc :: a -> c,
      -- | combines the equivalence class descriptor of two classes
      --   which are meant to be combined.
      combDesc :: c -> c -> c
      }

{-| This function constructs the initial data structure for
maintaining an equivalence relation. That is it represents, the fines
(or least) equivalence class (of the set of all elements of type
@a@). The arguments are used to maintain equivalence class
descriptors. -}

leastEquiv :: (Monad m, Applicative m)
           => (a -> c) -- ^ used to construct an equivalence class descriptor for a singleton class
           -> (c -> c -> c) -- ^ used to combine the equivalence class descriptor of two classes
                            --   which are meant to be combined.
           -> STT s m (Equiv s c a)
leastEquiv mk com = do 
  es <- newSTRef Map.empty
  return Equiv {entries = es, singleDesc = mk, combDesc = com}



{-| This function returns the representative entry of the argument's
equivalence class (i.e. the root of its tree) or @Nothing@ if it is
the representative itself.

This function performs path compression.  -}

representative' :: (Monad m, Applicative m) => Entry s c a -> STT s m (Maybe (Entry s c a),Bool)
representative' (Entry e) = do
  ed <- readSTRef e
  case ed of
    Root {entryDeleted = del} -> do
      return (Nothing, del)
    Node {entryParent = parent} -> do
      (mparent',del) <- representative' parent
      case mparent' of
        Nothing -> return $ (Just parent, del)
        Just parent' -> writeSTRef e ed{entryParent = parent'} >> return (Just parent', del)


{-| This function returns the representative entry of the argument's
equivalence class (i.e. the root of its tree).

This function performs path compression.  -}

representative :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> STT s m (Entry s c a)
representative eq v = do
  mentry <- getEntry eq v
  case mentry of -- check whether there is an entry
    Nothing -> mkEntry eq v -- if not, create a new one
    Just entry -> do
      (mrepr,del) <- representative' entry
      if del -- check whether equivalence class was deleted
        then mkEntry eq v -- if so, create a new entry
        else case mrepr of
               Nothing -> return entry
               Just repr -> return repr

{-| This function provides the representative entry of the given
equivalence class. This function performs path compression. -}

classRep :: (Monad m, Applicative m, Ord a) => Equiv s c a -> Class s c a -> STT s m (Entry s c a)
classRep eq (Class p) = do
  entry <- readSTRef p
  (mrepr,del) <- representative' entry
  if del -- check whether equivalence class was deleted
    then do v <- liftM entryValue $ readSTRef (unentry entry)
            en <- getEntry' eq v -- if so, create a new entry
            (mrepr,del) <- representative' en
            if del then do
                en' <- mkEntry' eq en
                writeSTRef p en'
                return en'
              else return (fromMaybe en mrepr)
    else return (fromMaybe entry mrepr)
  

{-| This function constructs a new (root) entry containing the given
entry's value, inserts it into the lookup table (thereby removing any
existing entry). -}

mkEntry' :: (Monad m, Applicative m, Ord a)
        => Equiv s c a -> Entry s c a
        -> STT s m (Entry s c a)  -- ^ the constructed entry
mkEntry' eq (Entry e) = readSTRef e >>= mkEntry eq . entryValue

{-| This function constructs a new (root) entry containing the given
value, inserts it into the lookup table (thereby removing any existing
entry). -}

mkEntry :: (Monad m, Applicative m, Ord a)
        => Equiv s c a -> a
        -> STT s m (Entry s c a)  -- ^ the constructed entry
mkEntry Equiv {entries = mref, singleDesc = mkDesc} val = do
  e <- newSTRef Root
       { entryDesc = mkDesc val,
         entryWeight = 1,
         entryValue = val,
         entryDeleted = False
       }
  let entry = Entry e
  m <- readSTRef mref
  writeSTRef mref (Map.insert val entry m)
  return entry

{-| This function provides the equivalence class the given element is
contained in. -}

getClass :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> STT s m (Class s c a)
getClass eq v = do 
  en <- (getEntry' eq v)
  liftM Class $ newSTRef en
  

getEntry' :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> STT s m (Entry s c a)
getEntry' eq v = do
  mentry <- getEntry eq v
  case mentry of
    Nothing -> mkEntry eq v
    Just entry -> return entry

{-| This function looks up the entry of the given element in the given
equivalence relation representation or @Nothing@ if there is none,
yet.  -}

getEntry :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> STT s m (Maybe (Entry s c a))
getEntry Equiv { entries = mref} val = do
  m <- readSTRef mref
  case Map.lookup val m of
    Nothing -> return Nothing
    Just entry -> return $ Just entry



{-| This function equates the two given elements. That is, it unions
the equivalence classes of the two elements and combines their
descriptor. The returned entry is the representative of the new
equivalence class -}

equateEntry :: (Fail.MonadFail m, Applicative m, Ord a) => Equiv s c a -> Entry s c a -> Entry s c a -> STT s m (Entry s c a)
equateEntry Equiv {combDesc = mkDesc} repx@(Entry rx) repy@(Entry ry) = 
  if (rx /= ry) then do
    dx <- readSTRef rx
    dy <- readSTRef ry
    case (dx, dy) of
      ( Root{entryWeight = wx, entryDesc = chx, entryValue = vx}
        , Root{entryWeight = wy, entryDesc = chy, entryValue = vy} ) ->
        if  wx >= wy
        then do
          writeSTRef ry Node {entryParent = repx, entryValue = vy}
          writeSTRef rx dx{entryWeight = wx + wy, entryDesc = mkDesc chx chy}
          return repx
        else do
          writeSTRef rx Node {entryParent = repy, entryValue = vx}
          writeSTRef ry dy{entryWeight = wx + wy, entryDesc = mkDesc chx chy}
          return repy

      _ -> fail "error on `equateEntry`"
  else return  repx

combineEntries :: (Fail.MonadFail m, Applicative m, Ord a)
               => Equiv s c a -> [b] -> (b -> STT s m (Entry s c a)) -> STT s m ()
combineEntries  _ [] _ = return ()
combineEntries eq (e:es) rep = do
  er <- rep e
  run er es
    where run er (f:r) = do
            fr <- rep f
            er' <- equateEntry eq er fr
            run er' r
          run _ _ = return ()


{-| This function combines all equivalence classes in the given
list. Afterwards all elements in the argument list represent the same
equivalence class! -}

combineAll :: (Fail.MonadFail m, Applicative m, Ord a) => Equiv s c a -> [Class s c a] -> STT s m ()
combineAll eq cls = combineEntries eq cls (classRep eq)


{-| This function combines the two given equivalence
classes. Afterwards both arguments represent the same equivalence
class! One of it is returned in order to represent the new combined
equivalence class. -}

combine :: (Fail.MonadFail m, Applicative m, Ord a) => Equiv s c a -> Class s c a -> Class s c a -> STT s m (Class s c a)
combine eq x y = combineAll eq [x,y] >> return x


{-| This function equates the element in the given list. That is, it
unions the equivalence classes of the elements and combines their
descriptor. -}

equateAll :: (Fail.MonadFail m, Applicative m, Ord a) => Equiv s c a -> [a] -> STT s m ()
equateAll eq cls = combineEntries eq cls (representative eq)

{-| This function equates the two given elements. That is, it unions
the equivalence classes of the two elements and combines their
descriptor. -}

equate :: (Fail.MonadFail m, Applicative m, Ord a) => Equiv s c a -> a -> a -> STT s m ()
equate eq x y = equateAll eq [x,y]


{-| This function returns the descriptor of the given
equivalence class. -}

desc :: (Monad m, Applicative m, Ord a) => Equiv s c a -> Class s c a -> STT s m c
desc eq cl = do
  Entry e <- classRep eq cl
  liftM entryDesc $ readSTRef e

{-| This function returns the descriptor of the given element's
equivalence class. -}

classDesc :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> STT s m c
classDesc eq val = do
  Entry e <- representative eq val
  liftM entryDesc $ readSTRef e


{-| This function decides whether the two given equivalence classes
are the same. -}

same :: (Monad m, Applicative m, Ord a) => Equiv s c a -> Class s c a -> Class s c a -> STT s m Bool
same eq c1 c2 = do
  (Entry r1) <- classRep eq c1
  (Entry r2) <- classRep eq c2
  return (r1 == r2)

{-| This function decides whether the two given elements are in the
same equivalence class according to the given equivalence relation
representation. -}

equivalent :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> a -> STT s m Bool
equivalent eq v1 v2 = do
  (Entry r1) <- representative eq v1
  (Entry r2) <- representative eq v2
  return (r1 == r2)



{-|
  This function modifies the content of a reference cell.
 -}

modifySTRef :: (Monad m, Applicative m) => STRef s a -> (a -> a) -> STT s m ()
modifySTRef r f = readSTRef r >>= (writeSTRef r . f)


{-| This function marks the given root entry as deleted.  -}

removeEntry :: (Monad m, Applicative m, Ord a) => Entry s c a -> STT s m ()
removeEntry (Entry r) = modifySTRef r change
    where change e = e {entryDeleted = True}


{-| This function removes the given equivalence class. If the
equivalence class does not exists anymore @False@ is returned;
otherwise @True@. -}

remove :: (Monad m, Applicative m, Ord a) => Equiv s c a -> Class s c a -> STT s m Bool
remove eq (Class p) = do
  entry <- readSTRef p
  (mrepr,del) <- representative' entry
  if del then do
        v <- liftM entryValue $ readSTRef (unentry entry)
        men <- getEntry eq v
        case men of
          Nothing -> return False
          Just en -> do      
            writeSTRef p en
            (mentry,del) <- representative' en
            if del 
              then return False
              else removeEntry (fromMaybe en mentry)
                   >> return True
    else removeEntry (fromMaybe entry mrepr)
         >> return True

{-| This function removes the equivalence class of the given
element. If there is no corresponding equivalence class, @False@ is
returned; otherwise @True@. -}

removeClass :: (Monad m, Applicative m, Ord a) => Equiv s c a -> a -> STT s m Bool
removeClass eq v = do
  mentry <- getEntry eq v
  case mentry of
    Nothing -> return False
    Just entry -> do
      (mentry, del) <- representative' entry
      if del 
        then return False
        else removeEntry (fromMaybe entry mentry)
             >> return True
