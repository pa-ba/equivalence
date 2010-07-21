{-# LANGUAGE
  RankNTypes,
  FlexibleInstances,
  FlexibleContexts,
  MultiParamTypeClasses,
  UndecidableInstances,
  FunctionalDependencies #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Equivalence.Monad
-- Copyright   : Patrick Bahr, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This is an alternative interface to the union-find implementation
-- in ''Data.Equivalence.STT''. It is wrapped into the monad
-- transformer 'EquivT'.
--
--------------------------------------------------------------------------------

module Data.Equivalence.Monad
    (
     MonadEquiv(..),
     EquivT(..),
     EquivM,
     runEquivT,
     runEquivM
     ) where

import Data.Equivalence.STT hiding (equate, equivalent, classDesc)
import qualified Data.Equivalence.STT  as S

 
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.ST.Trans


{-| This monad transformer encapsulates computations maintaining an
equivalence relation. A monadic computation of type 'EquivT' @s c v m
a@ maintains a state space indexed by type @s@, maintains an
equivalence relation over elements of type @v@ with equivalence class
descriptors of type @c@ and contains an internal monadic computation
of type @m a@. -}

newtype EquivT s c v m a = EquivT {unEquivT :: ReaderT (Equiv s c v) (STT s m) a}

{-| This monad encapsulates computations maintaining an equivalence
relation. A monadic computation of type 'EquivM' @s c v a@ maintains a
state space indexed by type @s@, maintains an equivalence relation
over elements of type @v@ with equivalence class descriptors of type
@c@ and returns a value of type @a@.  -}

type EquivM s c v = EquivT s c v Identity

instance (Monad m) => Monad (EquivT s c v m) where
    EquivT m >>= f = EquivT (m >>= (unEquivT . f))
    return = EquivT . return

instance MonadTrans (EquivT s c v) where
    lift = EquivT . lift . lift

instance (MonadReader r m) => MonadReader r (EquivT s c v m) where
    ask = EquivT $ lift ask
    local f (EquivT (ReaderT m)) = EquivT $ ReaderT $ (\ r -> local f (m r))

instance (Monoid w, MonadWriter w m) => MonadWriter w (EquivT s c v m) where
    tell w = EquivT $ tell w
    listen (EquivT m) = EquivT $ listen m
    pass (EquivT m) = EquivT $ pass m

instance (MonadState st m) => MonadState st (EquivT s c v m) where
    get = EquivT get
    put s = EquivT $ put s

instance (MonadError e m) => MonadError e (EquivT s c v m) where
    throwError e = lift $ throwError e
    catchError (EquivT m) f = EquivT $ catchError m (unEquivT . f)
    
{-| This function runs a monadic computation that maintains an
equivalence relation. The first tow arguments specify how to construct
an equivalence class descriptor for a singleton class and how to
combine two equivalence class descriptors. -}

runEquivT :: (Monad m)
          -- | used to construct an equivalence class descriptor for a singleton class
          => (v -> c)
          -- | used to combine the equivalence class descriptor of two classes
          --   which are meant to be combined.
          -> (c -> c -> c)
          -> (forall s. EquivT s c v m a)
          -> m a
runEquivT mk com m = runST $ do
  p <- leastEquiv mk com
  (`runReaderT` p) $ unEquivT m

{-| This function runs a monadic computation that maintains an
equivalence relation. The first tow arguments specify how to construct
an equivalence class descriptor for a singleton class and how to
combine two equivalence class descriptors. -}
runEquivM ::
          -- | used to construct an equivalence class descriptor for a singleton class
             (v -> c)
          -- | used to combine the equivalence class descriptor of two classes
          --   which are meant to be combined.
          -> (c -> c -> c)
          -> (forall s. EquivM s c v a)
          -> a
runEquivM sing comb m = runIdentity $ runEquivT sing comb m

{-| This class specifies the interface for a monadic computation that
maintains an equivalence relation.  -}

class (Monad m, Ord v) => MonadEquiv c v m | m -> v, m -> c where
    {-| This function decides whether the two given elements are
        equivalent in the current equivalence relation -}

    equivalent :: v -> v -> m Bool
    {-| This function obtains the descriptor of the given element's
        equivalence class. -}

    classDesc :: v -> m c
    
    {-| This function equates the given two elements. That is it
        unions the equivalence classes of the two elements. -}

    equate :: v -> v -> m ()

instance (Monad m, Ord v) => MonadEquiv c v (EquivT s c v m) where
    equivalent x y = EquivT $ do
      part <- ask
      lift $ S.equivalent part x y

    classDesc x = EquivT $ do
      part <- ask
      lift $ S.classDesc part x
           
    equate x y = EquivT $ do
      part <- ask
      lift $ S.equate part x y

instance (MonadEquiv c v m, Monoid w) => MonadEquiv c v (WriterT w m) where
    equivalent x y = lift $ equivalent x y
    classDesc = lift . classDesc
    equate x y = lift $ equate x y

instance (MonadEquiv c v m, Error e) => MonadEquiv c v (ErrorT e m) where
    equivalent x y = lift $ equivalent x y
    classDesc = lift . classDesc
    equate x y = lift $ equate x y

instance (MonadEquiv c v m) => MonadEquiv c v (StateT s m) where
    equivalent x y = lift $ equivalent x y
    classDesc = lift . classDesc
    equate x y = lift $ equate x y

instance (MonadEquiv c v m) => MonadEquiv c v (ReaderT r m) where
    equivalent x y = lift $ equivalent x y
    classDesc = lift . classDesc
    equate x y = lift $ equate x y