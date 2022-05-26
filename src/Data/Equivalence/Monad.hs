{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-} -- for type equality ~
{-# LANGUAGE TypeOperators #-} -- for type equality ~ with GHC 9.4
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Equivalence.Monad
-- Copyright   : Patrick Bahr, 2010
-- License     : BSD-3-Clause
--
-- Maintainer  :  Patrick Bahr, Andreas Abel
-- Stability   :  stable
-- Portability :  non-portable (MPTC with FD)
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
     EquivT',
     EquivM,
     EquivM',
     runEquivT,
     runEquivT',
     runEquivM,
     runEquivM'
     ) where

import Data.Equivalence.STT hiding (equate, equateAll, equivalent, classDesc, removeClass,
                                    getClass , combine, combineAll, same , desc , remove )
import qualified Data.Equivalence.STT  as S


import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Error.Class
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.ST.Trans
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Fail as Fail


{-| This monad transformer encapsulates computations maintaining an
equivalence relation. A monadic computation of type 'EquivT' @s c v m
a@ maintains a state space indexed by type @s@, maintains an
equivalence relation over elements of type @v@ with equivalence class
descriptors of type @c@ and contains an internal monadic computation
of type @m a@. -}

newtype EquivT s c v m a = EquivT {unEquivT :: ReaderT (Equiv s c v) (STT s m) a}
  deriving (Functor, Applicative, Monad, MonadError e, MonadState st, MonadWriter w)

{-| This monad transformer is a special case of 'EquivT' that only
maintains trivial equivalence class descriptors of type @()@. -}

type EquivT' s = EquivT s ()

{-| This monad encapsulates computations maintaining an equivalence
relation. A monadic computation of type 'EquivM' @s c v a@ maintains a
state space indexed by type @s@, maintains an equivalence relation
over elements of type @v@ with equivalence class descriptors of type
@c@ and returns a value of type @a@.  -}

type EquivM s c v = EquivT s c v Identity

{-| This monad is a special case of 'EquivM' that only maintains
trivial equivalence class descriptors of type @()@. -}

type EquivM' s v = EquivM s () v

-- Instances for EquivT:

instance MonadTrans (EquivT s c v) where
    lift = EquivT . lift . lift

instance Monad m => Fail.MonadFail (EquivT s c v m) where
    fail = error

-- NB: This instance is beyond GeneralizedNewtypeDeriving
-- because EquivT already contains a ReaderT in its monad transformer stack.
instance (MonadReader r m) => MonadReader r (EquivT s c v m) where
    ask = EquivT $ lift ask
    local f (EquivT (ReaderT m)) = EquivT $ ReaderT $ \ r -> local f (m r)

{-| This function runs a monadic computation that maintains an
equivalence relation. The first two arguments specify how to construct
an equivalence class descriptor for a singleton class and how to
combine two equivalence class descriptors. -}

runEquivT
  :: (Monad m, Applicative m)
  => (v -> c)      -- ^ Used to construct an equivalence class descriptor for a singleton class.
  -> (c -> c -> c) -- ^ Used to combine the equivalence class descriptor of two classes
                   --   which are meant to be combined.
  -> (forall s. EquivT s c v m a)
  -> m a
runEquivT mk com m = runSTT $ do
  p <- leastEquiv mk com
  (`runReaderT` p) $ unEquivT m


{-| This function is a special case of 'runEquivT' that only maintains
trivial equivalence class descriptors of type @()@. -}

runEquivT' :: (Monad m, Applicative m) => (forall s. EquivT' s v m a) -> m a
runEquivT' = runEquivT (const ()) (\_ _-> ())

{-| This function runs a monadic computation that maintains an
equivalence relation. The first tow arguments specify how to construct
an equivalence class descriptor for a singleton class and how to
combine two equivalence class descriptors. -}

runEquivM
  :: (v -> c)      -- ^ Used to construct an equivalence class descriptor for a singleton class.
  -> (c -> c -> c) -- ^ Used to combine the equivalence class descriptor of two classes
                   --   which are meant to be combined.
  -> (forall s. EquivM s c v a)
  -> a
runEquivM sing comb m = runIdentity $ runEquivT sing comb m

{-| This function is a special case of 'runEquivM' that only maintains
trivial equivalence class descriptors of type @()@. -}

runEquivM' :: (forall s. EquivM' s v a) -> a
runEquivM' = runEquivM (const ()) (\_ _ -> ())

{-| This class specifies the interface for a monadic computation that
maintains an equivalence relation.  -}

class (Monad m, Applicative m, Ord v) => MonadEquiv c v d m | m -> v, m -> c, m -> d where

    {-| This function decides whether the two given elements are
        equivalent in the current equivalence relation. -}

    equivalent :: v -> v -> m Bool

    {-| This function obtains the descriptor of the given element's
        equivalence class. -}

    classDesc :: v -> m d

    {-| This function equates the element in the given list. That is, it
      unions the equivalence classes of the elements and combines their
      descriptor. -}

    equateAll :: [v] -> m ()

    {-| This function equates the given two elements. That is it
        unions the equivalence classes of the two elements. -}

    equate :: v -> v -> m ()
    equate x y = equateAll [x,y]

    {-| This function removes the equivalence class of the given
      element. If there is no corresponding equivalence class, @False@ is
      returned; otherwise @True@. -}

    removeClass :: v -> m Bool

    {-| This function provides the equivalence class of the given element. -}

    getClass :: v -> m c

    {-| This function combines all equivalence classes in the given
      list. Afterwards all elements in the argument list represent the same
      equivalence class! -}

    combineAll :: [c] -> m ()

    {-| This function combines the two given equivalence
      classes. Afterwards both arguments represent the same equivalence
      class! One of it is returned in order to represent the new combined
      equivalence class. -}

    combine :: c -> c -> m c
    combine x y = combineAll [x,y] >> return x

    {-| This function decides whether the two given equivalence classes
      are the same. -}

    (===) :: c -> c -> m Bool

    {-| This function returns the descriptor of the given
      equivalence class. -}

    desc :: c -> m d

    {-| This function removes the given equivalence class. If the
      equivalence class does not exist anymore, @False@ is returned;
      otherwise @True@. -}

    remove :: c -> m Bool

    -- Default implementations for lifting via a monad transformer.
    -- Unfortunately, GHC does not permit us to give these also to
    -- 'equate' and 'combine', which already have a default implementation.

    default equivalent  :: (MonadEquiv c v d n, MonadTrans t, t n ~ m) => v -> v -> m Bool
    equivalent x y       = lift $ equivalent x y

    default classDesc   :: (MonadEquiv c v d n, MonadTrans t, t n ~ m) => v -> m d
    classDesc            = lift . classDesc

    default equateAll   :: (MonadEquiv c v d n, MonadTrans t, t n ~ m) => [v] -> m ()
    equateAll            = lift . equateAll

    default removeClass :: (MonadEquiv c v d n, MonadTrans t, t n ~ m) => v -> m Bool
    removeClass          = lift . removeClass

    default getClass    :: (MonadEquiv c v d n, MonadTrans t, t n ~ m) => v -> m c
    getClass             = lift . getClass

    default combineAll  :: (MonadEquiv c v d n, MonadTrans t, t n ~ m) => [c] -> m ()
    combineAll           = lift . combineAll

    default (===)       :: (MonadEquiv c v d n, MonadTrans t, t n ~ m) => c -> c -> m Bool
    x === y              = lift $ (===) x y

    default desc        :: (MonadEquiv c v d n, MonadTrans t, t n ~ m) => c -> m d
    desc                 = lift . desc

    default remove      :: (MonadEquiv c v d n, MonadTrans t, t n ~ m) => c -> m Bool
    remove               = lift . remove


instance (Monad m, Applicative m, Ord v) => MonadEquiv (Class s d v) v d (EquivT s d v m) where
    equivalent x y = EquivT $ do
      part <- ask
      lift $ S.equivalent part x y

    classDesc x = EquivT $ do
      part <- ask
      lift $ S.classDesc part x

    equateAll x = EquivT $ do
      part <- ask
      lift $ S.equateAll part x

    equate x y = EquivT $ do
      part <- ask
      lift $ S.equate part x y

    removeClass x = EquivT $ do
      part <- ask
      lift $ S.removeClass part x

    getClass x = EquivT $ do
      part <- ask
      lift $ S.getClass part x

    combineAll x = EquivT $ do
      part <- ask
      lift $ S.combineAll part x

    combine x y = EquivT $ do
      part <- ask
      lift $ S.combine part x y

    x === y = EquivT $ do
      part <- ask
      lift $ S.same part x y

    desc x = EquivT $ do
      part <- ask
      lift $ S.desc part x

    remove x = EquivT $ do
      part <- ask
      lift $ S.remove part x

instance (MonadEquiv c v d m, Monoid w) => MonadEquiv c v d (WriterT w m) where
    equate  x y = lift $ equate x y
    combine x y = lift $ combine x y

instance (MonadEquiv c v d m) => MonadEquiv c v d (ExceptT e m) where
    equate  x y = lift $ equate x y
    combine x y = lift $ combine x y

instance (MonadEquiv c v d m) => MonadEquiv c v d (StateT s m) where
    equate  x y = lift $ equate x y
    combine x y = lift $ combine x y

instance (MonadEquiv c v d m) => MonadEquiv c v d (ReaderT r m) where
    equate  x y = lift $ equate x y
    combine x y = lift $ combine x y
