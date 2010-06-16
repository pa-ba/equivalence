{-# LANGUAGE
  RankNTypes,
  FlexibleInstances,
  FlexibleContexts,
  MultiParamTypeClasses,
  UndecidableInstances,
  FunctionalDependencies #-}


module Data.Equivalence.Monad
    (
     MonadEquiv(..),
     EquivT,
     runEquivT
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



newtype EquivT s c v m a = EquivT {unEquivT :: ReaderT (Equiv s c v) (STT s m) a}
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
    

runEquivT :: (Monad m) => (v -> c) -> (c -> c -> c) -> (forall s. EquivT s c v m a) -> m a
runEquivT mk com m = runST $ do
  p <- leastEquiv mk com
  (`runReaderT` p) $ unEquivT m


class (Monad m, Ord v) => MonadEquiv c v m | m -> v, m -> c where
    equivalent :: v -> v -> m Bool
    classDesc :: v -> m c
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

instance (MonadEquiv c v m, MonadTrans t, Monad (t m)) => MonadEquiv c v (t m) where
    equivalent x y = lift $ equivalent x y
    classDesc = lift . classDesc
    equate x y = lift $ equate x y