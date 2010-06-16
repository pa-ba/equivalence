{-# LANGUAGE
  RankNTypes,
  FlexibleInstances,
  FlexibleContexts,
  MultiParamTypeClasses,
  UndecidableInstances,
  FunctionalDependencies #-}


module Data.UnionFind.Monad
    (
     MonadPartition(..),
     PartitionT,
     runPartitionT
     ) where

import Data.UnionFind.STT hiding (equate, equivalent, classDesc)
import qualified Data.UnionFind.STT  as S

 
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.ST.Trans



newtype PartitionT s c v m a = PartitionT {unPartitionT :: ReaderT (Partition s c v) (STT s m) a}
type PartitionM s c v = PartitionT s c v Identity

instance (Monad m) => Monad (PartitionT s c v m) where
    PartitionT m >>= f = PartitionT (m >>= (unPartitionT . f))
    return = PartitionT . return

instance MonadTrans (PartitionT s c v) where
    lift = PartitionT . lift . lift

instance (MonadReader r m) => MonadReader r (PartitionT s c v m) where
    ask = PartitionT $ lift ask
    local f (PartitionT (ReaderT m)) = PartitionT $ ReaderT $ (\ r -> local f (m r))

instance (Monoid w, MonadWriter w m) => MonadWriter w (PartitionT s c v m) where
    tell w = PartitionT $ tell w
    listen (PartitionT m) = PartitionT $ listen m
    pass (PartitionT m) = PartitionT $ pass m

instance (MonadState st m) => MonadState st (PartitionT s c v m) where
    get = PartitionT get
    put s = PartitionT $ put s

instance (MonadError e m) => MonadError e (PartitionT s c v m) where
    throwError e = lift $ throwError e
    catchError (PartitionT m) f = PartitionT $ catchError m (unPartitionT . f)
    

runPartitionT :: (Monad m) => (v -> c) -> (c -> c -> c) -> (forall s. PartitionT s c v m a) -> m a
runPartitionT mk com m = runST $ do
  p <- emptyPartition mk com
  (`runReaderT` p) $ unPartitionT m


class (Monad m, Ord v) => MonadPartition c v m | m -> v, m -> c where
    equivalent :: v -> v -> m Bool
    classDesc :: v -> m c
    equate :: v -> v -> m ()

instance (Monad m, Ord v) => MonadPartition c v (PartitionT s c v m) where
    equivalent x y = PartitionT $ do
      part <- ask
      lift $ S.equivalent part x y

    classDesc x = PartitionT $ do
      part <- ask
      lift $ S.classDesc part x
           
    equate x y = PartitionT $ do
      part <- ask
      lift $ S.equate part x y

instance (MonadPartition c v m, MonadTrans t, Monad (t m)) => MonadPartition c v (t m) where
    equivalent x y = lift $ equivalent x y
    classDesc = lift . classDesc
    equate x y = lift $ equate x y