{-# LANGUAGE
  RankNTypes,
  FlexibleInstances,
  MultiParamTypeClasses,
  UndecidableInstances #-}


module Data.UnionFind.Monad
where

import Data.UnionFind.STT hiding (equate, equivalent, equivalenceClass)
import qualified Data.UnionFind.STT  as S

 
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.ST.Trans

newtype PartitionT s v m a = PartitionT {unPartitionT :: ReaderT (Partition s v) (STT s m) a}

instance (Monad m) => Monad (PartitionT s v m) where
    PartitionT m >>= f = PartitionT (m >>= (unPartitionT . f))
    return = PartitionT . return

instance MonadTrans (PartitionT s v) where
    lift = PartitionT . lift . lift

instance (MonadReader r m) => MonadReader r (PartitionT s v m) where
    ask = PartitionT $ lift ask
    local f (PartitionT (ReaderT m)) = PartitionT $ ReaderT $ (\ r -> local f (m r))

instance (Monoid w, MonadWriter w m) => MonadWriter w (PartitionT s v m) where
    tell w = PartitionT $ tell w
    listen (PartitionT m) = PartitionT $ listen m
    pass (PartitionT m) = PartitionT $ pass m

instance (MonadState st m) => MonadState st (PartitionT s v m) where
    get = PartitionT get
    put s = PartitionT $ put s

instance (MonadError e m) => MonadError e (PartitionT s v m) where
    throwError e = lift $ throwError e
    catchError (PartitionT m) f = PartitionT $ catchError m (unPartitionT . f)
    

runPartitionT :: (Monad m) => (forall s. PartitionT s v m a) -> m a
runPartitionT m = runST $ do
  p <- emptyPartition
  (`runReaderT` p) $ unPartitionT m

equivalent :: (Monad m, Ord v) => v -> v -> PartitionT s v m Bool
equivalent x y = PartitionT $ do
  part <- ask
  lift $ S.equivalent part x y

equivalenceClass :: (Monad m, Ord v) => v -> PartitionT s v m [v]
equivalenceClass x = PartitionT $ do
  part <- ask
  lift $ S.equivalenceClass part x

equate :: (Monad m, Ord v) => v -> v -> PartitionT s v m ()
equate x y = PartitionT $ do
  part <- ask
  lift $ S.equate part x y