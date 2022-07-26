{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes, TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Equivalence.Monad_Test where

import Test.QuickCheck hiding ((===), classes)

import Data.Equivalence.Monad

import Control.Monad
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Exit

--------------------------------------------------------------------------------
-- Test Suits
--------------------------------------------------------------------------------

-- run :: (Ord a) => STT s Identity (Equiv s (Set a) a)
run :: (Ord v) => (forall s. EquivM s (Set v) v a) -> a
run = runEquivM Set.singleton Set.union

runInt :: (forall s. EquivM s (Set Int) Int a) -> a
runInt = run

allM f l = liftM and $ mapM f l

getClasses l1 = mapM getClass l1

infixr 9 <.>

-- | Composition: pure function after functorial (monadic) function.
(<.>) :: Functor m => (b -> c) -> (a -> m b) -> a -> m c
(f <.> g) a = f <$> g a


--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_singleton v = runInt $ do
  d <- classDesc v
  return (d == Set.singleton v)

prop_equateAll l' v = runInt $ do
  let l = v:l'
  equateAll l
  d <- classDesc v
  return (d == Set.fromList l)

prop_combineAll l' v = runInt $ do
  let l = v:l'
  cls <- getClasses l
  cl <- getClass v
  combineAll cls
  d <- desc cl
  return (d == Set.fromList l)

prop_equate x y = runInt $ do
  equate x y
  d <- classDesc x
  return (d == Set.fromList [x,y])

prop_combine x y = runInt $ do
  [cx,cy] <- getClasses [x,y]
  combine cx cy
  d <- desc cx
  return (d == Set.fromList [x,y])

prop_equateOverlap x y z = runInt $ do
  equate x y
  equate y z
  equivalent x z

prop_combineOverlap x y z = runInt $ do
  [cx,cy,cz] <- getClasses [x,y,z]
  combine cx cy
  combine cy cz
  cx === cz

prop_equateAllOverlap x y l1' l2' = runInt $ do
  let l1 = x:l1'
      l2 = y:l2'
  equateAll l1
  equateAll l2
  if Set.null $ Set.fromList l1 `Set.intersection` Set.fromList l2
    then liftM not $ equivalent x y
    else equivalent x y

prop_combineAllOverlap x y l1' l2' = runInt $ do
  let l1 = x:l1'
      l2 = y:l2'
  cls1 <- getClasses l1
  cls2 <- getClasses l2
  [cx,cy] <- getClasses [x,y]
  combineAll cls1
  combineAll cls2
  if Set.null $ Set.fromList l1 `Set.intersection` Set.fromList l2
    then liftM not (cx === cy)
    else cx === cy

prop_removeClass x l' = runInt $ do
  let l = x:l'
  equateAll l
  removeClass x
  allM (\e -> liftM (== Set.singleton e) (classDesc e)) l

prop_remove x l' = runInt $ do
  let l = x:l'
  cls <- getClasses l
  combineAll cls
  cx <- getClass x
  remove cx
  allM check l
      where check e = liftM (== Set.singleton e) $ getClass e >>= desc

prop_removeClass' x y l1' l2' = runInt $ do
  let l1 = x:l1'
      l2 = x:y:l2'
  equateAll l1
  removeClass x
  equateAll l2
  d <- classDesc y
  return (Set.fromList l2 == d)

prop_remove' x y l1' l2' = runInt $ do
  let l1 = x:l1'
      l2 = x:y:l2'
  cls1 <- getClasses l1
  cls2 <- getClasses l2
  cx <- getClass x
  combineAll cls1
  remove cx
  combineAll cls2
  cy <- getClass y
  d <- desc cy
  return (Set.fromList l2 == d)


prop_getClasses l1 l1' l2 x y =
  putStrLn (show el ++ ";" ++ show cl) `whenFail` (el == cl)
  where
    l3 = concat (l2 : l1)

    el = runInt $ do
      mapM equateAll l1
      mapM removeClass l2
      mapM equateAll (l1' :: [[Int]])
      res <- mapM classDesc l3
      eq <- equivalent x y
      return (res,eq)

    cl = runInt $ do
      cls1 <- mapM getClasses l1
      mapM combineAll cls1
      cls2 <- getClasses l2
      mapM remove cls2
      cls1' <- mapM getClasses l1'
      mapM combineAll cls1'
      cls3 <- getClasses l3
      res <- mapM desc cls3
      [cx,cy] <- getClasses [x,y]
      eq <- cx === cy
      return (res,eq)

prop_values l = runInt $ do
    mapM (\x -> equate x x) l
    sameSet l <$> values
  where
    sameSet = (==) `on` Set.fromList

prop_classes l = runInt $ do
    mapM equateAll (l :: [[Int]])
    classes1 <- uniqClass =<< mapM getClass =<< values
    sameClasses classes1 =<< classes
  where
    uniqClass []     = return []
    uniqClass (c:cs) = (c :) <$> do
      uniqClass =<< filterM (not <.> (c ===)) cs

    sameClasses []       cs2 = return $ null cs2
    sameClasses (c:cs1') cs2 = sameClasses cs1' =<< filterM (not <.> (c ===)) cs2

return []

main :: IO ()
main = do
  success <- $quickCheckAll
  if success then exitSuccess else exitFailure
