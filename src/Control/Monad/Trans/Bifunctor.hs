{-# LANGUAGE LambdaCase #-}
module Control.Monad.Trans.Bifunctor (
    BifunctorTrans(..)
  , bimapX
  , firstX
  , secondX
  ) where

import           Control.Monad.Morph (MMonad, squash, hoist)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict


-- | Class of monad transformers which are bifunctors.
--
--   You can implement a 'BifunctorTrans' by either defining 'bimapT' or by
--   defining both 'firstT' and 'secondT'.
--
--   If you supply 'bimapT', you should ensure that:
--
-- @'bimapT' 'id' 'id' ≡ 'id'@
--
--   If you supply 'first' and 'second', ensure:
--
-- @
-- 'firstT'  'id' ≡ 'id'
-- 'secondT' 'id' ≡ 'id'
-- @
--
--   If you supply both, you should also ensure:
--
-- @'bimapT' f g ≡ 'firstT' f '.' 'secondT' g@
--
--   These ensure by parametricity:
--
-- @
-- 'bimapT'  (f '.' g) (h '.' i) ≡ 'bimapT' f h '.' 'bimapT' g i
-- 'firstT'  (f '.' g) ≡ 'firstT'  f '.' 'firstT'  g
-- 'secondT' (f '.' g) ≡ 'secondT' f '.' 'secondT' g
-- @
--
class BifunctorTrans t where
  {-# MINIMAL bimapT | firstT, secondT #-}

  -- | Map over both arguments at the same time.
  --
  -- @'bimap' f g ≡ 'first' f '.' 'second' g@
  --
  bimapT :: Functor f => (x -> y) -> (a -> b) -> t x f a -> t y f b
  bimapT f g =
    firstT f . secondT g
  {-# INLINE bimapT #-}

  -- | Map covariantly over the first argument.
  --
  -- @'firstT' f ≡ 'bimapT' f 'id'@
  --
  firstT :: Functor f => (x -> y) -> t x f a -> t y f a
  firstT f =
    bimapT f id
  {-# INLINE firstT #-}

  -- | Map covariantly over the second argument.
  --
  -- @'second' ≡ 'bimap' 'id'@
  --
  secondT :: Functor f => (a -> b) -> t x f a -> t x f b
  secondT =
    bimapT id
  {-# INLINE secondT #-}

instance BifunctorTrans ExceptT where
  bimapT f g =
    let
      h = \case
        Left x ->
          Left (f x)
        Right a ->
          Right (g a)
      {-# INLINE h #-}
    in
      ExceptT . fmap h . runExceptT
  {-# INLINE bimapT #-}

instance BifunctorTrans Lazy.WriterT where
  bimapT f g =
    let
      h (a, x) =
        (g a, f x)
      {-# INLINE h #-}
    in
      Lazy.WriterT . fmap h . Lazy.runWriterT
  {-# INLINE bimapT #-}

instance BifunctorTrans Strict.WriterT where
  bimapT f g =
    let
      h (a, x) =
        (g a, f x)
      {-# INLINE h #-}
    in
      Strict.WriterT . fmap h . Strict.runWriterT
  {-# INLINE bimapT #-}

-- | Map over two stacked transformer bifunctors, unifying their type and
--   squashing the result.
--
bimapX :: (BifunctorTrans t, MMonad (t z), Monad (t y m), Monad m) => (x -> z) -> (y -> z) -> t x (t y m) a -> t z m a
bimapX f g =
  squash . hoist (firstT g) . firstT f
{-# INLINE bimapX #-}

-- | Map over the first of two stacked transformer bifunctors, unifying their
--   type and squashing the result.
--
firstX :: (BifunctorTrans t, MMonad (t y), Functor (t y m), Monad m) => (x -> y) -> t x (t y m) a -> t y m a
firstX f =
  squash . firstT f
{-# INLINE firstX #-}

-- | Map over the second of two stacked transformer bifunctors, unifying their
--   type and squashing the result.
--
secondX :: (BifunctorTrans t, MMonad (t x), Monad (t y m), Monad m) => (y -> x) -> t x (t y m) a -> t x m a
secondX f =
  squash . hoist (firstT f)
{-# INLINE secondX #-}
