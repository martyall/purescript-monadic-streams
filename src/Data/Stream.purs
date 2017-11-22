module Data.Stream where

import Prelude

import Data.Array (uncons, (:))
import Data.Enum (class Enum, succ)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

data StreamF m a s = StreamF {step :: s -> m (Step a s), initialState :: s}

data Step a s = Yield a s
              | Skip s
              | Done

newtype StreamT m a = StreamT (Exists (StreamF m a))

type Stream a = StreamT Identity a

instance newtypeStream :: Newtype (StreamT m a) (Exists (StreamF m a))  where
  wrap = StreamT
  unwrap (StreamT e) = e

instance functorStream :: Monad m => Functor (StreamT m) where
  map = mapStream

stream :: forall m a . Monad m => Array a -> StreamT m a
stream as = wrap <<< mkExists $
    StreamF { step: pure <<< next
            , initialState: as
            }
  where
    next :: Array a -> Step a (Array a)
    next as' = case uncons as' of
      Nothing -> Done
      Just {head, tail} -> Yield head tail

unstream :: forall m a . Monad m => StreamT m a -> m (Array a)
unstream as = runExists unstream' (unwrap as)
  where
    unstream' :: forall s . StreamF m a s -> m (Array a)
    unstream' (StreamF strm) = do
      st <- strm.step strm.initialState
      case st of
        Done -> pure []
        Skip s' -> unstream' $ StreamF strm {initialState = s'}
        Yield a s' -> (:) a <$> unstream' (StreamF strm {initialState = s'})

instance foldableStream :: Foldable (StreamT Identity) where
  foldr f init as = (\(Identity a) -> a) $ foldr f init as
  foldl f init as = (\(Identity a) -> a) $ foldl f init as
  foldMap f as = (\(Identity a) -> a) $ foldMap f as

mkEnumStream :: forall m a b .
                Monad m
             => Enum a
             => Tuple a a
             -> (a -> m (Maybe b))
             -> StreamT m b
mkEnumStream (Tuple start stop) f =
    wrap <<< mkExists $ StreamF {step: step', initialState: start}
  where
    step' s =
      if s > stop || succ s == Nothing
        then pure Done
        else do
          let next = unsafePartial fromJust <<< succ $ s
          mres <- f s
          case mres of
            Nothing -> pure $ Skip next
            Just res -> pure $ Yield res next

mapStream :: forall m a b . Monad m => (a -> b) -> StreamT m a -> StreamT m b
mapStream f as = runExists mapStream' $ unwrap as
  where
    mapStream' :: forall s. StreamF m a s -> StreamT m b
    mapStream' (StreamF strm) = wrap <<< mkExists $
      StreamF strm { step = \s -> do
                       st <- strm.step s
                       pure $ case st of
                         Done -> Done
                         Skip s' -> Skip s'
                         Yield a s' -> Yield (f a) s'
                   }

foldl :: forall m a b. Monad m => (b -> a -> b) -> b -> StreamT m a -> m b
foldl f init as = runExists (foldlStream' init) $ unwrap as
  where
    foldlStream' :: forall s . b -> StreamF m a s -> m b
    foldlStream' b (StreamF strm) = do
      st <- strm.step strm.initialState
      case st of
        Done -> pure b
        Skip s' -> foldlStream' b (StreamF strm {initialState = s'})
        Yield a s' -> foldlStream' (f b a) (StreamF strm {initialState = s'})

foldr :: forall m a b. Monad m => (a -> b -> b) -> b -> StreamT m a -> m b
foldr f init as = runExists (foldrStream' $ pure init) $ unwrap as
  where
    foldrStream' :: forall s . m b -> StreamF m a s -> m b
    foldrStream' mb (StreamF strm) = do
      st <- strm.step strm.initialState
      case st of
        Done -> mb
        Skip s' -> foldrStream' mb (StreamF strm {initialState = s'})
        Yield a s' -> f a <$> foldrStream' mb (StreamF strm {initialState = s'})

foldMap :: forall m a b .
           Monad m
           => Monoid b
           => (a -> b)
           -> StreamT m a
           -> m b
foldMap f as = runExists (foldMapStream' mempty) $ unwrap as
  where
    foldMapStream' :: forall s . b -> StreamF m a s -> m b
    foldMapStream' acc (StreamF strm) = do
      st <- strm.step strm.initialState
      case st of
        Done -> pure acc
        Skip s' -> foldMapStream' acc (StreamF strm {initialState = s'})
        Yield a s' -> foldMapStream' (acc  <> f a) (StreamF strm {initialState = s'})

