module Data.Stream where

import Prelude

import Control.Apply (lift2)
import Data.Array (uncons, (:))
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)

data StreamF m a s = StreamF {step :: s -> Step m a s, initialState :: s}

data Step m a s = Yield (m a) s
                | Skip s
                | Done

newtype StreamT m a = StreamT (Exists (StreamF m a))

type Stream a = StreamT Identity a

instance newtypeStream :: Newtype (StreamT m a) (Exists (StreamF m a))  where
  wrap = StreamT
  unwrap (StreamT e) = e

instance functorStream :: Functor m => Functor (StreamT m) where
  map = mapStream

stream :: forall m a . Applicative m => Array a -> StreamT m a
stream as = StreamT <<< mkExists $
    StreamF { step: next
            , initialState: as
            }
  where
    next :: Array a -> Step m a (Array a)
    next as' = case uncons as' of
      Nothing -> Done
      Just {head, tail} -> Yield (pure head) tail

unstream :: forall m a . Applicative m => StreamT m a -> m (Array a)
unstream as = runExists unstream' (unwrap as)
  where
    unstream' :: forall s . StreamF m a s -> m (Array a)
    unstream' (StreamF strm) = case strm.step strm.initialState of
      Done -> pure []
      Skip s' -> unstream' $ StreamF strm {initialState = s'}
      Yield ma s' -> (:) <$> ma <*> unstream' (StreamF strm {initialState = s'})

instance foldableStream :: Foldable (StreamT Identity) where
  foldr f init as = (\(Identity a) -> a) $ foldr f init as
  foldl f init as = (\(Identity a) -> a) $ foldl f init as
  foldMap f as = (\(Identity a) -> a) $ foldMap f as

--------------------------------------------------------------------------------

mapStream :: forall m a b . Functor m => (a -> b) -> StreamT m a -> StreamT m b
mapStream f as = runExists mapStream' $ unwrap as
  where
    mapStream' :: forall s. StreamF m a s -> StreamT m b
    mapStream' (StreamF strm) = wrap <<< mkExists $
      StreamF strm { step = \s -> case strm.step s of
                       Done -> Done
                       Skip s' -> Skip s'
                       Yield la s' -> Yield (f <$> la) s'
                   }

foldl :: forall m a b. Applicative m => (b -> a -> b) -> b -> StreamT m a -> m b
foldl f init as = runExists (foldrStream' $ pure init) $ unwrap as
  where
    foldrStream' :: forall s . m b -> StreamF m a s -> m b
    foldrStream' mb (StreamF strm) = case strm.step strm.initialState of
      Done -> mb
      Skip s' -> foldrStream' mb (StreamF strm {initialState = s'})
      Yield ma s' -> foldrStream' (lift2 f mb ma) (StreamF strm {initialState = s'})

-- foldrStream might blow the stack
foldr :: forall m a b. Applicative m => (a -> b -> b) -> b -> StreamT m a -> m b
foldr f init as = runExists (foldrStream' $ pure init) $ unwrap as
  where
    foldrStream' :: forall s . m b -> StreamF m a s -> m b
    foldrStream' mb (StreamF strm) = case strm.step strm.initialState of
      Done -> mb
      Skip s' -> foldrStream' mb (StreamF strm {initialState = s'})
      Yield ma s' -> lift2 f ma $ foldrStream' mb (StreamF strm {initialState = s'})

--   foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
foldMap :: forall m a b .
           Monad m
           => Monoid b
           => (a -> b)
           -> StreamT m a
           -> m b
foldMap f as = runExists (foldMapStream' $ pure mempty) $ unwrap as
  where
    foldMapStream' :: forall s . m b -> StreamF m a s -> m b
    foldMapStream' acc (StreamF strm) = case strm.step strm.initialState of
      Done -> acc
      Skip s' -> foldMapStream' acc (StreamF strm {initialState = s'})
      Yield ma s' -> foldMapStream' (lift2 (<>) acc $ f <$> ma) (StreamF strm {initialState = s'})
