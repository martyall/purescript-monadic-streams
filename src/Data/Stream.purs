module Data.Stream where

import Prelude

import Data.Array (uncons, (:))
import Data.Enum (class Enum, succ)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (Tuple(..))

-- | `StreamF m a s` represents a stream of type `s` producing values of type `a`
-- in a monadic context provided by `m`.
newtype StreamF m a s =
  StreamF {step :: s -> m (Step s a), currentState :: s}

-- | An incremental step yielded by the stream
data Step s a = Yield a s
              | Skip s
              | Done

instance functorStep :: Functor (Step s) where
  map f (Yield a s) = Yield (f a) s
  map _ (Skip s) = Skip s
  map _ Done = Done

instance foldableStep :: Foldable (Step s) where
  foldl f init st = case st of
    Yield a s -> f init a
    _ -> init
  foldr f init st = case st of
    Yield a s -> f a init
    _ -> init
  foldMap f st = case st of
    Yield a _ -> f a
    _ -> mempty

instance traversableStep :: Traversable (Step s) where
  traverse f st = case st of
    Yield a s -> map (\b -> Yield b s) (f a)
    Skip s' -> pure (Skip s')
    Done -> pure Done
  sequence = sequenceDefault

-- | `StreamT m a` represents a stream of values of type `a` in a context provided by `m`
newtype StreamT m a = StreamT (Exists (StreamF m a))

-- | `StreamT` specialized to the identity context
type Stream a = StreamT Identity a

instance newtypeStream :: Newtype (StreamT m a) (Exists (StreamF m a))  where
  wrap = StreamT
  unwrap (StreamT e) = e

instance functorStream :: Monad m => Functor (StreamT m) where
  map = mapStream

-- | Convert an array into a stream
stream :: forall m a . Monad m => Array a -> StreamT m a
stream as = wrap <<< mkExists $
    StreamF { step: pure <<< next
            , currentState: as
            }
  where
    next :: Array a -> Step (Array a) a
    next as' = case uncons as' of
      Nothing -> Done
      Just {head, tail} -> Yield head tail

-- | Convert a stream into an array
unstream :: forall m a . Monad m => StreamT m a -> m (Array a)
unstream as = runExists unstream' (unwrap as)
  where
    unstream' :: forall s . StreamF m a s -> m (Array a)
    unstream' (StreamF strm) = do
      st <- strm.step strm.currentState
      case st of
        Done -> pure []
        Skip s' -> unstream' $ StreamF strm {currentState = s'}
        Yield a s' -> (:) a <$> unstream' (StreamF strm {currentState = s'})

instance foldableStream :: Foldable (StreamT Identity) where
  foldr f init as = (\(Identity a) -> a) $ foldr f init as
  foldl f init as = (\(Identity a) -> a) $ foldl f init as
  foldMap f as = (\(Identity a) -> a) $ foldMap f as

enumStream :: forall m a b .
                Monad m
           => Enum a
           => Tuple a a
           -> StreamT m a
enumStream (Tuple start stop) =
    wrap <<< mkExists $ StreamF {step: pure <<< step', currentState: Just start}
  where
    step' ms = case ms of
      Nothing -> Done
      Just s -> if s > stop
                  then Done
                  else Yield s $ succ s

mapStream :: forall m a b . Monad m => (a -> b) -> StreamT m a -> StreamT m b
mapStream f as = runExists mapStream' $ unwrap as
  where
    mapStream' :: forall s. StreamF m a s -> StreamT m b
    mapStream' (StreamF strm) = wrap <<< mkExists $
      StreamF strm { step = map (map f) <<< strm.step
                   }

mapM :: forall m a b . Monad m => (a -> m b) -> StreamT m a -> StreamT m b
mapM f as = runExists mapMStream $ unwrap as
  where
    mapMStream :: forall s . StreamF m a s -> StreamT m b
    mapMStream (StreamF strm) = wrap <<< mkExists $
      StreamF strm {step = join <<< map (traverse f) <<< strm.step}

foldl :: forall m a b. Monad m => (b -> a -> b) -> b -> StreamT m a -> m b
foldl f init as = runExists (foldlStream init) $ unwrap as
  where
    foldlStream :: forall s . b -> StreamF m a s -> m b
    foldlStream b (StreamF strm) = do
      st <- strm.step strm.currentState
      case st of
        Done -> pure b
        Skip s' -> foldlStream b (StreamF strm {currentState = s'})
        Yield a s' -> foldlStream (f b a) (StreamF strm {currentState = s'})

foldr :: forall m a b. Monad m => (a -> b -> b) -> b -> StreamT m a -> m b
foldr f init as = runExists (foldrStream $ pure init) $ unwrap as
  where
    foldrStream :: forall s . m b -> StreamF m a s -> m b
    foldrStream mb (StreamF strm) = do
      st <- strm.step strm.currentState
      case st of
        Done -> mb
        Skip s' -> foldrStream mb (StreamF strm {currentState = s'})
        Yield a s' -> f a <$> foldrStream mb (StreamF strm {currentState = s'})

foldMap :: forall m a b .
           Monad m
           => Monoid b
           => (a -> b)
           -> StreamT m a
           -> m b
foldMap f as = runExists (foldMapStream mempty) $ unwrap as
  where
    foldMapStream :: forall s . b -> StreamF m a s -> m b
    foldMapStream acc (StreamF strm) = do
      st <- strm.step strm.currentState
      case st of
        Done -> pure acc
        Skip s' -> foldMapStream acc (StreamF strm {currentState = s'})
        Yield a s' -> foldMapStream (acc  <> f a) (StreamF strm {currentState = s'})

hoist :: forall m n a. (m ~> n) -> StreamT m a -> StreamT n a
hoist phi as = runExists hoistStream $ unwrap as
  where
    hoistStream :: forall s . StreamF m a s -> StreamT n a
    hoistStream (StreamF strm) = wrap <<< mkExists $ StreamF strm {step = phi <<< strm.step}
