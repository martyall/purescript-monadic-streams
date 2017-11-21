module Data.Stream where

import Prelude

import Data.Array (uncons, (:))
import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)


data StreamT m a s = StreamT {step :: s -> Step m a s, initialState :: s}

data Step m a s = Yield (m a) s
                | Skip s
                | Done

data Stream m a = Stream (Exists (StreamT m a))

instance functorStream :: Functor m => Functor (Stream m) where
  map = mapStream

stream :: forall m a . Applicative m => Array a -> Stream m a
stream as = Stream <<< mkExists $
    StreamT { step: next
            , initialState: as
            }
  where
    next :: Array a -> Step m a (Array a)
    next as' = case uncons as' of
      Nothing -> Done
      Just {head, tail} -> Yield (pure head) tail

unstream :: forall m a . Applicative m => Stream m a -> m (Array a)
unstream as = sequence $ runExists unstream' (unpackStream as)
  where
    unstream' :: forall s . StreamT m a s -> Array (m a)
    unstream' (StreamT stream) = case stream.step stream.initialState of
      Done -> []
      Skip s' -> unstream' $ StreamT stream {initialState = s'}
      Yield ma s' -> ma : unstream' (StreamT stream {initialState = s'})

--------------------------------------------------------------------------------

unpackStream :: forall m a . Stream m a -> Exists (StreamT m a)
unpackStream (Stream a) = a

mapStream :: forall m a b . Functor m => (a -> b) -> Stream m a -> Stream m b
mapStream f as = runExists mapStream' $ unpackStream as
  where
    mapStream' :: forall s. StreamT m a s -> Stream m b
    mapStream' (StreamT stream) = Stream <<< mkExists $
      StreamT stream { step = \s -> case stream.step s of
                         Done -> Done
                         Skip s' -> Skip s'
                         Yield la s' -> Yield (f <$> la) s'
                     }
