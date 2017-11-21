module Data.Stream where

import Prelude

import Data.Array (uncons, (:))
import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe(..))

data StreamF a s = StreamF {step :: s -> Step a s, initialState :: s}

data Step a s = Yield a s
              | Skip    s
              | Done

data Stream a = Stream (Exists (StreamF a))

instance functorStream :: Functor Stream where
  map = mapStream

stream :: forall a . Array a -> Stream a
stream as = Stream <<< mkExists $
    StreamF { step: next
            , initialState: as
            }
  where
    next :: Array a -> Step a (Array a)
    next as' = case uncons as' of
      Nothing -> Done
      Just {head, tail} -> Yield head tail

unstream :: forall a . Stream a -> Array a
unstream as = runExists unstream' (unpackStream as)
  where
    unstream' :: forall s . StreamF a s -> Array a
    unstream' (StreamF stream) = case stream.step stream.initialState of
      Done -> []
      Skip s' -> unstream' $ StreamF stream {initialState = s'}
      Yield a s' -> a : unstream' (StreamF stream {initialState = s'})

--------------------------------------------------------------------------------

unpackStream :: forall a . Stream a -> Exists (StreamF a)
unpackStream (Stream a) = a

mapStream :: forall a b . (a -> b) -> Stream a -> Stream b
mapStream f as = runExists mapStream' $ unpackStream as
  where
    mapStream' :: forall s. StreamF a s -> Stream b
    mapStream' (StreamF stream) = Stream <<< mkExists $
      StreamF stream { step = \s -> case stream.step s of
                         Done -> Done
                         Skip s' -> Skip s'
                         Yield la s' -> Yield (f la) s'
                     }
