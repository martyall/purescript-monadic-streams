module Data.Stream where

data Stream a = Stream forall s . {step :: s -> Step a s, initalState :: s}

data Step a s = Yield a s
              | Skip    s
              | Done
