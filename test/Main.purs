module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log, logShow)

import Data.Array ((..))
import Data.Tuple (Tuple(..))
import Data.Set.Monad (Set, fromArray)

main :: Effect Unit
main = do
  -- Set Monad
  let s1 :: Set (Tuple Int Int)
      s1 = do a <- fromArray $ 1 .. 4
              b <- fromArray $ 1 .. 4
              pure (Tuple a b)
  logShow s1  
  -- Set Applicative (pure)
  let s2 :: Set Int
      s2 = pure $ 1
  logShow s2
  -- Set Applicative (pure & ap)
  let s3 :: Set (Int -> Int)
      s3 = pure $ identity
  logShow $ s3 <*> s2
  log "🍝"
