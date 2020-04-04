module Stardust.VM where

import Prelude

import Data.Maybe (Maybe)
import Stardust.Builder (Register(..))

data DataSize = Byte | DoubleWord | QuadWord

instance showDataSize :: Show DataSize where
  show Byte = "Byte"
  show DoubleWord = "DoubleWord"
  show QuadWord = "QuadWord"

data Interrupt = Reset | VRef | IO | Invalid

class Monad m <= VM m where
  register :: Register -> m Int
  setRegister :: Register -> Int -> m Unit
  read :: DataSize -> Int -> m Int
  write :: DataSize -> Int -> Int -> m Unit
  exit :: m Unit
  tick :: m Unit
  ticks :: m Int
  events :: m (Array (Maybe Unit))

halt :: forall m. VM m => m Unit
halt = tick *> exit

jmp :: forall m. VM m => Int -> m Unit
jmp v = void $ setRegister (Register 31) v <* tick

swp :: forall m. VM m => Register -> Register -> m Unit
swp a b = do
  a' <- register a
  tick
  b' <- register b
  tick
  setRegister b a'
  tick
  setRegister a b'
  tick

put :: forall m. VM m => DataSize -> Register -> Register -> m Unit
put sz from to = do
  from' <- register from
  tick
  register to >>= write sz from'
  tick

get :: forall m. VM m => DataSize -> Register -> Register -> m Unit
get sz from to = do
  from' <- register from
  tick
  read sz from' >>= setRegister to
  tick