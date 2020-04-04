module Stardust.Builder where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Stardust.Instructions (Instruction(..))

class Builder m where
  label :: String -> m Unit
  location :: String -> m Int
  insn :: Instruction Argument Argument Argument -> m Unit

instructionPointer :: Register
instructionPointer = Register 31
newtype Register = Register Int
derive newtype instance showRegister :: Show Register

data Argument = R Register | C Int | L String

instance showArgument :: Show Argument where
  show (R reg) = "(Register " <> show reg <> ")"
  show (C i) = "(Constant " <> show i <> ")"
  show (L j) = j

register :: Int -> Register
register = Register

val :: forall m. Builder m => Register -> Int -> m Unit
val reg v = insn $ Val (R reg) (C v)

jmp :: forall m. Builder m => Register -> m Unit
jmp reg = insn $ Jmp $ R reg

infinite :: forall m. Monad m => Builder m => m Unit
infinite = do
  loop <- location "loop"
  val (register 0) loop
  label "loop"
  jmp $ register 0

newtype Print a = Print (Effect a)
derive instance newtypePrint :: Newtype (Print a) _
derive newtype instance applicativePrint :: Applicative Print
derive newtype instance monadPrint :: Monad Print

instance printBuilder :: Builder Print where
  label s = Print $ log $ s <> ":"
  location s = pure 0
  insn i = Print $ logShow i