module Stardust.Eval where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, forever, untilJust)
import Control.Plus (empty)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Stardust.Builder (Register(..), instructionPointer)
import Stardust.IO (Device(..))
import Stardust.Instructions (Instruction(..))
import Stardust.VM (class VM, DataSize(..), Interrupt(..), events, exit, get, jmp, put, read, setRegister, swp, tick, ticks)

run :: forall m. VM m => MonadRec m => m (Instruction Int Int Int) -> m Unit
run next = runInterrupt next Reset

runWithInstruction :: forall m. VM m => MonadRec m => Instruction Int Int Int -> m (Instruction Int Int Int) -> m Unit
runWithInstruction insn next = do
  handle insn
  tick
  -- externalInterrupts next
  -- tick

step :: forall m. VM m => MonadRec m => m (Instruction Int Int Int) -> m Unit
step next = next >>= \i -> runWithInstruction i next

handle :: forall m. VM m => Instruction Int Int Int -> m Unit
handle insn = case insn of
  Hlt -> exit
  Jmp addr -> jmp addr
  Swp a b -> swp (Register a) (Register b)
  Put a b -> put QuadWord (Register a) (Register b)
  Get a b -> get QuadWord (Register a) (Register b)
  _ -> interrupt Invalid

interrupt :: forall m. VM m => Interrupt -> m Unit
interrupt Reset = do
  resetVector <- read DoubleWord 0xffc0
  tick
  setRegister instructionPointer resetVector
  tick
interrupt VRef = do
  vrefVector <- read DoubleWord 0xffc2
  tick
  setRegister instructionPointer vrefVector
  tick
interrupt IO = do
  ioVector <- read DoubleWord 0xffc4
  tick
  setRegister instructionPointer ioVector
  tick
interrupt Invalid = do
  invalidVector <- read DoubleWord 0xffc6
  tick
  setRegister instructionPointer invalidVector
  tick

runInterrupt :: forall m. VM m => MonadRec m => m (Instruction Int Int Int) -> Interrupt -> m Unit
runInterrupt next i = do
  -- TODO save register state
  interrupt i
  tick
  -- Run the interrupt until we reach a `ret`
  untilJust do
    insn <- next
    tick
    case insn of
      Ret -> pure $ Just unit
      _ -> runWithInstruction insn next <#> const Nothing
  -- Restore registers
  pure unit

externalInterrupts :: forall m. VM m => MonadRec m => m (Instruction Int Int Int) -> m Unit
externalInterrupts next = do
  pendingEvents <- events
  traverse_ (maybe (pure unit) (const $ runInterrupt next IO)) pendingEvents