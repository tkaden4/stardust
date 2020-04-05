module Stardust.Debug where

import Prelude

import Control.Monad.State (StateT, gets, modify_)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throw)
import Stardust.Builder (Register(..))
import Stardust.IO (Port(..))
import Stardust.Instructions (Instruction(..))
import Stardust.VM (Evaluator)

newtype VMState = VMState { registers :: Array Int }

instance vmStateSemigroup :: Semigroup VMState where
  append (VMState s) (VMState k) = VMState k

instance vmStateMonoid :: Monoid VMState where
  mempty = VMState { registers: Array.replicate 32 0 }

logger :: forall m. MonadEffect m => Port m
logger = Port (log "Read" *> pure 0) (const $ log "Write")

evaluator :: Evaluator (StateT VMState Effect)
evaluator = {
  next: pure $ Get 0 0,
  pendingInterrupt: pure true,
  ports: Map.singleton 0xEEEE logger,
  registers: {
    register: \(Register r) -> do
      regs <- gets \(VMState { registers }) -> registers
      let value = Array.index regs r 
      maybe (liftEffect $ throw "Unable to get register") pure value,
    setRegister: \(Register r) v -> do
      regs <- gets \(VMState { registers }) -> registers
      let newArray = Array.updateAt r v regs
      maybe (liftEffect $ throw "Unable to set register") (\arr -> modify_ \(VMState s) -> VMState $ s { registers = arr }) newArray
  },
  memory: {
    write: \sz _ _ -> do
      pure unit,
    read: \sz _ -> pure 0
  }
}