module Stardust.Debug where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.State (class MonadState, StateT, get, gets, modify_, put)
import Control.Plus (class Plus, empty)
import Data.Array as Array
import Data.Char (fromCharCode)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.String.CodeUnits as String
import Data.Typelevel.Num (class Lt, class Nat, D32, d31, d32)
import Data.Vec (Vec)
import Data.Vec as Vec
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throw)
import Stardust.Builder (Register(..))
import Stardust.IO (Port(..))
import Stardust.Instructions (Instruction(..))
import Stardust.VM (Evaluator)

newtype VMState = VMState { registers :: Vec D32 Int }

instance vmStateSemigroup :: Semigroup VMState where
  append = flip const

instance vmStateMonoid :: Monoid VMState where
  mempty = VMState { registers: Vec.replicate d32 0 }

registers :: VMState -> Vec D32 Int
registers (VMState { registers:r }) = r

register :: forall n. Nat n => Lt n D32 => n -> VMState -> Int
register idx state = Vec.index (registers state) idx

setRegister :: forall n. Nat n => Lt n D32 => n -> Int -> VMState -> VMState
setRegister idx v state =
  let newRegisters = Vec.updateAt idx v (registers state) in
  VMState { registers: newRegisters }

register' :: Int -> VMState -> Maybe Int
register' idx state = Vec.index' (registers state) idx

setRegister' :: Int -> Int -> VMState -> Maybe (VMState)
setRegister' idx v state = do
  newArray <- Array.updateAt idx v $ Vec.toArray $ registers state
  newRegisters <- Vec.fromArray newArray
  pure $ VMState { registers: newRegisters }

consoleOut :: forall m. MonadEffect m => Port m
consoleOut = Port (pure 0) \i -> liftEffect $ maybe (pure unit) (\c -> log $ String.singleton c) $ fromCharCode i

nextInstruction :: forall m. MonadState VMState m => m (Instruction Int Int Int)
nextInstruction = do
  ip <- gets $ register d31
  modify_ $ setRegister d31 $ ip + 1
  pure $ Get 0 0

evaluator :: forall m. MonadState VMState m => Plus m => MonadEffect m => Evaluator m
evaluator = {
  exit: empty,
  next: nextInstruction,
  pendingInterrupt: pure false,
  ports: Map.singleton 0xffe2 consoleOut,
  registers: {
    register: \(Register r) -> do
      state <- get
      maybe (liftEffect $ throw $ "Unable to get register " <> show r) pure $ register' r state
    ,
    setRegister: \(Register r) v -> do
      state <- get
      maybe (liftEffect $ throw $ "Unable to set register " <> show r <> " = " <> show v) put $ setRegister' r v state
  },
  memory: {
    write: \sz addr value -> do
      pure unit,
    read: \sz addr -> do
      pure 0
  }
}