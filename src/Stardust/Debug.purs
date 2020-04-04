module Stardust.Debug where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, State, StateT(..), gets, modify_)
import Control.MonadPlus (class Alt, class MonadPlus, class Plus)
import Control.Plus (empty)
import Data.Newtype (class Newtype)
import Data.Ord (greaterThan)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Stardust.Instructions (Instruction(..))
import Stardust.VM (class VM, ticks)

newtype VMState = VMState {
  ticks :: Int
}

newtype DebugVM a = DebugVM (StateT VMState (MaybeT Effect) a)

derive instance newtypeDebug :: Newtype (DebugVM a) _
derive newtype instance applicativeDebug :: Applicative DebugVM
derive newtype instance monadEffectDebug :: MonadEffect DebugVM
derive newtype instance bindDebug :: Bind DebugVM
derive newtype instance monadDebug :: Monad DebugVM
derive newtype instance monadStateDebug :: MonadState VMState DebugVM
derive newtype instance monadRecDebug :: MonadRec DebugVM
derive newtype instance functorDebug :: Functor DebugVM
derive newtype instance applyDebug :: Apply DebugVM
derive newtype instance altDebug :: Alt DebugVM
derive newtype instance monadPlusDebug :: Plus DebugVM

nextInstruction :: DebugVM (Instruction Int Int Int)
nextInstruction = pure $ Get 0 0

doTick :: DebugVM Unit
doTick = do
  tickCount <- gets \(VMState { ticks: t}) -> t
  modify_ \(VMState s) -> VMState (s { ticks = s.ticks + 1 })

instance vmDebug :: VM DebugVM where
  ticks = gets \(VMState { ticks: t }) -> t
  tick = doTick
  events = do
    log "Getting events"
    pure []
  exit = log "Exiting" *> empty
  write sz a b = log ("Write " <> show sz <> " " <> show a <> " " <> show b) *> pure unit
  read sz a = log ("Read " <> show sz <> " " <> show a) *> pure 0
  register = const (log "ReadRegister" *> pure 0)
  setRegister = const $ const (log "SetRegister" *> pure unit)