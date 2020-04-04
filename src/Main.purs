module Main where

import Prelude

import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.State (evalStateT, runStateT)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Graphics.Canvas (CanvasElement, Context2D, Dimensions)
import Graphics.Canvas as Canvas
import Stardust.Builder (Print, infinite)
import Stardust.Debug as Debug
import Stardust.Eval as Eval
import Web.HTML (window)
import Web.HTML.Window (Window)
import Web.HTML.Window as Window

dimensions :: Window -> Effect Dimensions
dimensions window = do
  height <- Window.innerHeight window <#> toNumber
  width <- Window.innerWidth window <#> toNumber
  pure { height, width }

prepareScreen :: Effect (Tuple Context2D CanvasElement)
prepareScreen = do
  elem <- Canvas.getCanvasElementById "mainCanvas" >>= maybe (throw "Unable to get mainCanvas element") (pure)
  ctx <- Canvas.getContext2D elem
  dim@{ width, height } <- window >>= dimensions
  Canvas.setCanvasDimensions elem dim
  Canvas.setFillStyle ctx "black"
  Canvas.fillRect ctx $ { x: 0.0, y: 0.0, width, height }
  pure $ Tuple ctx elem

main :: Effect Unit
main = do
  void $ runMaybeT $ evalStateT (unwrap $ Eval.run (Debug.nextInstruction)) (Debug.VMState { ticks: 0 })