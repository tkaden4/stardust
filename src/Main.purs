module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.State (evalStateT, execStateT, runStateT)
import Data.Either (either)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throw)
import Graphics.Canvas (CanvasElement, Context2D, Dimensions)
import Graphics.Canvas as Canvas
import Nova.Language (Expression(..), compile, compileTopLevel)
import Nova.Stack (Named(..), printNamed, printProgram)
import Stardust.Debug as Debug
import Stardust.VM as VM
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

sampleProgram :: Array Expression
sampleProgram = [
  Def "const" (Lambda ["x", "y"] (Var "x")),
  Def "id" (Lambda ["x"] (Var "x")),
  Def "x" (Var "y"),
  Apply (Var "const") [Var "id", Const 0x02],
  Apply (Lambda [] $ Const 2) [Const 0xffff],
  Def "yptr" $ Apply (Var "@ref") [Var "y"],
  Apply (Var "@set") [Var "yptr", Const 0xff],
  Apply (Var "id") [Apply (Var "@get") [Var "yptr"]],
  Def "y" (Apply (Var "id") [Const 0xff00])
]

main :: Effect Unit
main = do
  -- void $ runMaybeT $ evalStateT (VM.start Debug.evaluator) mempty
  let compiled = compileTopLevel (const Nothing) sampleProgram
  let result = runExcept $ execStateT compiled { fresh: 0, definitions: Map.empty }
  either logShow (\s -> log $ printProgram $ foldMapWithIndex (\n d -> [Named { name: n, def: d}]) s.definitions) result
