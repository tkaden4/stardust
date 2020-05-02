module Stardust.Instructions where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Instruction a b c
  = Hlt
  | Ret
  | Jmp a
  | Jeq a b c
  | Jne a b c
  | Jlt a b c
  | Jgt a b c
  | Swp a b
  | Val a b
  | Mov a b
  | Put a b
  | Get a b
  | Psh a
  | Pop a
  | Add a b c
  | Sub a b c
  | Div a b c
  | Mul a b c
  | Mod a b c
  | And a b c
  | Ior a b c
  | Xor a b c

derive instance genericInstruction :: Generic (Instruction a b c) _

instance showInstruction :: (Show a, Show b, Show c) => Show (Instruction a b c) where
  show x = genericShow x