module Nova.Stack where

import Prelude

import Data.ByteString (ByteString)
import Data.ByteString as ByteString
import Data.Map as Map
import Data.Ord (abs)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Nova.Operation (Insn)
import Stardust.VM (DataSize)
import Type.Quotient (runQuotient)

data Type = UInt DataSize 
data Value = C Int | L String | B Int
data Variable = Local Int | Parameter Int | Global String

data Opcode
  = Call Int
  | Load Variable
  | Set Variable
  | Pointer Variable
  | SetPtr
  | LoadPtr
  | Push ByteString
  | Pop
  | Dup
  | Swp

type Thunk = Array Opcode

data Chunk = Chunk Int Int Thunk

input :: Chunk -> Int
input (Chunk i _ _) = i

output :: Chunk -> Int
output (Chunk _ o _) = o

instance semigroupChunk :: Semigroup Chunk where
  append (Chunk in' out' body') (Chunk in'' out'' body'') =
    let realIn = in' + abs (out' - in'') in
    let realOut = out'' in
    Chunk realIn realOut $ body' <> body''

instance monoidChunk :: Monoid Chunk where
  mempty = Chunk 0 0 []

data Definition
  = Function { params :: Int, local :: Int, returns :: Int, body :: Thunk }
  | Value { value :: Value }

newtype Named = Named { def :: Definition, name :: String }

type Program = Array Named

parametersUsed :: Thunk -> Array Int
parametersUsed thunk = thunk >>= \op -> case op of
  (Load (Parameter i)) -> [i]
  _ -> []

localsUsed :: Thunk -> Array Int
localsUsed thunk = thunk >>= \op -> case op of
  (Load (Local i)) -> [i]
  _ -> []

functionalize :: (Int -> Int) -> (Int -> Int) -> Thunk -> Thunk
functionalize localToParam paramToParam t = t <#> \op ->
  case op of
    (Load (Local i)) -> Load $ Parameter $ localToParam i
    (Load (Parameter i)) -> Load $ Parameter $ paramToParam i
    _ -> op

printVar :: Variable -> String
printVar (Global s) = "global " <> s
printVar (Local i) = "local " <> show i
printVar (Parameter i) = "parameter " <> show i

printOpcode :: Opcode -> String
printOpcode (Call n) = "call " <> show n
printOpcode (Load var) = "load " <> printVar var
printOpcode (Push bs) = "push [" <> joinWith ", " (ByteString.unpack bs <#> runQuotient >>> show) <> "]"
printOpcode (Set var) = "set " <> printVar var
printOpcode LoadPtr = "load*"
printOpcode SetPtr = "set*"
printOpcode (Pointer var) = "ptr " <> printVar var
printOpcode Pop = "pop"
printOpcode Dup = "dup"
printOpcode Swp = "swp"

printValue :: Value -> String
printValue (C i) = show i
printValue (L l) = l
printValue (B n) = "[" <> show n <> "]"

printNamed :: Named -> String
printNamed (Named { name, def: Function { params, returns, local, body } }) =
  joinWith "\n" $ [
    name <> "(" <> show params <> ", " <> show local <> ", " <> show returns <> ")"
  ] <> (body <#> printOpcode <#> \s -> "  " <> s)
printNamed (Named { name, def: Value { value }}) = name <> " = " <> printValue value

printProgram :: Program -> String
printProgram = joinWith "\n" <<< map printNamed