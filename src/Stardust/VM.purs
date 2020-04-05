module Stardust.VM where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, forever)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Debug.Trace (spy)
import Stardust.Builder (Register(..), instructionPointer)
import Stardust.IO (Port(..))
import Stardust.Instructions (Instruction(..))

data DataSize = Byte | DoubleWord | QuadWord

instance showDataSize :: Show DataSize where
  show Byte = "Byte"
  show DoubleWord = "DoubleWord"
  show QuadWord = "QuadWord"

data Interrupt = Reset | VRef | IO | Invalid

type Registers m = { register :: Register -> m Int, setRegister :: Register -> Int -> m Unit }
type Memory m = { read ::  DataSize -> Int -> m Int, write :: DataSize -> Int -> Int -> m Unit }
type Evaluator m = { registers :: Registers m, memory :: Memory m, next :: m (Instruction Int Int Int), ports :: Map Int (Port m), pendingInterrupt :: m Boolean  }

jmp :: forall m. Monad m => Registers m -> Int -> m Unit
jmp {setRegister} v = void $ setRegister (Register 31) v

swp :: forall m. Monad m => Registers m -> Register -> Register -> m Unit
swp {register, setRegister} a b = do
  a' <- register a
  b' <- register b
  setRegister b a'
  setRegister a b'

put :: forall m. Monad m => Registers m -> Memory m -> DataSize -> Register -> Register -> m Unit
put {register} {write} sz from to = do
  from' <- register from
  register to >>= write sz from'

get :: forall m. Monad m => Registers m -> Memory m -> DataSize -> Register -> Register -> m Unit
get {register, setRegister} {read} sz from to = do
  from' <- register from
  read sz from' >>= setRegister to

val :: forall m. Monad m => Registers m -> DataSize -> Register -> Int -> m Unit
val {setRegister} _ register value = setRegister register value 

handleInstruction :: forall m. Monad m => Registers m -> Memory m -> m Unit -> Instruction Int Int Int -> m Unit
handleInstruction regs mem exit insn = case insn of
  Hlt -> exit
  Jmp addr -> jmp regs addr
  Val a b -> val regs QuadWord (Register a) b
  Swp a b -> swp regs (Register a) (Register b)
  Put a b -> put regs mem QuadWord (Register a) (Register b)
  Get a b -> get regs mem QuadWord (Register a) (Register b)
  _ -> interrupt regs mem Invalid

vector :: forall m. Monad m => Registers m -> Memory m -> Int -> m Unit
vector {setRegister} {read} addr = do
  vec <- read DoubleWord addr
  setRegister instructionPointer vec

interrupt :: forall m. Monad m => Registers m -> Memory m -> Interrupt -> m Unit
interrupt regs mem Reset = vector regs mem 0xffc0
interrupt regs mem VRef = vector regs mem 0xffc2
interrupt regs mem IO = vector regs mem 0xffc4
interrupt regs mem Invalid = vector regs mem 0xffc6

-- Map an address to either a port or a
-- location in physical memory
mmap :: forall m. Evaluator m -> Memory m
mmap { ports, memory: { write, read } } = { read: mmapRead, write: mmapWrite }
  where
    mmapRead sz address = do
      let port = Map.lookup address ports
      maybe (read sz address) (\(Port r _) -> r) port
    mmapWrite sz address value = do
      let port = Map.lookup address ports
      maybe (write sz address value) (\(Port _ w) -> w value) port

start :: forall m. MonadRec m => Evaluator m -> m Unit
start e@{ next, registers, memory } = do
  let memoryMap = mmap e
  interrupt registers memoryMap Reset
  forever do
    next >>= handleInstruction registers memoryMap (pure unit)
    processInterrupts $ e { memory = memoryMap }

processInterrupts :: forall m. Monad m => Evaluator m -> m Unit
processInterrupts { pendingInterrupt, registers, memory } = do
  whenM pendingInterrupt do
    interrupt registers memory IO