module Nova.Language where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (Except, throwError)
import Control.Monad.State (StateT(..), modify, modify_)
import Data.Array (fold)
import Data.Array as Array
import Data.ByteString.Encode (int32be)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (abs)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Nova.Stack (Definition(..), Opcode(..), Thunk, Value(..), Variable(..)) as S
import Nova.Stack (Chunk(..), input, output)

data Expression
  = Apply Expression (Array Expression)
  | Lambda (Array String) Expression
  | Def String Expression
  | Var String
  | Do (Array Expression)
  | Const Int

type CompilerState = { fresh :: Int, definitions :: Map String S.Definition }
type Compiler a = StateT CompilerState (Except String)  a

data Entry = Name S.Variable | Special (Env -> Array Expression -> Compiler Chunk) 

type Env = String -> Maybe Entry

variable :: Env -> String -> Maybe S.Variable
variable env name = env name >>= \s -> case s of
  Name var' -> Just var'
  _ -> Nothing

runChunk :: Chunk -> S.Thunk
runChunk (Chunk in' out' vals) = vals <> Array.replicate out' S.Pop

definition :: String -> S.Definition -> Compiler Unit
definition name def = modify_ \s -> s { definitions = Map.insert name def s.definitions }

defun :: Env -> String -> Array String -> Expression -> Compiler Unit
defun env name args body = do
  let params' = args <#> obfuscate
  let env' = \ident -> if Array.elem ident params'
    then Array.elemIndex ident params' <#> (S.Parameter >>> Name)
    else env ident
  Chunk in' out' body' <- compile env' body
  definition name $ S.Function { local: 0, returns: 1, params: Array.length args, body: body' }

obfuscate :: String -> String
obfuscate a = "_" <> (String.replaceAll (Pattern " ") (Replacement "_") a) <> "_"

fresh :: Compiler String
fresh = do
  new <- modify \s -> s { fresh = s.fresh + 1 }
  pure $ "_label_" <> show (new.fresh) <> "_"

compile :: Env -> Expression -> Compiler Chunk
compile env (Const i) = constant i
compile env (Def name (Lambda args body)) = defineFun env name args body
compile env (Def name expr) = define env name expr
compile env (Lambda args body) = lambda env args body
compile env (Apply fn args) = apply env fn args
compile env (Var x) = var env x
compile env (Do exprs) = do' env exprs

let' :: Env -> String -> Expression -> Expression -> Compiler Chunk
let' env name value body = do
  pure mempty

do' :: Env -> Array Expression -> Compiler Chunk
do' env exprs = do
  thunks <- traverse (compile env) exprs
  let body' = fold $ thunks <#> runChunk
  pure $ Chunk 0 0 body'

var :: Env -> String -> Compiler Chunk
var env name = do
  let name' = obfuscate name
  let var' = variable env name'
  maybe (throwError $ "Unable to find identifier " <> name) (pure <<< Chunk 0 1 <<< Array.singleton <<< S.Load) var'

defineFun :: Env -> String -> Array String -> Expression -> Compiler Chunk
defineFun env name params expr = do
  let name' = obfuscate name
  defun env name' params expr
  pure $ Chunk 0 1 [
    S.Load $ S.Global name'
  ]

define :: Env -> String -> Expression -> Compiler Chunk
define env name expr = do
  let name' = obfuscate name
  definition name' $ S.Value { value: S.B 4 }
  initializer' <- compile env expr
  pure $ initializer' <> Chunk 1 0 [
    S.Set $ S.Global name'
  ]

constant :: Int -> Compiler Chunk
constant i = pure $ Chunk 0 1 [S.Push $ int32be i]

call :: Env -> Expression -> Array Expression -> Compiler Chunk
call env fn' args = do
  fn <- compile env fn'
  -- FIXME this wont work with zero-argument functions
  args' <- traverse (compile env) args
  pure $ fn <> fold args' <> Chunk (Array.length args + 1) (output fn) [
    S.Call $ Array.length args
  ]

apply :: Env -> Expression -> Array Expression -> Compiler Chunk
apply env (Var fn') args = do
  entry <- maybe (throwError $ "No variable " <> fn') pure $ env (obfuscate fn')
  case entry of
    Special fn -> fn env args
    Name _ -> call env (Var fn') args
apply env fn args = call env fn args


lambda :: Env -> Array String -> Expression -> Compiler Chunk
lambda env params body = do
  name <- fresh
  defun env name params body
  pure $ Chunk 0 1 [
    S.Load $ S.Global name
  ]

definitions :: Expression -> Array String
definitions (Def name expr) = [obfuscate name] <> definitions expr
definitions (Lambda _ body) = definitions body
definitions (Apply fn' args) = definitions fn' <> (fold $ args <#> definitions)
definitions (Do exprs) = exprs >>= definitions
definitions _ = []

ref' :: Env -> Array Expression -> Compiler Chunk
ref' env [Var var'] = do
  let var'' = variable env $ obfuscate var'
  maybe (throwError "Cannot @ref non-variable") (\s -> pure $ Chunk 0 1 [S.Pointer s]) var''
ref' _ _ = throwError "Invalid @ref usage"

set' :: Env -> Array Expression -> Compiler Chunk
set' env [a, b] = do
  a' <- compile env a
  b' <- compile env b
  pure $ a' <> b' <> Chunk 2 0 [S.SetPtr]
set' _ _ = throwError "Invalid @set usage"

get' :: Env -> Array Expression -> Compiler Chunk
get' env [a] = do
  a' <- compile env a
  pure $ a' <> Chunk 1 1 [S.LoadPtr]
get' _ _ = throwError "Invalid @get usage"

specialForms :: Map String (Env -> Array Expression -> Compiler Chunk)
specialForms = Map.fromFoldable [
  -- FIXME obfuscation at the env level
  Tuple (obfuscate "@ref") ref',
  Tuple (obfuscate "@set") set',
  Tuple (obfuscate "@get") get'
]

compileTopLevel :: Env -> Array Expression -> Compiler Unit
compileTopLevel env exprs = do
  let definitions' = fold $ exprs <#> definitions
  let defMap = Map.fromFoldable $ definitions' <#> \def -> Tuple def $ S.Global def
  let special = \ident -> Map.lookup ident specialForms <#> Special
  let globals = \ident -> Map.lookup ident defMap <#> Name
  let env' = \i -> special i <|> globals i <|> env i
  mainChunk <- do' env' exprs
  let body' = runChunk mainChunk
  definition "main" $ S.Function { params: 0, returns: 0, local: 0, body: body' }