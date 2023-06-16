{-# LANGUAGE ScopedTypeVariables #-}
{-|

This module provides a thin wrapper around the winter Wasm engine, exposing just
the bits needed by the IC ref.

This is the interface at which one might plug in a different Wasm engine.
-}
module IC.Wasm.Wasmtime
  ( Module
  , Instance
  , Extern
  , toExtern
  , getBytes
  , setBytes
  , invokeExport
  , invokeTable
  , HostM
  , HostFunc
  , ModName
  , FuncName
  , Import
  , Imports
  , ValueType(..)
  , Value(..)
  , StackType
  , initialize
  )
where

import qualified Data.ByteString.Lazy as BS
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.IntMap as IM
import qualified Data.Text.Lazy as T
import Control.Monad.ST
import Data.Binary.Get (runGetOrFail)
import Data.Default.Class (Default (..))
import Data.Int
import Data.Foldable
import Data.MemoUgly

import Control.Exception (Exception, throwIO)
import Control.Monad.Primitive (RealWorld)
import Wasmtime

data ValueType = I32Type | I64Type
data Value = I32 Int32 | I64 Int64
  deriving Show
type StackType = [ValueType]

type HostFunc s = HostM s [Value]
type HostM s = ExceptT String (ST s)

type ModName = String
type FuncName = String
type Import s = (ModName, FuncName, StackType, StackType, [Value] -> HostFunc s)
type Imports s = [Import s]

{-
type Instance s = (IM.IntMap (W.ModuleInst W.Phrase (ST s)), Int)

type HostM s = ExceptT String (ST s)

type HostFunc s = HostM s [W.Value]

type ModName = String
type Import s = (ModName, FuncName, W.StackType, W.StackType, [W.Value] -> HostFunc s)
type Imports s = [Import s]

type Module = W.Module W.Phrase
-}

handleException :: Exception e => Either e r -> IO r
handleException = either throwIO pure

hello :: IO (Either Trap ())
hello = error "not implemented"

initialize :: forall s. BS.ByteString -> Imports s -> IO (Instance RealWorld)
initialize wasm imps = do
  engine <- newEngine
  store <- newStore engine
  ctx <- storeContext store
  myModule <- handleException $ newModule engine (unsafeFromByteString $ BS.toStrict wasm)
  funcs <- mapM (\(mod, func, args, rets, f) -> newFunc ctx hello) imps -- TODO
  newInstance ctx myModule (V.fromList $ map toExtern funcs) >>= handleException

{-
  let by_mod :: [(T.Text, [(T.Text, W.StackType, W.StackType, [W.Value] -> HostFunc s)])]
      by_mod = M.toList $ M.fromListWith (<>)
        [ (T.pack m, [(T.pack n,t1,t2,f)]) | (m,n,t1,t2,f) <- imps ]

      names :: M.Map T.Text Int
      names = M.fromList (zip (map fst by_mod) [1..])

      mods :: IM.IntMap (W.ModuleInst W.Phrase (ST s))
      mods  = IM.fromList $ zip [1..]
        [ (W.emptyModuleInst def)
          { W._miGlobals  = mempty
          , W._miTables   = mempty
          , W._miMemories = mempty
          , W._miFuncs    = mempty
          , W._miExports  = M.fromList
            [ (,) fname $ W.ExternFunc $
              W.allocHostEff (W.FuncType arg_ty ret_ty)
                  (\ args -> runExceptT $ f args)
            | (fname, arg_ty, ret_ty, f) <- funcs
            ]
          }
        | (_name, funcs) <- by_mod
        ]
  (ref, inst, start_err) <- W.initialize mod names mods
  for_ start_err throwError
  let mods' = IM.insert ref inst mods
  return (mods', ref)


exportedFunctions :: Module -> [FuncName]
exportedFunctions wasm_mod =
  [ T.unpack (W._exportName e)
  | W.Phrase _ e <- V.toList $ W._moduleExports wasm_mod
  , W.FuncExport {} <- return $ W._exportDesc e
  ]
-}

invokeExport :: Instance s -> FuncName -> [Value] -> HostM s [Value]
invokeExport i method args = error "not implemented"

invokeTable :: Instance s -> Int32 -> [Value] -> HostM s [Value]
invokeTable i idx args = error "not implemented"

getBytes :: Instance s -> Int32 -> Int32 -> HostM s BS.ByteString
getBytes i ptr len = error "not implemented"

setBytes :: Instance s -> Int32 -> BS.ByteString -> HostM s ()
setBytes i ptr blob = error "not implemented"
