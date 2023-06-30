{-# LANGUAGE ScopedTypeVariables #-}
{-|

This module provides a thin wrapper around the winter Wasm engine, exposing just
the bits needed by the IC ref.

This is the interface at which one might plug in a different Wasm engine.
-}
module IC.Wasm.Winter
  ( Module
  , parseModule
  , exportedFunctions
  , Import
  , Imports
  , HostM
  , HostFunc
  , W.Value(..)
  , W.StackType
  , W.ValueType(..)
  , W.Address
  , W.Size
  , getBytes
  , setBytes
  , initialize
  , Instance
  , invokeExport
  , invokeTable
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
import Data.List (isPrefixOf)
import Data.Foldable
import Data.MemoUgly

import qualified Wasm.Binary.Decode as W
import qualified Wasm.Exec.Eval as W
import qualified Wasm.Runtime.Func as W
import qualified Wasm.Runtime.Instance as W
import qualified Wasm.Runtime.Memory as W
import qualified Wasm.Syntax.AST as W
import qualified Wasm.Syntax.Types as W
import qualified Wasm.Syntax.Values as W
import qualified Wasm.Syntax.Memory as W
import qualified Wasm.Util.Source as W

type Instance s = (IM.IntMap (W.ModuleInst W.Phrase (ST s)), Int)

type HostM s = ExceptT String (ST s)

type HostFunc s = HostM s [W.Value]

type ModName = String
type FuncName = String
type Import s = (ModName, FuncName, W.StackType, W.StackType, [W.Value] -> HostFunc s)
type Imports s = [Import s]

type Module = W.Module W.Phrase

-- This function is memoized using Data.MemoUgly. This optimizes for workloads
-- where the same module is parsed many times (ic-ref-test). It is wasteful when
-- a module is parsed, eventually dropped (i.e. canister deleted), and never installed
-- again.
parseModule :: BS.ByteString -> Either String Module
parseModule = memo $ \bytes -> do
    wasm_mod <- case runGetOrFail W.getModule bytes of Left (_, _, err) -> Left err
                                                       Right (_, _, wasm_mod) -> Right wasm_mod
    let tys = map (\w -> case w of W.Phrase _ (W.FuncType args res) -> (args, res)) $ V.toList $ W._moduleTypes wasm_mod
    let globs = V.toList $ W._moduleGlobals wasm_mod
    let funcs = map (\w -> case w of W.Phrase _ (W.Func (W.Phrase _ ty) _ _) -> ty) $ V.toList $ W._moduleFuncs wasm_mod
    let imps = [e | W.Phrase _ e <- V.toList $ W._moduleImports wasm_mod, W.FuncImport {} <- return $ W._importDesc e]
    let nimps = length imps
    let exps = [(T.unpack (W._exportName e), idx) | W.Phrase _ e <- V.toList $ W._moduleExports wasm_mod, W.FuncExport (W.Phrase _ idx) <- return $ W._exportDesc e]
    let maxFunctions = 50000
    unless (length funcs <= maxFunctions) $ throwError $ "Wasm module must not declare more than " ++ show maxFunctions ++ " functions"
    let maxGlobals = 300
    unless (length globs <= maxGlobals) $ throwError $ "Wasm module must not declare more than " ++ show maxGlobals ++ " globals"
    forM_ exps $ \(n, idx) ->
      let canister_prefix = "canister_" in
      when (isPrefixOf canister_prefix n) $ do
        let special = map (canister_prefix ++) ["init", "inspect_message", "heartbeat", "global_timer", "pre_upgrade", "post_upgrade"]
        unless (elem n special || isPrefixOf "canister_update " n || isPrefixOf "canister_query " n || isPrefixOf "canister_composite_query " n) $ throwError $ "Names of exported functions starting with " ++ canister_prefix ++ " must have the form " ++ show special ++ " or canister_update <name>, canister_query <name>, or canister_composite_query <name>"
        let ty = funcs !! (idx - nimps)
        let (args, res) = tys !! ty
        unless (args == [] && res == []) $ throwError $ "Exported functions whose names start with " ++ show canister_prefix ++ " must have type () -> ()"
    return wasm_mod


initialize :: forall s. Module -> Imports s -> HostM s (Instance s)
initialize mod imps = withExceptT show $ do
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


invokeExport :: Instance s -> FuncName -> [W.Value] -> HostM s [W.Value]
invokeExport (mods', ref) method args = do
  let inst = mods' IM.! ref
  withExceptT show $
    W.invokeByName mods' inst (T.pack method) args

invokeTable :: Instance s -> Int32 -> [W.Value] -> HostM s [W.Value]
invokeTable (mods', ref) idx args = do
  let inst = mods' IM.! ref
  withExceptT show $ do
    func <- W.elem inst (0 W.@@ def) idx def
    W.invoke mods' inst func args

getBytes :: Instance s -> W.Address -> W.Size -> HostM s BS.ByteString
getBytes (mods', ref) ptr len = do
  let inst = mods' IM.! ref
  let mem = V.head (W._miMemories inst)
  withExceptT show $ W.loadBytes mem ptr len

setBytes :: Instance s -> W.Address -> BS.ByteString -> HostM s ()
setBytes (mods', ref) ptr blob = do
  let inst = mods' IM.! ref
  let mem = V.head (W._miMemories inst)
  withExceptT show $ W.storeBytes mem (fromIntegral ptr) blob

