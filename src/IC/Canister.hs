{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module IC.Canister
  ( WasmState,
    parseCanister,
    CanisterModule (..),
    InitFunc,
    UpdateFunc,
    QueryFunc,
    asUpdate,
  )
where

import Data.Foldable (toList)
import Data.List
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import IC.Canister.Imp
import IC.Canister.Snapshot
import IC.Hash
import IC.Purify
import IC.Types
import IC.Wasm.Winter (Module, exportedFunctions, parseModule)
import qualified Wasm.Syntax.AST as W

-- Here we can swap out the purification machinery
type WasmState = CanisterSnapshot

-- type WasmState = Replay ImpState

type InitFunc = EntityId -> Env -> Blob -> TrapOr (WasmState, CanisterActions)

type UpdateFunc = WasmState -> TrapOr (WasmState, UpdateResult)

type QueryFunc = WasmState -> TrapOr Response

data CanisterModule = CanisterModule
  { raw_wasm :: Blob,
    raw_wasm_hash :: Blob, -- just caching, it’s worth it
    init_method :: InitFunc,
    update_methods :: MethodName ↦ (EntityId -> Env -> NeedsToRespond -> Cycles -> Blob -> UpdateFunc),
    query_methods :: MethodName ↦ (EntityId -> Env -> Blob -> QueryFunc),
    callbacks :: Callback -> Env -> NeedsToRespond -> Cycles -> Response -> Cycles -> UpdateFunc,
    cleanup :: WasmClosure -> Env -> WasmState -> TrapOr (WasmState, ()),
    pre_upgrade_method :: WasmState -> EntityId -> Env -> TrapOr (CanisterActions, Blob),
    post_upgrade_method :: EntityId -> Env -> Blob -> Blob -> TrapOr (WasmState, CanisterActions),
    inspect_message :: MethodName -> EntityId -> Env -> Blob -> WasmState -> TrapOr (),
    heartbeat :: Env -> WasmState -> TrapOr (WasmState, ([MethodCall], CanisterActions)),
    public_custom_sections :: Text ↦ Blob,
    private_custom_sections :: Text ↦ Blob
  }

instance Show CanisterModule where
  show _ = "CanisterModule{...}"

parseCanister :: Blob -> Either String CanisterModule
parseCanister bytes =
  case parseModule bytes of
    Left err -> Left err
    Right wasm_mod ->
      Right $
        CanisterModule
          { raw_wasm = bytes,
            raw_wasm_hash = sha256 bytes,
            init_method = \caller env dat ->
              case instantiate wasm_mod of
                Trap err -> Trap err
                Return wasm_state0 ->
                  invoke wasm_state0 (rawInitialize caller env dat),
            update_methods =
              M.fromList
                [ ( m,
                    \caller env needs_to_respond cycles_available dat wasm_state ->
                      invoke wasm_state (rawUpdate m caller env needs_to_respond cycles_available dat)
                  )
                  | n <- exportedFunctions wasm_mod,
                    Just m <- return $ stripPrefix "canister_update " n
                ],
            query_methods =
              M.fromList
                [ ( m,
                    \caller env arg wasm_state ->
                      snd <$> invoke wasm_state (rawQuery m caller env arg)
                  )
                  | n <- exportedFunctions wasm_mod,
                    Just m <- return $ stripPrefix "canister_query " n
                ],
            callbacks = \cb env needs_to_respond cycles_available res refund wasm_state ->
              invoke wasm_state (rawCallback cb env needs_to_respond cycles_available res refund),
            cleanup = \cb env wasm_state ->
              invoke wasm_state (rawCleanup cb env),
            pre_upgrade_method = \wasm_state caller env ->
              snd <$> invoke wasm_state (rawPreUpgrade caller env),
            post_upgrade_method = \caller env mem dat ->
              case instantiate wasm_mod of
                Trap err -> Trap err
                Return wasm_state0 ->
                  invoke wasm_state0 (rawPostUpgrade caller env mem dat),
            inspect_message = \method_name caller env arg wasm_state ->
              snd <$> invoke wasm_state (rawInspectMessage method_name caller env arg),
            heartbeat = \env wasm_state -> invoke wasm_state (rawHeartbeat env),
            public_custom_sections = collect_sections wasm_mod "icp:public",
            private_custom_sections = collect_sections wasm_mod "icp:private"
          }
  where
    collect_sections wasm_mod prefix =
        foldl'
            (\m cust ->
                 let custName = W._customName cust in
                 if (prefix <> " ") `TL.isPrefixOf` custName
                     then M.insert
                              (TL.drop (TL.length (prefix <> " ")) custName)
                              (W._customPayload cust) m
                     else m)
            M.empty
            (toList (W._moduleCustom wasm_mod))


instantiate :: Module -> TrapOr WasmState
instantiate wasm_mod =
  either Trap Return $
    snd $
      createMaybe $ do
        rawInstantiate wasm_mod >>= \case
          Trap err -> return ((), Left err)
          Return rs -> return ((), Right rs)

invoke :: WasmState -> CanisterEntryPoint (TrapOr r) -> TrapOr (WasmState, r)
invoke s f =
  case perform f s of
    (_, Trap msg) -> Trap msg
    (s', Return r) -> Return (s', r)

-- | Turns a query function into an update function
asUpdate ::
  (EntityId -> Env -> Blob -> QueryFunc) ->
  (EntityId -> Env -> NeedsToRespond -> Cycles -> Blob -> UpdateFunc)
asUpdate f caller env (NeedsToRespond needs_to_respond) _cycles_available dat wasm_state
  | not needs_to_respond = error "asUpdate: needs_to_respond == False"
  | otherwise =
    (\res -> (wasm_state, (noCallActions {ca_response = Just res}, noCanisterActions)))
      <$> f caller env dat wasm_state
