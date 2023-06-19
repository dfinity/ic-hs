{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MultiWayIf #-}

{-|
This module implements the management canister logic of the Internet Computer.
-}
module IC.Ref.Management (invokeManagementCanister)
where

import qualified Data.Map as M
import qualified Data.Row as R
import qualified Data.Row.Variants as V
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as Vec
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Numeric.Natural
import Control.Monad.State.Class
import Control.Monad.Except
import Control.Monad.Random.Lazy
import Codec.Candid
import Data.Row ((.==), (.+), (.!), type (.!))

import IC.Types
import IC.Constants
import IC.Canister
import IC.Id.Fresh
import IC.Management
import IC.Crypto.Bitcoin as Bitcoin
import IC.Ref.HTTP
import IC.Ref.Types

invokeManagementCanister ::
  forall m. (CanReject m, ICM m, MonadIO m) => EntityId -> Maybe Subnet -> CallId -> EntryPoint -> m ()
invokeManagementCanister caller maybeSubnet ctxt_id (Public method_name arg) =
  case method_name of
      "create_canister" -> atomic $ noSubnet caller maybeSubnet $ icCreateCanister caller maybeSubnet ctxt_id
      "install_code" -> atomic $ onlyControllerOrSelf method_name False caller $ checkSubnet fetchCanisterId maybeSubnet $ icInstallCode caller
      "uninstall_code" -> atomic $ onlyControllerOrSelf method_name False caller $ checkSubnet fetchCanisterId maybeSubnet $ icUninstallCode
      "update_settings" -> atomic $ onlyControllerOrSelf method_name False caller $ checkSubnet fetchCanisterId maybeSubnet icUpdateCanisterSettings
      "start_canister" -> atomic $ onlyControllerOrSelf method_name False caller $ checkSubnet fetchCanisterId maybeSubnet icStartCanister
      "stop_canister" -> deferred $ onlyControllerOrSelf method_name False caller $ checkSubnet fetchCanisterId maybeSubnet $ icStopCanister ctxt_id
      "canister_status" -> atomic $ onlyControllerOrSelf method_name True caller $ checkSubnet fetchCanisterId maybeSubnet icCanisterStatus
      "delete_canister" -> atomic $ onlyControllerOrSelf method_name False caller $ checkSubnet fetchCanisterId maybeSubnet icDeleteCanister
      "deposit_cycles" -> atomic $ checkSubnet fetchCanisterId maybeSubnet $ icDepositCycles ctxt_id
      "provisional_create_canister_with_cycles" -> atomic $ icCreateCanisterWithCycles caller maybeSubnet ctxt_id
      "provisional_top_up_canister" -> atomic $ checkSubnet fetchCanisterId maybeSubnet icTopUpCanister
      "raw_rand" -> atomic $ noSubnet caller maybeSubnet icRawRand
      "http_request" -> atomic $ noSubnet caller maybeSubnet $ icHttpRequest caller maybeSubnet ctxt_id
      "ecdsa_public_key" -> atomic $ checkSubnet (fetchCanisterIdfromMaybe caller) maybeSubnet $ icEcdsaPublicKey caller
      "sign_with_ecdsa" -> atomic $ noSubnet caller maybeSubnet $ icSignWithEcdsa caller
      _ -> reject RC_DESTINATION_INVALID ("Unsupported management function " ++ method_name) (Just EC_METHOD_NOT_FOUND)
  where
    -- always responds
    atomic :: forall a b.  (CandidArg a, CandidArg b) => (a -> m b) -> m ()
    atomic meth = wrap (\k x -> meth x >>= k) (replyCallContext ctxt_id) arg

    -- no implict reply
    deferred :: forall a. CandidArg a => (a -> m ()) -> m ()
    deferred meth = wrap @a @() (\_k x -> meth x) (error "unused") arg

    wrap
      :: forall a b.
      (CandidArg a, CandidArg b) =>
      ((b -> m ()) -> a -> m ()) ->
      ((Blob -> m ()) -> Blob -> m ())
    wrap method raw_reply blob =
      case decode @a blob of
        Left msg -> reject RC_CANISTER_ERROR ("Candid failed to decode: " ++ msg) (Just EC_INVALID_ENCODING)
        Right x -> method (raw_reply . encode @b) x

invokeManagementCanister _ _ _ Closure{} = error "closure invoked on management canister"
invokeManagementCanister _ _ _ Heartbeat = error "heartbeat invoked on management canister"
invokeManagementCanister _ _ _ GlobalTimer = error "global timer invoked on management canister"

icCreateCanister :: (ICM m, CanReject m) => EntityId -> Maybe Subnet -> CallId -> ICManagement m .! "create_canister"
icCreateCanister caller maybe_subnet ctxt_id r = do
    forM_ (r .! #settings) validateSettings
    available <- getCallContextCycles ctxt_id
    setCallContextCycles ctxt_id 0
    cid <- icCreateCanisterCommon caller maybe_subnet Nothing caller available
    forM_ (r .! #settings) $ applySettings cid
    return (#canister_id .== entityIdToPrincipal cid)

icCreateCanisterWithCycles :: (ICM m, CanReject m) => EntityId -> Maybe Subnet -> CallId -> ICManagement m .! "provisional_create_canister_with_cycles"
icCreateCanisterWithCycles caller maybe_subnet ctxt_id r = do
    forM_ (r .! #settings) validateSettings
    ecid <- ecidOfCallID ctxt_id
    cid <- icCreateCanisterCommon ecid maybe_subnet (principalToEntityId <$> r .! #specified_id) caller (fromMaybe cDEFAULT_PROVISIONAL_CYCLES_BALANCE (r .! #amount))
    forM_ (r .! #settings) $ applySettings cid
    return (#canister_id .== entityIdToPrincipal cid)

icCreateCanisterCommon :: (ICM m, CanReject m) => EntityId -> Maybe Subnet -> Maybe EntityId -> EntityId -> Natural -> m EntityId
icCreateCanisterCommon ecid maybe_subnet specified_id controller amount = do
    ranges <- case maybe_subnet of
          Nothing -> do
            (_, _, _, _, ranges) <- getSubnetFromCanisterId ecid
            return ranges
          Just (_, _, _, _, ranges) -> return ranges
    taken <- gets (M.keys . canisters)
    new_id <- case specified_id of
                Nothing -> do
                  case freshId ranges taken of
                    Nothing -> reject RC_SYS_FATAL ("Could not create canister. Subnet has surpassed its canister ID allocation.") Nothing
                    Just new_id -> do return new_id
                Just cid -> do
                  when (cid `elem` taken) $
                    reject RC_DESTINATION_INVALID ("The specified_id of the created canister is already in use.") Nothing
                  unless (checkCanisterIdInRanges ranges cid) $
                    reject RC_CANISTER_REJECT ("The specified_id of the created canister does not belong to the subnet's canister ranges.") Nothing
                  return cid
    let currentTime = 0 -- ic-ref lives in the 70ies
    createEmptyCanister new_id (S.singleton controller) currentTime
    -- Here we fill up the canister with the cycles provided by the caller
    setBalance new_id amount
    return new_id

validateSettings :: CanReject m => Settings -> m ()
validateSettings r = do
    forM_ (r .! #compute_allocation) $ \n -> do
        unless (n <= 100) $
            reject RC_CANISTER_ERROR "Compute allocation not <= 100" (Just EC_CANISTER_CONTRACT_VIOLATION)
    forM_ (r .! #memory_allocation) $ \n -> do
        unless (n <= 2^(48::Int)) $
            reject RC_CANISTER_ERROR "Memory allocation not <= 2^48" (Just EC_CANISTER_CONTRACT_VIOLATION)
    forM_ (r .! #freezing_threshold) $ \n -> do
        unless (n < 2^(64::Int)) $
            reject RC_CANISTER_ERROR "Freezing threshold not < 2^64" (Just EC_CANISTER_CONTRACT_VIOLATION)
    forM_ (r .! #controllers) $ \n -> do
        unless (length n <= 10) $
            reject RC_CANISTER_ERROR "Controllers cannot be > 10" (Just EC_CANISTER_CONTRACT_VIOLATION)

applySettings :: ICM m => EntityId -> Settings -> m ()
applySettings cid r = do
    forM_ (r .! #controllers) $ setControllers cid . S.fromList . map principalToEntityId . Vec.toList
    forM_ (r .! #compute_allocation) $ setComputeAllocation cid
    forM_ (r .! #memory_allocation) $ setMemoryAllocation cid
    forM_ (r .! #freezing_threshold) $ setFreezingThreshold cid

onlyControllerOrSelf ::
  (ICM m, CanReject m, (r .! "canister_id") ~ Principal) =>
  String -> Bool -> EntityId -> (R.Rec r -> m a) -> (R.Rec r -> m a)
onlyControllerOrSelf method_name self caller act r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    canisterMustExist canister_id
    controllers <- getControllers canister_id
    let allowed = if self then S.insert canister_id controllers else controllers
    if caller `S.member` allowed
    then act r
    else reject RC_CANISTER_ERROR (
        prettyID caller <> " is not authorized to call " ++ method_name ++ " on canister " <>
        prettyID canister_id <> ", Allowed principals are: " <> intercalate ", " (map prettyID (S.toList allowed)))
        (Just EC_NOT_AUTHORIZED)

fetchCanisterIdfromMaybe ::
  ((r .! "canister_id") ~ Maybe Principal) =>
  EntityId -> R.Rec r -> EntityId
fetchCanisterIdfromMaybe cid r =
  case r .! #canister_id of Nothing -> cid
                            Just c -> principalToEntityId c

fetchCanisterId ::
  ((r .! "canister_id") ~ Principal) =>
  R.Rec r -> EntityId
fetchCanisterId r = principalToEntityId (r .! #canister_id)

checkSubnet ::
  (ICM m, CanReject m) =>
  (r -> EntityId) -> Maybe Subnet -> (r -> m a) -> (r -> m a)
checkSubnet _ Nothing act r = act r
checkSubnet c (Just (subnet_id, _, _, _, _)) act r = do
    let canister_id = c r
    canisterMustExist canister_id
    (subnet_id', _, _, _, _) <- getSubnetFromCanisterId canister_id
    if subnet_id == subnet_id'
    then act r
    else reject RC_DESTINATION_INVALID (
        prettyID canister_id <> " does not belong to subnet " <>
        prettyID subnet_id)
        (Just EC_INVALID_ARGUMENT)

noSubnet ::
  (ICM m, CanReject m) =>
  EntityId -> Maybe Subnet -> (r -> m a) -> (r -> m a)
noSubnet _ Nothing act r = act r
noSubnet caller (Just (subnet_id, _, _, _, _)) act r = do
    root_subnet_id <- gets rootSubnet
    (caller_subnet_id, _, _, _, _) <- getSubnetFromCanisterId caller
    if (root_subnet_id == Just caller_subnet_id || subnet_id == caller_subnet_id) then
      act r
    else
      reject RC_CANISTER_ERROR "the caller must be on the root subnet or belong to the target subnet" (Just EC_INVALID_ARGUMENT)

icInstallCode :: (ICM m, CanReject m) => EntityId -> ICManagement m .! "install_code"
icInstallCode caller r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    let arg = r .! #arg
    new_can_mod <- return (parseCanister (r .! #wasm_module))
      `onErr` (\err -> reject RC_CANISTER_ERROR ("Parsing failed: " ++ err) (Just EC_INVALID_MODULE))
    was_empty <- isCanisterEmpty canister_id

    let
      reinstall = do
        env <- canisterEnv canister_id
        let env1 = env { env_canister_version = env_canister_version env + 1, env_global_timer = 0 }
        (wasm_state, ca) <- liftIO (init_method new_can_mod caller env1 arg)
          `onTrap` (\msg -> reject RC_CANISTER_ERROR ("Initialization trapped: " ++ msg) (Just EC_CANISTER_TRAPPED))
        setCanisterContent canister_id $ CanisterContent
            { can_mod = new_can_mod
            , wasm_state = wasm_state
            }
        performCanisterActions canister_id ca
        bumpCanisterVersion canister_id
        when (set_certified_data ca == Nothing) $ setCertifiedData canister_id ""
        when (set_global_timer ca == Nothing) $ setCanisterGlobalTimer canister_id 0

      install = do
        unless was_empty $
          reject RC_CANISTER_ERROR "canister is not empty during installation" (Just EC_CANISTER_NOT_EMPTY)
        reinstall

      upgrade = do
        when was_empty $
          reject RC_CANISTER_ERROR "canister is empty during upgrade" (Just EC_CANISTER_EMPTY)
        old_wasm_state <- getCanisterState canister_id
        old_can_mod <- getCanisterMod canister_id
        env <- canisterEnv canister_id
        let env1 = env
        (ca1, mem) <- return (pre_upgrade_method old_can_mod old_wasm_state caller env1)
          `onTrap` (\msg -> reject RC_CANISTER_ERROR ("Pre-upgrade trapped: " ++ msg) (Just EC_CANISTER_TRAPPED))
        -- TODO: update balance in env based on ca1 here, once canister actions
        -- can change balances
        let env2 = env { env_canister_version = env_canister_version env + 1, env_global_timer = 0 }
        (new_wasm_state, ca2) <- return (post_upgrade_method new_can_mod caller env2 mem arg)
          `onTrap` (\msg -> reject RC_CANISTER_ERROR ("Post-upgrade trapped: " ++ msg) (Just EC_CANISTER_TRAPPED))

        setCanisterContent canister_id $ CanisterContent
            { can_mod = new_can_mod
            , wasm_state = new_wasm_state
            }
        performCanisterActions canister_id (ca1 { set_global_timer = Nothing } <> ca2)
        bumpCanisterVersion canister_id
        when (set_global_timer ca2 == Nothing) $ setCanisterGlobalTimer canister_id 0

    R.switch (r .! #mode) $ R.empty
      .+ #install .== (\() -> install)
      .+ #reinstall .== (\() -> reinstall)
      .+ #upgrade .== (\() -> upgrade)

icUninstallCode :: (ICM m, CanReject m) => ICManagement m .! "uninstall_code"
icUninstallCode r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    -- empty canister, resetting selected state
    modCanister canister_id $ \can_state -> can_state
      { content = Nothing
      , certified_data = ""
      , canister_version = canister_version can_state + 1
      , global_timer = 0
      }
    -- reject all call open contexts of this canister
    gets (M.toList . call_contexts) >>= mapM_ (\(ctxt_id, ctxt) ->
        when (canister ctxt == canister_id && needs_to_respond ctxt == NeedsToRespond True) $ do
            rejectCallContext ctxt_id (RC_CANISTER_REJECT, "Canister has been uninstalled", Just EC_CANISTER_EMPTY)
            deleteCallContext ctxt_id
        )

icUpdateCanisterSettings :: (ICM m, CanReject m) => ICManagement m .! "update_settings"
icUpdateCanisterSettings r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    validateSettings (r .! #settings)
    applySettings canister_id (r .! #settings)
    bumpCanisterVersion canister_id

icStartCanister :: (ICM m, CanReject m) => ICManagement m .! "start_canister"
icStartCanister r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    getRunStatus canister_id >>= \case
        IsRunning -> return ()
        IsStopping pending -> do
            forM_ pending $ \ctxt_id ->
                rejectCallContext ctxt_id (RC_CANISTER_ERROR, "Canister has been restarted", Just EC_CANISTER_RESTARTED)
            setRunStatus canister_id IsRunning
        IsStopped -> setRunStatus canister_id IsRunning
        IsDeleted -> error "deleted canister encountered"

icStopCanister ::
  (ICM m, CanReject m) =>
  (a -> m b) ~ (ICManagement m .! "stop_canister") =>
  CallId -> a -> m ()
icStopCanister ctxt_id r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    getRunStatus canister_id >>= \case
        IsRunning -> setRunStatus canister_id (IsStopping [ctxt_id])
        IsStopping pending -> setRunStatus canister_id (IsStopping (pending ++ [ctxt_id]))
        IsStopped -> replyCallContext ctxt_id (encode ())
        IsDeleted -> error "deleted canister encountered"

icCanisterStatus :: (ICM m, CanReject m) => ICManagement m .! "canister_status"
icCanisterStatus r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    s <- getRunStatus canister_id >>= \case
        IsRunning -> return (V.IsJust #running ())
        IsStopping _pending -> return (V.IsJust #stopping ())
        IsStopped -> return (V.IsJust #stopped ())
        IsDeleted -> error "deleted canister encountered"
    can_state <- getCanister canister_id
    hash <- module_hash <$> getCanister canister_id
    cycles <- getBalance canister_id
    idle_cycles_burned_per_day <- idle_cycles_burned_per_day <$> getCanister canister_id
    return $ R.empty
      .+ #status .== s
      .+ #settings .== (R.empty
        .+ #controllers .== Vec.fromList (map entityIdToPrincipal (S.toList (controllers can_state)))
        .+ #memory_allocation .== memory_allocation can_state
        .+ #compute_allocation .== compute_allocation can_state
        .+ #freezing_threshold .== freezing_threshold can_state
      )
      .+ #memory_size .== 0 -- not implemented here
      .+ #module_hash .== hash
      .+ #cycles .== cycles
      .+ #idle_cycles_burned_per_day .== idle_cycles_burned_per_day


icDeleteCanister :: (ICM m, CanReject m) => ICManagement m .! "delete_canister"
icDeleteCanister r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    getRunStatus canister_id >>= \case
        IsRunning -> reject RC_CANISTER_ERROR "Cannot delete running canister" (Just EC_CANISTER_NOT_STOPPED)
        IsStopping _pending -> reject RC_CANISTER_ERROR "Cannot delete stopping canister" (Just EC_CANISTER_NOT_STOPPED)
        IsStopped -> return ()
        IsDeleted -> error "deleted canister encountered"

    setRunStatus canister_id IsDeleted

icDepositCycles :: (ICM m, CanReject m) => CallId -> ICManagement m .! "deposit_cycles"
icDepositCycles ctxt_id r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    canisterMustExist canister_id

    cycles <- getCallContextCycles ctxt_id
    available <- getCallContextCycles ctxt_id
    setCallContextCycles ctxt_id (available - cycles)
    prev_balance <- getBalance canister_id
    setBalance canister_id $ prev_balance + cycles

icTopUpCanister :: (ICM m, CanReject m) => ICManagement m .! "provisional_top_up_canister"
icTopUpCanister r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    canisterMustExist canister_id

    prev_balance <- getBalance canister_id
    setBalance canister_id $ prev_balance + (r .! #amount)

icRawRand :: ICM m => ICManagement m .! "raw_rand"
icRawRand _r = runRandIC $ BS.pack <$> replicateM 32 getRandom

runRandIC :: ICM m => Rand StdGen a -> m a
runRandIC a = state $ \ic ->
    let (x, g) = runRand a (rng ic)
    in (x, ic { rng = g })

icEcdsaPublicKey :: (ICM m, CanReject m) => EntityId -> ICManagement m .! "ecdsa_public_key"
icEcdsaPublicKey caller r = do
    let cid = fetchCanisterIdfromMaybe caller r
    canisterMustExist cid
    let key = getCanisterRootKey cid
    case Bitcoin.derivePublicKey key (r .! #derivation_path) of
      Left err -> reject RC_CANISTER_ERROR err (Just EC_INVALID_ENCODING)
      Right k -> return $ R.empty
                   .+ #public_key .== (publicKeyToDER k)
                   .+ #chain_code .== (extractChainCode k)

icSignWithEcdsa :: (ICM m, CanReject m) => EntityId -> ICManagement m .! "sign_with_ecdsa"
icSignWithEcdsa caller r = do
    let key = getCanisterRootKey caller
    case Bitcoin.derivePrivateKey key (r .! #derivation_path) of
      Left err -> reject RC_CANISTER_ERROR err  (Just EC_INVALID_ENCODING)
      Right k -> do
        case Bitcoin.toHash256 (r .! #message_hash) of
          Left err -> reject RC_CANISTER_ERROR err (Just EC_INVALID_ENCODING)
          Right h ->
            return $ R.empty
              .+ #signature .== (Bitcoin.sign k h)
