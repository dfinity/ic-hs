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
This module implements the main abstract types of the Internet Computer.
-}
module IC.Ref.Types where

import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Word as W
import Data.Functor
import Data.List
import Data.Maybe
import Numeric.Natural
import Control.Monad.State.Class
import Control.Monad.Except
import Control.Monad.Random.Lazy
import Data.Sequence (Seq(..))
import GHC.Stack

import IC.Crypto.Bitcoin as Bitcoin
import IC.Types
import IC.Canister
import IC.Id.Fresh
import IC.Id.Forms(mkSelfAuthenticatingId)
import IC.Utils
import IC.HashTree hiding (Blob)
import IC.Certificate
import IC.Crypto

-- Abstract HTTP Interface

data CallRequest
    = CallRequest CanisterId UserId MethodName Blob
  deriving (Eq, Ord, Show)

data QueryRequest = QueryRequest CanisterId UserId MethodName Blob
data ReadStateRequest = ReadStateRequest UserId [Path]

data RequestStatus
  = Received
  | Processing
  | CallResponse CallResponse
  deriving (Show)

data CallResponse
  = Rejected (RejectCode, String, Maybe ErrorCode)
  | Replied Blob
  deriving (Show)

data ValidationError
  = HTTPError String
  | ExecutionError (RejectCode, String, Maybe ErrorCode)

data ReqResponse
  = RequestError (RejectCode, String, Maybe ErrorCode)
  | QueryResponse CallResponse
  | ReadStateResponse Certificate
  deriving (Show)

-- IC state

-- The canister state

data RunStatus
  = IsRunning
  | IsStopping [CallId]
  | IsStopped
  | IsDeleted -- not actually a run state, but convenient in this code
  deriving (Eq, Show)

data CanisterContent = CanisterContent
  { can_mod :: CanisterModule
  , wasm_state :: WasmState
  }
  deriving (Show)

data CanisterHistory = CanisterHistory {
    changes :: [Change]
  , total_num_changes :: W.Word64
}
  deriving (Show)

data CanState = CanState
  { content :: Maybe CanisterContent -- absent when empty
  , run_status :: RunStatus
  , controllers :: S.Set EntityId
  , memory_allocation :: Natural
  , compute_allocation :: Natural
  , freezing_threshold :: Natural
  , time :: Timestamp
  , cycle_balance :: Natural
  , certified_data :: Blob
  , canister_version :: Natural
  , canister_history :: CanisterHistory
  , global_timer :: Natural
  -- |Not part of the spec, but in this implementation we schedule
  -- heartbeats only for canisters who have not been idle since the
  -- last heartbeat, so we remember the last action.
  , last_action :: Maybe EntryPoint
  }
  deriving (Show)

-- A canister entry point is either a publicly named function, a closure
-- (callback + environment) or a heartbeat.
data EntryPoint
  = Public MethodName Blob
  | Closure Callback Response Cycles
  | Heartbeat
  | GlobalTimer
  deriving (Show)

type CallId = Int
data CallContext = CallContext
  { canister :: CanisterId
  , origin :: CallOrigin
  , needs_to_respond :: NeedsToRespond
  , deleted :: Bool
  , available_cycles :: Cycles
  , last_trap :: Maybe String
      -- ^ non-normative, but yields better reject messages
  }
  deriving (Show)

data CallOrigin
  = FromUser RequestID CanisterId
  | FromCanister CallId Callback
  | FromSystemTask
  deriving (Eq, Show)

data Message
  = CallMessage
    { call_context :: CallId
    , entry :: EntryPoint
    }
  | ResponseMessage
    { call_context :: CallId
    , response :: Response
    , refunded_cycles :: Cycles
    }
  deriving (Show)

-- Finally, the full IC state:

type Subnet = (EntityId, SubnetType, W.Word64, SecretKey, [(W.Word64, W.Word64)])

isRootSubnet :: Subnet -> Bool
isRootSubnet (_, _, _, _, ranges) = checkCanisterIdInRanges ranges nns_canister_id
  where
    nns_canister_id = wordToId 0

data IC = IC
  { canisters :: CanisterId ↦ CanState
  , requests :: RequestID ↦ (CallRequest, (RequestStatus, CanisterId))
  , messages :: Seq Message
  , call_contexts :: CallId ↦ CallContext
  , rng :: StdGen
  , secretRootKey :: SecretKey
  , rootSubnet :: Maybe EntityId
  , subnets :: [Subnet]
  }
  deriving (Show)

-- The functions below want stateful access to a value of type 'IC'
type ICM m = (MonadState IC m, HasRefConfig, HasCallStack, MonadIO m)

initialIC :: [SubnetConfig] -> IO IC
initialIC subnet_configs = do
    let subnets = map sub subnet_configs
    let root_subnet = find isRootSubnet subnets
    let sk = case root_subnet of Nothing -> createSecretKeyBLS "ic-ref's very secure secret root key"
                                 Just (_, _, _, k, _) -> k
    IC mempty mempty mempty mempty <$> newStdGen <*> pure sk <*> pure (fmap (\(id, _, _, _, _) -> id) root_subnet) <*> pure subnets
  where
    sub conf = (sub_id (key conf), subnet_type conf, subnet_size conf, key conf, canister_ranges conf)
    sub_id = EntityId . mkSelfAuthenticatingId . toPublicKey
    key = createSecretKeyBLS . BLU.fromString . nonce

-- Error handling plumbing

type CanReject = MonadError (RejectCode, String, Maybe ErrorCode)
reject :: CanReject m => RejectCode -> String -> Maybe ErrorCode -> m a2
reject code msg err = throwError (code, msg, err)

canisterRejected :: (RejectCode, String, Maybe ErrorCode) -> CallResponse
canisterRejected (rc, msg, err) =  Rejected (rc, msg, Just $ maybe EC_CANISTER_REJECTED id err)

rejectMessage :: (RejectCode, String, Maybe ErrorCode) -> String
rejectMessage (_rc, msg, _err) = msg

onReject :: ICM m =>
  ((RejectCode, String, Maybe ErrorCode) -> m b) ->
  (forall m'. (CanReject m', ICM m') => m' b) -> m b
onReject h act = runExceptT act >>= \case
  Left cs -> h cs
  Right x -> return x


onErr :: Monad m => m (Either a b) -> (a -> m b) -> m b
onErr a b = a >>= either b return

orElse :: Monad m => m (Maybe a) -> m a -> m a
orElse a b = a >>= maybe b return

onTrap :: Monad m => m (TrapOr a) -> (String -> m a) -> m a
onTrap a b = a >>= \case { Trap msg -> b msg; Return x -> return x }

-- Helper functions

getSubnetFromCanisterId' :: (CanReject m, ICM m) => CanisterId -> m (Maybe Subnet)
getSubnetFromCanisterId' cid = do
  subnets <- gets subnets
  return $ find (\(_, _, _, _, ranges) -> checkCanisterIdInRanges ranges cid) subnets

getSubnetFromCanisterId :: (CanReject m, ICM m) => CanisterId -> m Subnet
getSubnetFromCanisterId cid = do
    subnet <- getSubnetFromCanisterId' cid
    case subnet of
      Nothing -> reject RC_SYS_FATAL "Canister id does not belong to any subnet." Nothing
      Just x -> return x

getSubnetFromSubnetId :: (CanReject m, ICM m) => CanisterId -> m (Maybe Subnet)
getSubnetFromSubnetId sid = find (\(id, _, _, _, _) -> sid == id) <$> gets subnets

getCanisterRootKey :: CanisterId -> Bitcoin.ExtendedSecretKey
getCanisterRootKey cid = Bitcoin.createExtendedKey $ rawEntityId cid

canisterMustExist :: (CanReject m, ICM m) => CanisterId -> m ()
canisterMustExist cid =
  gets (M.lookup cid . canisters) >>= \case
    Nothing ->
      reject RC_DESTINATION_INVALID ("canister does not exist: " ++ prettyID cid) (Just EC_CANISTER_NOT_FOUND)
    Just CanState{ run_status = IsDeleted } ->
      reject RC_DESTINATION_INVALID ("canister no longer exists: " ++ prettyID cid) (Just EC_CANISTER_NOT_FOUND)
    _ -> return ()

isCanisterEmpty :: ICM m => CanisterId -> m Bool
isCanisterEmpty cid = isNothing . content <$> getCanister cid

createEmptyCanister :: ICM m => CanisterId -> S.Set EntityId -> Timestamp -> m ()
createEmptyCanister cid controllers time = modify $ \ic ->
    ic { canisters = M.insert cid can (canisters ic) }
  where
    can = CanState
      { content = Nothing
      , run_status = IsRunning
      , controllers = controllers
      , memory_allocation = 0
      , compute_allocation = 0
      , freezing_threshold = 2592000
      , time = time
      , cycle_balance = 0
      , certified_data = ""
      , canister_version = 0
      , canister_history = CanisterHistory [] 0
      , global_timer = 0
      , last_action = Nothing
      }

enqueueMessage :: ICM m => Message -> m ()
enqueueMessage m = modify $ \ic -> ic { messages = messages ic :|> m }

managementCanisterId :: EntityId
managementCanisterId = EntityId mempty

-- Call context handling

callerOfCallRequest :: CallRequest -> EntityId
callerOfCallRequest = \case
    CallRequest _ user_id _ _ -> user_id

callerOfRequest :: ICM m => RequestID -> m EntityId
callerOfRequest rid = gets (M.lookup rid . requests) >>= \case
    Just (ar,_) -> return (callerOfCallRequest ar)
    Nothing -> error "callerOfRequest"

callerOfCallID :: ICM m => CallId -> m EntityId
callerOfCallID ctxt_id = do
  ctxt <- getCallContext ctxt_id
  case origin ctxt of
    FromUser rid _ -> callerOfRequest rid
    FromCanister other_ctxt_id _callback -> calleeOfCallID other_ctxt_id
    FromSystemTask -> return $ canister ctxt

calleeOfCallID :: ICM m => CallId -> m EntityId
calleeOfCallID ctxt_id = canister <$> getCallContext ctxt_id

ecidOfCallID :: ICM m => CallId -> m CanisterId
ecidOfCallID ctxt_id = do
  ctxt <- getCallContext ctxt_id
  case origin ctxt of
    FromUser _rid ecid -> return ecid
    _ -> callerOfCallID ctxt_id

getCallContext :: ICM m => CallId -> m CallContext
getCallContext ctxt_id = gets ((M.! ctxt_id) . call_contexts)

modifyCallContext :: ICM m => CallId -> (CallContext -> CallContext) -> m ()
modifyCallContext ctxt_id f = modify $ \ic ->
  ic { call_contexts = M.adjust f ctxt_id (call_contexts ic) }

getCallContextCycles :: ICM m => CallId -> m Cycles
getCallContextCycles ctxt_id = available_cycles <$> getCallContext ctxt_id

setCallContextCycles :: ICM m => CallId -> Cycles -> m ()
setCallContextCycles ctxt_id cycles = modifyCallContext ctxt_id $ \ctxt ->
  ctxt { available_cycles = cycles }

respondCallContext :: ICM m => CallId -> Response -> m ()
respondCallContext ctxt_id response = do
  ctxt <- getCallContext ctxt_id
  when (deleted ctxt) $
    error "Internal error: response to deleted call context"
  when (origin ctxt == FromSystemTask) $
    error "Internal error: System tasks cannot be responded to"
  when (needs_to_respond ctxt == NeedsToRespond False) $
    error $ "Internal error: Double response when responding with " <> show response
  modifyCallContext ctxt_id $ \ctxt -> ctxt
    { needs_to_respond = NeedsToRespond False
    , available_cycles = 0
    }
  enqueueMessage $ ResponseMessage {
    call_context = ctxt_id,
    response,
    refunded_cycles = available_cycles ctxt
  }

replyCallContext :: ICM m => CallId -> Blob -> m ()
replyCallContext ctxt_id blob =
  respondCallContext ctxt_id (Reply blob)

rejectCallContext :: ICM m => CallId -> (RejectCode, String, Maybe ErrorCode) -> m ()
rejectCallContext ctxt_id (rc, msg, _err) =
  respondCallContext ctxt_id (Reject (rc, msg))

deleteCallContext :: ICM m => CallId -> m ()
deleteCallContext ctxt_id =
  modifyCallContext ctxt_id $ \ctxt ->
    if (needs_to_respond ctxt == NeedsToRespond True)
    then error "Internal error: deleteCallContext on non-responded call context"
    else if deleted ctxt
    then error "Internal error: deleteCallContext on deleted call context"
    else ctxt { deleted = True }

-- The following functions assume the canister does exist.
-- It would be an internal error if they don't.

getCanister :: ICM m => CanisterId -> m CanState
getCanister cid =
  gets (M.lookup cid . canisters)
    `orElse` error ("canister does not exist: " ++ prettyID cid)

modCanister :: ICM m => CanisterId -> (CanState -> CanState) -> m ()
modCanister cid f = do
    void $ getCanister cid
    modify $ \ic -> ic { canisters = M.adjust f cid (canisters ic) }

setCanisterContent :: ICM m => CanisterId -> CanisterContent -> m ()
setCanisterContent cid content = modCanister cid $
    \cs -> cs { content = Just content }

modCanisterContent :: ICM m => CanisterId -> (CanisterContent -> CanisterContent) -> m ()
modCanisterContent cid f = do
    modCanister cid $ \c -> c { content = Just (f (fromMaybe err (content c))) }
  where err = error ("canister is empty: " ++ prettyID cid)

setCanisterState :: ICM m => CanisterId -> WasmState -> m ()
setCanisterState cid wasm_state = modCanisterContent cid $
    \cs -> cs { wasm_state = wasm_state }

getControllers :: ICM m => CanisterId -> m (S.Set EntityId)
getControllers cid = controllers <$> getCanister cid

setControllers :: ICM m => CanisterId -> (S.Set EntityId) -> m ()
setControllers cid controllers = modCanister cid $
    \cs -> cs { controllers = controllers }

setComputeAllocation :: ICM m => CanisterId -> Natural -> m ()
setComputeAllocation cid n = modCanister cid $
    \cs -> cs { compute_allocation = n }

setMemoryAllocation :: ICM m => CanisterId -> Natural -> m ()
setMemoryAllocation cid n = modCanister cid $
    \cs -> cs { memory_allocation = n }

setFreezingThreshold :: ICM m => CanisterId -> Natural -> m ()
setFreezingThreshold cid n = modCanister cid $
    \cs -> cs { freezing_threshold = n }

getBalance :: ICM m => CanisterId -> m Natural
getBalance cid = cycle_balance <$> getCanister cid

setBalance :: ICM m => CanisterId -> Natural -> m ()
setBalance cid balance = modCanister cid $
    \cs -> cs { cycle_balance = balance }

setCertifiedData :: ICM m => CanisterId -> Blob -> m ()
setCertifiedData cid b = modCanister cid $
    \cs -> cs { certified_data = b }

getRunStatus :: ICM m => CanisterId -> m RunStatus
getRunStatus cid = run_status <$> getCanister cid

setRunStatus :: ICM m => CanisterId -> RunStatus -> m ()
setRunStatus cid run_status = modCanister cid $
    \cs -> cs { run_status = run_status }

getCanisterState :: ICM m => CanisterId -> m WasmState
getCanisterState cid = wasm_state . fromJust . content <$> getCanister cid

getCanisterMod :: ICM m => CanisterId -> m CanisterModule
getCanisterMod cid = can_mod . fromJust . content <$> getCanister cid

getCanisterTime :: ICM m => CanisterId -> m Timestamp
getCanisterTime cid = time <$> getCanister cid

getCanisterVersion :: ICM m => CanisterId -> m Natural
getCanisterVersion cid = canister_version <$> getCanister cid

bumpCanisterVersion :: ICM m => CanisterId -> m ()
bumpCanisterVersion cid = modCanister cid $
    \cs -> cs { canister_version = canister_version cs + 1 }

getCanisterHistory :: ICM m => CanisterId -> m CanisterHistory
getCanisterHistory cid = canister_history <$> getCanister cid

addCanisterHistory :: ICM m => CanisterId -> Change -> m ()
addCanisterHistory cid c = modCanister cid $
    \cs -> cs { canister_history = CanisterHistory (take 20 $ c : changes (canister_history cs)) (total_num_changes (canister_history cs) + 1) }

getCanisterGlobalTimer :: ICM m => CanisterId -> m Natural
getCanisterGlobalTimer cid = global_timer <$> getCanister cid

setCanisterGlobalTimer :: ICM m => CanisterId -> Natural -> m ()
setCanisterGlobalTimer cid ts = modCanister cid $
    \cs -> cs { global_timer = ts }

module_hash :: CanState -> Maybe Blob
module_hash = fmap (raw_wasm_hash . can_mod) . content

idle_cycles_burned_per_day :: CanState -> Natural
idle_cycles_burned_per_day _ = fromInteger 0

canisterEnv :: ICM m => CanisterId -> m Env
canisterEnv canister_id = do
  env_time <- getCanisterTime canister_id
  env_balance <- getBalance canister_id
  env_status <- getRunStatus canister_id <&> \case
      IsRunning -> Running
      IsStopping _pending -> Stopping
      IsStopped -> Stopped
      IsDeleted -> error "deleted canister encountered"
  env_canister_version <- getCanisterVersion canister_id
  env_global_timer <- getCanisterGlobalTimer canister_id
  can_state <- getCanister canister_id
  return $ Env
    { env_self = canister_id
    , env_time
    , env_balance
    , env_status
    , env_certificate = Nothing
    , env_canister_version
    , env_global_timer
    , env_controllers = controllers can_state
    }

performCanisterActions :: ICM m => CanisterId -> CanisterActions -> m ()
performCanisterActions cid ca = do
  mapM_ (setCertifiedData cid) (set_certified_data ca)
  mapM_ (setCanisterGlobalTimer cid) (set_global_timer ca)

