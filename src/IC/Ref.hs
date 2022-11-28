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

{-|
This module implements the main abstract logic of the Internet Computer. It
assumes a pure and abstracted view on Canisters (provided by "IC.Canister"),
and deals with abstract requests ('CallRequest', 'QueryRequest', ...), so HTTP
and CBOR-level processing has already happened.
-}
module IC.Ref
  ( IC(..)
  , CallRequest(..)
  , callerOfCallRequest
  , QueryRequest(..)
  , ReadStateRequest(..)
  , RequestStatus(..)
  , ReqResponse(..)
  , CallResponse(..)
  , initialIC
  , authCallRequest
  , authQueryRequest
  , authReadStateRequest
  , submitRequest
  , handleQuery
  , handleReadState
  , runStep
  , runToCompletion
  , processSystemTasks
  -- $ Exported for use as a library, e.g. in testing
  , setAllTimesTo
  , createEmptyCanister
  -- $ Exported merely for debug introspection
  , CallContext(..)
  , Message(..)
  , CanState(..)
  , CallOrigin(..)
  , EntryPoint(..)
  , RunStatus(..)
  , CanisterContent(..)
  )
where

import qualified Data.Map as M
import qualified Data.Row as R
import qualified Data.Row.Variants as V
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Data.Set as S
import qualified Data.Word as W
import Data.List
import Data.Maybe
import Numeric.Natural
import Data.Functor
import Control.Monad.State.Class
import Control.Monad.Except
import Control.Monad.Random.Lazy
import Data.Sequence (Seq(..))
import Data.Foldable (toList)
import Codec.Candid
import Data.Row ((.==), (.+), (.!), type (.!))
import GHC.Stack

import IC.Types
import IC.Constants
import IC.Canister
import IC.CBOR.Utils
import IC.Id.Fresh
import IC.Id.Forms(mkSelfAuthenticatingId)
import IC.Utils
import IC.Management
import IC.HashTree hiding (Blob)
import IC.Certificate
import IC.Certificate.Value
import IC.Certificate.CBOR
import IC.Crypto
import IC.Crypto.Bitcoin as Bitcoin
import IC.Ref.IO (sendHttpRequest)

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

data ReqResponse
  = QueryResponse CallResponse
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

data IC = IC
  { canisters :: CanisterId ↦ CanState
  , requests :: RequestID ↦ (CallRequest, (RequestStatus, CanisterId))
  , messages :: Seq Message
  , call_contexts :: CallId ↦ CallContext
  , rng :: StdGen
  , secretRootKey :: SecretKey
  , rootSubnet :: Maybe EntityId
  , subnets :: [(EntityId, SubnetType, SecretKey, [(W.Word64, W.Word64)])]
  }
  deriving (Show)

-- The functions below want stateful access to a value of type 'IC'
type ICM m = (MonadState IC m, HasCallStack, MonadIO m)

initialIC :: [SubnetConfig] -> IO IC
initialIC subnets = do
    let root_subnet = find (\conf -> subnet_type conf == System) subnets
    let sk = case root_subnet of Nothing -> createSecretKeyBLS "ic-ref's very secure secret root key"
                                 Just conf -> key conf
    IC mempty mempty mempty mempty <$> newStdGen <*> pure sk <*> pure (fmap (sub_id . key) root_subnet) <*> pure (map sub subnets)
  where
    sub conf = (sub_id (key conf), subnet_type conf, key conf, canister_ranges conf)
    sub_id = EntityId . mkSelfAuthenticatingId . toPublicKey
    key = createSecretKeyBLS . BLU.fromString . nonce

-- Request handling

findRequest :: RequestID -> IC -> Maybe (CallRequest, (RequestStatus, CanisterId))
findRequest rid ic = M.lookup rid (requests ic)

setReqStatus :: ICM m => RequestID -> RequestStatus -> m ()
setReqStatus rid s = modify $ \ic ->
  ic { requests = M.adjust (\(r,(_,ecid)) -> (r,(s,ecid))) rid (requests ic) }

calleeOfCallRequest :: CallRequest -> EntityId
calleeOfCallRequest = \case
    CallRequest canister_id _ _ _ -> canister_id

callerOfCallRequest :: CallRequest -> EntityId
callerOfCallRequest = \case
    CallRequest _ user_id _ _ -> user_id

callerOfRequest :: ICM m => RequestID -> m EntityId
callerOfRequest rid = gets (M.lookup rid . requests) >>= \case
    Just (ar,_) -> return (callerOfCallRequest ar)
    Nothing -> error "callerOfRequest"


-- Canister handling

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
      , global_timer = 0
      , last_action = Nothing
      }

doesCanisterExist :: ICM m => CanisterId -> m Bool
doesCanisterExist cid =
  gets (M.lookup cid . canisters) >>= \case
    Nothing -> return False
    Just CanState{ run_status = IsDeleted } -> return False
    _ -> return True

canisterMustExist :: (CanReject m, ICM m) => CanisterId -> m ()
canisterMustExist cid =
  gets (M.lookup cid . canisters) >>= \case
    Nothing ->
      reject RC_DESTINATION_INVALID ("canister does not exist: " ++ prettyID cid) (Just EC_CANISTER_NOT_FOUND)
    Just CanState{ run_status = IsDeleted } ->
      reject RC_DESTINATION_INVALID ("canister no longer exists: " ++ prettyID cid) (Just EC_CANISTER_NOT_FOUND)
    _ -> return ()

isCanisterRunning :: ICM m => CanisterId -> m Bool
isCanisterRunning cid = getRunStatus cid >>= \case
    IsRunning -> return True
    _         -> return False

isCanisterEmpty :: ICM m => CanisterId -> m Bool
isCanisterEmpty cid = isNothing . content <$> getCanister cid

getCanisterRootKey :: CanisterId -> Bitcoin.ExtendedSecretKey
getCanisterRootKey cid = Bitcoin.createExtendedKey $ rawEntityId cid 

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

getCanisterGlobalTimer :: ICM m => CanisterId -> m Natural
getCanisterGlobalTimer cid = global_timer <$> getCanister cid

setCanisterGlobalTimer :: ICM m => CanisterId -> Natural -> m ()
setCanisterGlobalTimer cid ts = modCanister cid $
    \cs -> cs { global_timer = ts }

module_hash :: CanState -> Maybe Blob
module_hash = fmap (raw_wasm_hash . can_mod) . content

idle_cycles_burned_per_day :: CanState -> Natural
idle_cycles_burned_per_day _ = fromInteger 0

-- Authentication and authorization of requests
--
-- The envelope has already been validated. So this includes
--  * Comparing the envelope validity with the contents
--  * Authorization of the sender
--  * ingress message inspection
--  * checking the correct effective id

type RequestValidation m = (MonadError T.Text m, ICM m)

authCallRequest :: RequestValidation m => Timestamp -> CanisterId -> EnvValidity -> CallRequest -> m ()
authCallRequest t ecid ev r@(CallRequest canister_id user_id meth arg) = do
    checkEffectiveCanisterID ecid canister_id meth arg
    valid_when ev t
    valid_for ev user_id
    valid_where ev canister_id
    inspectIngress r

authQueryRequest :: RequestValidation m => Timestamp -> CanisterId -> EnvValidity -> QueryRequest -> m ()
authQueryRequest t ecid ev (QueryRequest canister_id user_id meth arg) = do
    checkEffectiveCanisterID ecid canister_id meth arg
    valid_when ev t
    valid_for ev user_id
    valid_where ev canister_id

authReadStateRequest :: RequestValidation m => Timestamp -> CanisterId -> EnvValidity -> ReadStateRequest -> m ()
authReadStateRequest t ecid ev (ReadStateRequest user_id paths) = do
    valid_when ev t
    valid_for ev user_id
    -- Implement ACL for read requests here
    forM_ paths $ \case
      ["time"] -> return ()
      ("subnet":_) -> return ()
      ("canister":cid:"module_hash":_) ->
        assertEffectiveCanisterId ecid (EntityId cid)
      ("canister":cid:"controllers":_) ->
        assertEffectiveCanisterId ecid (EntityId cid)
      ("canister":cid:"metadata":name:_) -> do
        assertEffectiveCanisterId ecid (EntityId cid)
        name <- case fromUtf8 name of
            Nothing -> throwError "Invalid utf8 in metadata path"
            Just name -> pure name
        ex <- doesCanisterExist ecid
        when ex $ do
          mod <- getCanisterMod ecid
          cs <- getControllers ecid
          case M.lookup name (metadata mod) of
            Just (False, _) -> -- private
              unless (S.member user_id cs) $
                throwError "User is not authorized to read this metadata field"
            _ -> return () -- public or absent
      ("request_status":rid: _) | BS.length rid /= 32 -> throwError "Request IDs must be 32 bytes in length."
      ("request_status":rid: _) ->                            
        gets (findRequest rid) >>= \case
          Just (ar@(CallRequest cid _ meth arg),_) -> do
            checkEffectiveCanisterID ecid cid meth arg
            unless (user_id == callerOfCallRequest ar) $
              throwError "User is not authorized to read this request status"
            valid_where ev (calleeOfCallRequest ar)
          Nothing -> return ()
      _ -> throwError "User is not authorized to read unspecified state paths"

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
  return $ Env
    { env_self = canister_id
    , env_time
    , env_balance
    , env_status
    , env_certificate = Nothing
    , env_canister_version
    , env_global_timer
    }

-- Synchronous requests

handleQuery :: ICM m => Timestamp -> QueryRequest -> m ReqResponse
handleQuery time (QueryRequest canister_id user_id method arg) =
  fmap QueryResponse $ onReject (return . canisterRejected) $ do
    canisterMustExist canister_id
    is_running <- isCanisterRunning canister_id
    when (not is_running) $ reject RC_CANISTER_ERROR "canister is stopped" (Just EC_CANISTER_STOPPED)
    empty <- isCanisterEmpty canister_id
    when empty $ reject RC_DESTINATION_INVALID "canister is empty" (Just EC_CANISTER_EMPTY)
    wasm_state <- getCanisterState canister_id
    can_mod <- getCanisterMod canister_id
    certificate <- getDataCertificate time canister_id
    env0 <- canisterEnv canister_id
    let env = env0 { env_certificate = Just certificate }

    f <- return (M.lookup method (query_methods can_mod))
      `orElse` reject RC_DESTINATION_INVALID "query method does not exist" (Just EC_METHOD_NOT_FOUND)

    case f user_id env arg wasm_state of
      Trap msg -> reject RC_CANISTER_ERROR ("canister trapped: " ++ msg) (Just EC_CANISTER_TRAPPED)
      Return (Reject (rc, rm)) -> reject rc rm (Just EC_CANISTER_REJECTED)
      Return (Reply res) -> return $ Replied res

handleReadState :: ICM m => Timestamp -> CanisterId -> ReadStateRequest -> m (Either T.Text ReqResponse)
handleReadState time ecid (ReadStateRequest _sender paths) = onReject (\(_, s, _) -> return $ Left $ T.pack s) $ do
    -- NB: Already authorized in authSyncRequest
    cert <- getPrunedCertificate time ecid (["time"] : paths)
    return $ Right $ ReadStateResponse cert

checkEffectiveCanisterID :: RequestValidation m => CanisterId -> CanisterId -> MethodName -> Blob -> m ()
checkEffectiveCanisterID ecid cid method arg
  | cid == managementCanisterId =
    if | method == "provisional_create_canister_with_cycles" -> pure ()
       | method `elem` ["raw_rand", "http_request", "ecdsa_public_key", "sign_with_ecdsa"] -> 
         throwError $ T.pack method <>  " cannot be invoked via ingress calls"
       | otherwise -> case Codec.Candid.decode @(R.Rec ("canister_id" R..== Principal)) arg of
                        Left err ->
                          throwError $ "call to management canister is not valid candid: " <> T.pack err
                        Right r ->
                          assertEffectiveCanisterId ecid (principalToEntityId (r .! #canister_id))
  | otherwise = assertEffectiveCanisterId ecid cid

assertEffectiveCanisterId :: RequestValidation m => CanisterId -> CanisterId -> m ()
assertEffectiveCanisterId ecid cid = do
  unless (ecid == cid) $ do
    throwError $ "expected effective canister_id " <> T.pack (prettyID cid) <> ", got " <> T.pack (prettyID ecid)

inspectIngress :: RequestValidation m => CallRequest -> m ()
inspectIngress (CallRequest canister_id user_id method arg)
  | canister_id == managementCanisterId =
    if| method `elem` ["provisional_create_canister_with_cycles", "provisional_top_up_canister"]
      -> return ()
      | method `elem` [ "raw_rand", "deposit_cycles", "http_request", "ecdsa_public_key", "sign_with_ecdsa" ]
      -> throwError $ "Management method " <> T.pack method <> " cannot be invoked via an ingress call"
      | method `elem` managementMethods
      -> case decode @(R.Rec ("canister_id" R..== Principal)) arg of
        Left msg -> throwError $ "Candid failed to decode: " <> T.pack msg
        Right r -> do
            let canister_id = principalToEntityId $ r .! #canister_id
            onReject (throwError . T.pack . rejectMessage) $
                canisterMustExist canister_id
            controllers <- getControllers canister_id
            unless (user_id `S.member` controllers) $
                throwError "Wrong sender"
      | otherwise
      -> throwError $ "Unknown management method " <> T.pack method
  | otherwise = do
    onReject (throwError . T.pack . rejectMessage) $
        canisterMustExist canister_id
    getRunStatus canister_id >>= \case
       IsRunning -> return ()
       _ -> throwError "canister is stopped"
    empty <- isCanisterEmpty canister_id
    when empty $ throwError "canister is empty"
    wasm_state <- getCanisterState canister_id
    can_mod <- getCanisterMod canister_id
    env <- canisterEnv canister_id

    case inspect_message can_mod method user_id env arg wasm_state of
      Trap msg -> throwError $ "canister trapped in inspect_message: " <> T.pack msg
      Return () -> return ()

-- The state tree

stateTree :: Timestamp -> IC -> LabeledTree
stateTree (Timestamp t) ic = node
  [ "time" =: val t
  , "subnet" =: node (map subnet_tree (subnets ic))
  , "request_status" =: node
    [ rid =: case rs of
        Received -> node
          [ "status" =: str "received" ]
        Processing -> node
          [ "status" =: str "processing" ]
        CallResponse (Replied r) -> node
          [ "status" =: str "replied"
          , "reply" =: val r
          ]
        CallResponse (Rejected (c, msg, err)) -> node $
          [ "error_code" =: val (T.pack $ errorCode code) | code <- maybeToList err] ++
          [ "status" =: str "rejected"
          , "reject_code" =: val (rejectCode c)
          , "reject_message" =: val (T.pack msg)
          ]
    | (rid, (_, (rs, _))) <- M.toList (requests ic)
    ]
  , "canister" =: node
    [ cid =: node (
      [ "certified_data" =: val (certified_data cs)
      , "controllers" =: val (encodePrincipalList (S.toList (controllers cs)))
      ] ++
      ( case content cs of
        Nothing -> []
        Just cc ->
          [ "metadata" =: node
            [ toUtf8 n =: val c | (n,(_,c)) <- M.toList (metadata (can_mod cc)) ]
          , "module_hash" =: val (raw_wasm_hash (can_mod cc))
          ]
      )
    )
    | (EntityId cid, cs) <- M.toList (canisters ic)
    , run_status cs /= IsDeleted
    ]
  ]
  where
    node = SubTrees . mconcat
    val :: CertVal a => a -> LabeledTree
    val = Value . toCertVal
    str = val @T.Text
    (=:) = M.singleton
    subnet_tree (EntityId subnet_id, _, _, ranges) = subnet_id =: node (
        [ "public_key" =: val subnet_id
        , "canister_ranges" =: val (encodeCanisterRangeList $ map (\(a, b) -> (wordToId a, wordToId b)) ranges)
        ]
      )

delegationTree :: Timestamp -> SubnetId -> Blob -> [(W.Word64, W.Word64)] -> LabeledTree
delegationTree (Timestamp t) (EntityId subnet_id) subnet_pub_key ranges = node
  [ "time" =: val t
  , "subnet" =: node
    [ subnet_id =: node (
          [ "public_key" =: val subnet_pub_key
          , "canister_ranges" =: val (encodeCanisterRangeList $ map (\(a, b) -> (wordToId a, wordToId b)) ranges)
          ]
      )
    ]
  ]
  where
    node = SubTrees . mconcat
    val :: CertVal a => a -> LabeledTree
    val = Value . toCertVal
    (=:) = M.singleton

getSubnetFromCanisterId :: (CanReject m, ICM m) => CanisterId -> m (EntityId, SubnetType, SecretKey, [(W.Word64, W.Word64)])
getSubnetFromCanisterId cid = do
    subnets <- gets subnets
    case subnetOfCid cid subnets of
      Nothing -> reject RC_SYS_FATAL "Canister id does not belong to any subnet." Nothing
      Just x -> return x
    where
      subnetOfCid cid subnets = find (\(_, _, _, ranges) -> find (\(a, b) -> wordToId a <= cid && cid <= wordToId b) ranges /= Nothing) subnets

getPrunedCertificate :: (CanReject m, ICM m) => Timestamp -> CanisterId -> [Path] -> m Certificate
getPrunedCertificate time ecid paths = do
    root_subnet <- gets rootSubnet
    sk1 <- gets secretRootKey
    (subnet_id, _, sk2, ranges) <- getSubnetFromCanisterId ecid
    full_tree <- gets (construct . stateTree time)
    let cert_tree = prune full_tree (["time"] : paths)
    return $ signCertificate time sk1 (if root_subnet == Just subnet_id then Nothing else Just (subnet_id, sk2, ranges)) cert_tree

signCertificate :: Timestamp -> SecretKey -> Maybe (SubnetId, SecretKey, [(W.Word64, W.Word64)]) -> HashTree -> Certificate
signCertificate time rootKey (Just (subnet_id, subnet_key, ranges)) cert_tree =
    Certificate { cert_tree, cert_sig, cert_delegation }
 where
    cert_sig = signPure "ic-state-root" subnet_key (reconstruct cert_tree)
    cert_delegation = Just $ Delegation { del_subnet_id, del_certificate }
    del_subnet_id = rawEntityId subnet_id
    del_certificate =
      encodeCert $
      signCertificate time rootKey Nothing $
      construct $
      delegationTree time subnet_id (toPublicKey subnet_key) ranges

signCertificate _time rootKey Nothing cert_tree =
    Certificate { cert_tree, cert_sig, cert_delegation = Nothing }
 where
    cert_sig = signPure "ic-state-root" rootKey (reconstruct cert_tree)

-- If `stateTree` ever becomes a bottleneck:
-- Since ic-ref creates a fresh state tree everytime it is used, we _could_
-- construct one with just the required data, e.g. only of the canister in
-- question. That would not be secure, but `ic-ref` doesn’t have to be.
getDataCertificate :: (CanReject m, ICM m) => Timestamp -> CanisterId -> m Blob
getDataCertificate t cid = do
    encodeCert <$> getPrunedCertificate t cid
        [["time"], ["canister", rawEntityId cid, "certified_data"]]

-- Asynchronous requests

-- | Submission simply enqueues requests

submitRequest :: ICM m => RequestID -> CallRequest -> CanisterId -> m ()
submitRequest rid r ecid = modify $ \ic ->
  if M.member rid (requests ic)
  then ic
  else ic { requests = M.insert rid (r, (Received, ecid)) (requests ic) }


-- | Eventually, they are processed

processRequest :: ICM m => (RequestID, CallRequest, CanisterId) -> m ()
processRequest (rid, CallRequest canister_id _user_id method arg, ecid) =
  onReject (setReqStatus rid . CallResponse . canisterRejected) $ do
    ctxt_id <- newCallContext $ CallContext
      { canister = canister_id
      , origin = FromUser rid ecid
      , needs_to_respond = NeedsToRespond True
      , deleted = False
      , last_trap = Nothing
      , available_cycles = 0
      }
    enqueueMessage $ CallMessage
      { call_context = ctxt_id
      , entry = Public method arg
      }
    setReqStatus rid Processing

-- Call context handling

newCallContext :: ICM m => CallContext -> m CallId
newCallContext cc = state $ \ic ->
  let i = freshKey (call_contexts ic)
  in (i, ic { call_contexts = M.insert i cc (call_contexts ic)})

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

rememberTrap :: ICM m => CallId -> String -> m ()
rememberTrap ctxt_id msg =
  modifyCallContext ctxt_id $ \ctxt -> ctxt { last_trap = Just msg }

callerOfCallID :: ICM m => CallId -> m EntityId
callerOfCallID ctxt_id = do
  ctxt <- getCallContext ctxt_id
  case origin ctxt of
    FromUser rid _ -> callerOfRequest rid
    FromCanister other_ctxt_id _callback -> calleeOfCallID other_ctxt_id
    FromSystemTask -> return $ canister ctxt

calleeOfCallID :: ICM m => CallId -> m EntityId
calleeOfCallID ctxt_id = canister <$> getCallContext ctxt_id

needsToRespondCallID :: ICM m => CallId -> m NeedsToRespond
needsToRespondCallID ctxt_id = needs_to_respond <$> getCallContext ctxt_id

deletedCallID :: ICM m => CallId -> m Bool
deletedCallID ctxt_id = deleted <$> getCallContext ctxt_id

starveCallContext :: ICM m => CallId -> m ()
starveCallContext ctxt_id = do
  ctxt <- getCallContext ctxt_id
  let (msg, err) | Just t <- last_trap ctxt = ("canister trapped: " ++ t, EC_CANISTER_TRAPPED)
                 | otherwise                = ("canister did not respond", EC_CANISTER_DID_NOT_REPLY)
  rejectCallContext ctxt_id (RC_CANISTER_ERROR, msg, Just err)

ecidOfCallID :: ICM m => CallId -> m CanisterId
ecidOfCallID ctxt_id = do
  ctxt <- getCallContext ctxt_id
  case origin ctxt of
    FromUser _rid ecid -> return ecid
    _ -> callerOfCallID ctxt_id

-- Message handling

enqueueMessage :: ICM m => Message -> m ()
enqueueMessage m = modify $ \ic -> ic { messages = messages ic :|> m }

processMessage :: ICM m => Message -> m ()
processMessage m = case m of
  CallMessage ctxt_id entry -> onReject (rejectCallContext ctxt_id) $ do
    callee <- calleeOfCallID ctxt_id
    if callee == managementCanisterId
    then do
      caller <- callerOfCallID ctxt_id
      rejectAsCanister $
        invokeManagementCanister caller ctxt_id entry
    else do
      canisterMustExist callee
      status <- getRunStatus callee
      case (status, entry) of
          (IsRunning, _) -> return ()
          (IsStopping _, Closure{}) -> return ()
          -- This is a hack, detecting callbacks via the entry, and demands refactoring
          _ -> reject RC_CANISTER_ERROR "canister is not running" (Just EC_CANISTER_NOT_RUNNING)
      empty <- isCanisterEmpty callee
      when empty $ reject RC_DESTINATION_INVALID "canister is empty" (Just EC_CANISTER_EMPTY) -- NB: An empty canister cannot receive a callback.
      wasm_state <- getCanisterState callee
      can_mod <- getCanisterMod callee
      env <- canisterEnv callee
      invokeEntry ctxt_id wasm_state can_mod env entry >>= \case
        Trap msg -> do
          -- Eventually update cycle balance here
          rememberTrap ctxt_id msg
        Return (new_state, (call_actions, canister_actions)) -> do
          modCanister callee $ \cs -> cs { last_action = Just entry }
          performCallActions ctxt_id call_actions
          performCanisterActions callee canister_actions
          setCanisterState callee new_state

  ResponseMessage ctxt_id response refunded_cycles -> do
    ctxt <- getCallContext ctxt_id
    case origin ctxt of
      FromSystemTask -> error "Response from system task"
      FromUser rid _ -> setReqStatus rid $ CallResponse $
        -- NB: Here cycles disappear
        case response of
          Reject (rc, msg) -> canisterRejected (rc, msg, Nothing)
          Reply blob -> Replied blob
      FromCanister other_ctxt_id callback -> do
        -- Add refund to balance
        cid <- calleeOfCallID other_ctxt_id
        prev_balance <- getBalance cid
        setBalance cid $ prev_balance + refunded_cycles
        -- Unless deleted
        d <- deletedCallID other_ctxt_id
        unless d $
          -- Enqueue execution
          enqueueMessage $ CallMessage
            { call_context = other_ctxt_id
            , entry = Closure callback response refunded_cycles
            }

performCallActions :: ICM m => CallId -> CallActions -> m ()
performCallActions ctxt_id ca = do
  updateBalances ctxt_id (ca_new_calls ca) (ca_accept ca)
  mapM_ (newCall ctxt_id) (ca_new_calls ca)
  mapM_ (respondCallContext ctxt_id) (ca_response ca)


performCanisterActions :: ICM m => CanisterId -> CanisterActions -> m ()
performCanisterActions cid ca = do
  mapM_ (setCertifiedData cid) (set_certified_data ca)
  mapM_ (setCanisterGlobalTimer cid) (set_global_timer ca)

updateBalances :: ICM m => CallId -> [MethodCall] -> Cycles -> m ()
updateBalances ctxt_id new_calls accepted = do
  cid <- calleeOfCallID ctxt_id

  -- Eventually update when we track cycle consumption
  let max_cycles = 0
  let cycles_consumed = 0

  prev_balance <- getBalance cid
  available <- getCallContextCycles ctxt_id
  if accepted <= available
  then do
    let to_spend = prev_balance + accepted - max_cycles
    let transferred = sum [ call_transferred_cycles c | c <- new_calls]
    if transferred <= to_spend
    then do
      setBalance cid $ prev_balance
        + accepted
        - cycles_consumed
        - transferred
      setCallContextCycles ctxt_id $ available - accepted
    else error "Internal error: More cycles transferred than available"
  else error "Internal error: More cycles accepted than available"


managementCanisterId :: EntityId
managementCanisterId = EntityId mempty


invokeManagementCanister ::
  forall m. (CanReject m, ICM m, MonadIO m) => EntityId -> CallId -> EntryPoint -> m ()
invokeManagementCanister caller ctxt_id (Public method_name arg) =
  case method_name of
      "create_canister" -> atomic $ icCreateCanister caller ctxt_id
      "install_code" -> atomic $ onlyController caller $ icInstallCode caller
      "uninstall_code" -> atomic $ onlyController caller $ icUninstallCode
      "update_settings" -> atomic $ onlyController caller icUpdateCanisterSettings
      "start_canister" -> atomic $ onlyController caller icStartCanister
      "stop_canister" -> deferred $ onlyController caller $ icStopCanister ctxt_id
      "canister_status" -> atomic $ onlyController caller icCanisterStatus
      "delete_canister" -> atomic $ onlyController caller icDeleteCanister
      "deposit_cycles" -> atomic $ icDepositCycles ctxt_id
      "provisional_create_canister_with_cycles" -> atomic $ icCreateCanisterWithCycles caller ctxt_id
      "provisional_top_up_canister" -> atomic icTopUpCanister
      "raw_rand" -> atomic icRawRand
      "http_request" -> atomic $ icHttpRequest caller
      "ecdsa_public_key" -> atomic $ icEcdsaPublicKey caller
      "sign_with_ecdsa" -> atomic $ icSignWithEcdsa caller
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

invokeManagementCanister _ _ Closure{} = error "closure invoked on management canister"
invokeManagementCanister _ _ Heartbeat = error "heartbeat invoked on management canister"
invokeManagementCanister _ _ GlobalTimer = error "global timer invoked on management canister"

icHttpRequest :: (ICM m, CanReject m) => EntityId -> ICManagement m .! "http_request"
icHttpRequest caller r = do
    resp <- liftIO $ sendHttpRequest (r .! #url)
    case (r .! #transform) of
      Nothing -> return resp
      Just t -> case t .! #function of
        FuncRef p m -> do
            let cid = principalToEntityId p
            let arg = R.empty
                  .+ #response .== resp
                  .+ #context .== t .! #context
            unless (cid == caller) $
              reject RC_CANISTER_REJECT "transform needs to be exported by a caller canister" (Just EC_CANISTER_REJECTED)
            can_mod <- getCanisterMod cid
            wasm_state <- getCanisterState cid
            env <- canisterEnv cid
            case M.lookup (T.unpack m) (query_methods can_mod) of
              Nothing -> reject RC_DESTINATION_INVALID "transform function with a given name does not exist" (Just EC_METHOD_NOT_FOUND)
              Just f -> case f managementCanisterId env (Codec.Candid.encode arg) wasm_state of
                Return (Reply r) -> case Codec.Candid.decode @HttpResponse r of
                  Left _ -> reject RC_CANISTER_ERROR "could not decode the response" (Just EC_INVALID_ENCODING)
                  Right r -> return r
                _ -> reject RC_CANISTER_ERROR "canister did not return a response properly" (Just EC_CANISTER_DID_NOT_REPLY)

icCreateCanister :: (ICM m, CanReject m) => EntityId -> CallId -> ICManagement m .! "create_canister"
icCreateCanister caller ctxt_id r = do
    forM_ (r .! #settings) validateSettings
    available <- getCallContextCycles ctxt_id
    setCallContextCycles ctxt_id 0
    (_, _, _, ranges) <- getSubnetFromCanisterId caller
    cid <- icCreateCanisterCommon ranges caller available
    forM_ (r .! #settings) $ applySettings cid
    return (#canister_id .== entityIdToPrincipal cid)

icCreateCanisterWithCycles :: (ICM m, CanReject m) => EntityId -> CallId -> ICManagement m .! "provisional_create_canister_with_cycles"
icCreateCanisterWithCycles caller ctxt_id r = do
    forM_ (r .! #settings) validateSettings
    ecid <- ecidOfCallID ctxt_id
    (_, _, _, ranges) <- getSubnetFromCanisterId ecid
    cid <- icCreateCanisterCommon ranges caller (fromMaybe cDEFAULT_PROVISIONAL_CYCLES_BALANCE (r .! #amount))
    forM_ (r .! #settings) $ applySettings cid
    return (#canister_id .== entityIdToPrincipal cid)

icCreateCanisterCommon :: (ICM m, CanReject m) => [(W.Word64, W.Word64)] -> EntityId -> Natural -> m EntityId
icCreateCanisterCommon ranges controller amount = do
    taken <- gets (M.keys . canisters)
    case freshId ranges taken of
      Nothing -> reject RC_SYS_FATAL ("Could not create canister. Subnet has surpassed its canister ID allocation.") Nothing
      Just new_id -> do
        let currentTime = 0 -- ic-ref lives in the 70ies
        createEmptyCanister new_id (S.singleton controller) currentTime
        -- Here we fill up the canister with the cycles provided by the caller
        setBalance new_id amount
        return new_id

validateSettings :: CanReject m => Settings -> m ()
validateSettings r = do
    forM_ (r .! #compute_allocation) $ \n -> do
        unless (n <= 100) $
            reject RC_SYS_FATAL  "Compute allocation not <= 100" (Just EC_INVALID_ARGUMENT)
    forM_ (r .! #memory_allocation) $ \n -> do
        unless (n <= 2^(48::Int)) $
            reject RC_SYS_FATAL  "Memory allocation not <= 2^48" (Just EC_INVALID_ARGUMENT)
    forM_ (r .! #freezing_threshold) $ \n -> do
        unless (n < 2^(64::Int)) $
            reject RC_SYS_FATAL  "Memory allocation not < 2^64" (Just EC_INVALID_ARGUMENT)
    forM_ (r .! #controllers) $ \n -> do
        unless (length n <= 10) $
            reject RC_SYS_FATAL  "Controllers cannot be > 10" (Just EC_INVALID_ARGUMENT)

applySettings :: ICM m => EntityId -> Settings -> m ()
applySettings cid r = do
    forM_ (r .! #controllers) $ setControllers cid . S.fromList . map principalToEntityId . Vec.toList
    forM_ (r .! #compute_allocation) $ setComputeAllocation cid
    forM_ (r .! #memory_allocation) $ setMemoryAllocation cid
    forM_ (r .! #freezing_threshold) $ setFreezingThreshold cid

onlyController ::
  (ICM m, CanReject m, (r .! "canister_id") ~ Principal) =>
  EntityId -> (R.Rec r -> m a) -> (R.Rec r -> m a)
onlyController caller act r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    canisterMustExist canister_id
    controllers <- getControllers canister_id
    if caller `S.member` controllers
    then act r
    else reject RC_SYS_FATAL (
        prettyID caller <> " is not authorized to manage canister " <>
        prettyID canister_id <> ", Controllers are: " <> intercalate ", " (map prettyID (S.toList controllers)))
        (Just EC_NOT_AUTHORIZED)

icInstallCode :: (ICM m, CanReject m) => EntityId -> ICManagement m .! "install_code"
icInstallCode caller r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    let arg = r .! #arg
    new_can_mod <- return (parseCanister (r .! #wasm_module))
      `onErr` (\err -> reject RC_SYS_FATAL ("Parsing failed: " ++ err) (Just EC_INVALID_MODULE))
    was_empty <- isCanisterEmpty canister_id

    let
      reinstall = do
        env <- canisterEnv canister_id
        let env1 = env { env_canister_version = env_canister_version env + 1, env_global_timer = 0 }
        (wasm_state, ca) <- return (init_method new_can_mod caller env1 arg)
          `onTrap` (\msg -> reject RC_CANISTER_ERROR ("Initialization trapped: " ++ msg) (Just EC_CANISTER_TRAPPED))
        setCanisterContent canister_id $ CanisterContent
            { can_mod = new_can_mod
            , wasm_state = wasm_state
            }
        performCanisterActions canister_id ca
        bumpCanisterVersion canister_id
        when (set_global_timer ca == Nothing) $ setCanisterGlobalTimer canister_id 0

      install = do
        unless was_empty $
          reject RC_DESTINATION_INVALID "canister is not empty during installation" (Just EC_CANISTER_NOT_EMPTY)
        reinstall

      upgrade = do
        when was_empty $
          reject RC_DESTINATION_INVALID "canister is empty during upgrade" (Just EC_CANISTER_EMPTY)
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

actuallyStopCanister :: ICM m => CanisterId -> m ()
actuallyStopCanister canister_id =
    getRunStatus canister_id >>= \case
        IsStopping pending -> do
            setRunStatus canister_id IsStopped
            forM_ pending $ \ctxt_id ->
              replyCallContext ctxt_id (Codec.Candid.encode ())
        IsRunning -> error "unexpected canister status"
        IsStopped -> error "unexpected canister status"
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
        IsRunning -> reject RC_SYS_FATAL "Cannot delete running canister" (Just EC_CANISTER_NOT_STOPPED)
        IsStopping _pending -> reject RC_SYS_FATAL "Cannot delete stopping canister" (Just EC_CANISTER_NOT_STOPPED)
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
    let cid = case r .! #canister_id of
                Just cid -> principalToEntityId cid
                Nothing -> caller
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
 
invokeEntry :: ICM m =>
    CallId -> WasmState -> CanisterModule -> Env -> EntryPoint ->
    m (TrapOr (WasmState, UpdateResult))
invokeEntry ctxt_id wasm_state can_mod env entry = do
    needs_to_respond <- needsToRespondCallID ctxt_id
    available <- getCallContextCycles ctxt_id
    case entry of
      Public method dat -> do
        caller <- callerOfCallID ctxt_id
        case lookupUpdate method can_mod of
          Just f -> return $ f caller env needs_to_respond available dat wasm_state
          Nothing -> do
            let reject = Reject (RC_DESTINATION_INVALID, "method does not exist: " ++ method)
            return $ Return (wasm_state, (noCallActions { ca_response = Just reject}, noCanisterActions))
      Closure cb r refund -> return $ do
        case callbacks can_mod cb env needs_to_respond available r refund wasm_state of
            Trap err -> case cleanup_callback cb of
                Just closure -> case cleanup can_mod closure env wasm_state of
                    Trap err' -> Trap err'
                    Return (wasm_state', ()) ->
                        Return (wasm_state', (noCallActions, noCanisterActions))
                Nothing -> Trap err
            Return (wasm_state, actions) -> Return (wasm_state, actions)
      Heartbeat -> return $ do
        case heartbeat can_mod env wasm_state of
            Trap _ -> Return (wasm_state, (noCallActions, noCanisterActions))
            Return (wasm_state, (calls, actions)) ->
                Return (wasm_state, (noCallActions { ca_new_calls = calls }, actions))
      GlobalTimer -> return $ do
        case canister_global_timer can_mod env wasm_state of
            Trap _ -> Return (wasm_state, (noCallActions, noCanisterActions))
            Return (wasm_state, (calls, actions)) ->
                Return (wasm_state, (noCallActions { ca_new_calls = calls }, actions))
  where
    lookupUpdate method can_mod
        | Just f <- M.lookup method (update_methods can_mod) = Just f
        | Just f <- M.lookup method (query_methods can_mod)  = Just (asUpdate f)
        | otherwise = Nothing

newCall :: ICM m => CallId -> MethodCall -> m ()
newCall from_ctxt_id call = do
  new_ctxt_id <- newCallContext $ CallContext
    { canister = call_callee call
    , origin = FromCanister from_ctxt_id (call_callback call)
    , needs_to_respond = NeedsToRespond True
    , deleted = False
    , last_trap = Nothing
    , available_cycles = call_transferred_cycles call
    }
  enqueueMessage $ CallMessage
    { call_context = new_ctxt_id
    , entry = Public (call_method_name call) (call_arg call)
    }

-- Scheduling

-- | Pick next request in state `received`
nextReceived :: ICM m => m (Maybe (RequestID, CallRequest, CanisterId))
nextReceived = gets $ \ic -> listToMaybe
  [ (rid,r,ecid) | (rid, (r, (Received, ecid))) <- M.toList (requests ic) ]

-- A call context is still waiting for a response if…
willReceiveResponse :: IC -> CallId -> Bool
willReceiveResponse ic c = c `elem`
  -- there is another call context promising to respond to this
  [ c'
  | CallContext { needs_to_respond = NeedsToRespond True, deleted = False, origin = FromCanister c' _}
      <- M.elems (call_contexts ic)
  ] ++
  -- there is an in-flight call or response message:
  [ call_context m | m <- toList (messages ic) ] ++
  -- there this canister is waiting for some canister to stop
  [ c'
  | CanState { run_status = IsStopping pending } <- M.elems (canisters ic)
  , c' <- pending
  ]
  -- NB: this could be implemented more efficient if kepts a counter of
  -- outstanding calls in each call context

-- | Find a starved call context
nextStarved :: ICM m => m (Maybe CallId)
nextStarved = gets $ \ic -> listToMaybe
  [ c
  | (c, CallContext { needs_to_respond = NeedsToRespond True } ) <- M.toList (call_contexts ic)
  , not $ willReceiveResponse ic c
  ]

-- | Find a canister in stopping state that is, well, stopped
nextStoppedCanister :: ICM m => m (Maybe CanisterId)
nextStoppedCanister = gets $ \ic -> listToMaybe
  [ cid
  | (cid, CanState { run_status = IsStopping _ }) <- M.toList (canisters ic)
  -- no call context still waiting for a response
  , null [ ()
    | (c, ctxt) <- M.toList (call_contexts ic)
    , canister ctxt == cid
    , not (deleted ctxt)
    , willReceiveResponse ic c
    ]
  ]


-- | Pick (and remove) next message from queue
popMessage :: ICM m => m (Maybe Message)
popMessage = state $ \ic ->
  case messages ic of
    Empty -> (Nothing, ic)
    m :<| ms -> (Just m, ic { messages = ms })

-- | Fake time increase
bumpTime :: ICM m => m ()
bumpTime = modify $
  \ic -> ic { canisters = M.map (\cs -> cs { time = time cs +1 }) (canisters ic) }

setAllTimesTo :: ICM m => Timestamp -> m ()
setAllTimesTo ts = modify $
  \ic -> ic { canisters = M.map (\cs -> cs { time = ts }) (canisters ic) }

runHeartbeat :: ICM m => CanisterId -> m ()
runHeartbeat cid = do
  can <- getCanister cid
  is_empty   <- isCanisterEmpty cid
  is_running <- isCanisterRunning cid
  when (not (idleSinceLastHeartbeat $ last_action can) && not is_empty && is_running) $ do
    new_ctxt_id <- newCallContext $ CallContext
      { canister = cid
      , origin = FromSystemTask
      , needs_to_respond = NeedsToRespond False
      , deleted = False
      , last_trap = Nothing
      , available_cycles = 0
      }
    processMessage $ CallMessage
      { call_context = new_ctxt_id
      , entry = Heartbeat
      }

runGlobalTimer :: ICM m => CanisterId -> m ()
runGlobalTimer cid = do
  is_empty   <- isCanisterEmpty cid
  is_running <- isCanisterRunning cid
  Timestamp current_time <- getCanisterTime cid
  global_timer <- getCanisterGlobalTimer cid
  let should_fire = global_timer /= 0 && current_time >= global_timer
  when (should_fire && is_running && not is_empty) $ do
    setCanisterGlobalTimer cid 0
    new_ctxt_id <- newCallContext $ CallContext
      { canister = cid
      , origin = FromSystemTask
      , needs_to_respond = NeedsToRespond False
      , deleted = False
      , last_trap = Nothing
      , available_cycles = 0
      }
    processMessage $ CallMessage
      { call_context = new_ctxt_id
      , entry = GlobalTimer
      }

processSystemTasks :: ICM m => m ()
processSystemTasks = do
  cs <- gets (M.keys . canisters)
  forM_ cs runHeartbeat
  forM_ cs runGlobalTimer

idleSinceLastHeartbeat :: Maybe EntryPoint -> Bool
idleSinceLastHeartbeat (Just Heartbeat) = True
idleSinceLastHeartbeat _                = False

-- | Returns true if a step was taken
runStep :: ICM m => m Bool
runStep = do
  bumpTime
  try
    [ with nextReceived processRequest
    , with popMessage processMessage
    , with nextStarved starveCallContext
    , with nextStoppedCanister actuallyStopCanister
    ]
  where
    try = foldr (\g r -> g >>= \case True -> return True; False -> r) (return False)
    with sel act = sel >>= maybe (return False) (\x -> act x >> return True)

runToCompletion :: ICM m => m ()
runToCompletion = repeatWhileTrue runStep



-- Error handling plumbing

type CanReject = MonadError (RejectCode, String, Maybe ErrorCode)
reject :: CanReject m => RejectCode -> String -> Maybe ErrorCode -> m a2
reject code msg err = throwError (code, msg, err)

-- To maintain the abstraction that the management canister is a canister,
-- all its errors are turned into canister errors
isCanisterRejectedCode :: RejectCode -> Bool
isCanisterRejectedCode RC_SYS_FATAL           = False
isCanisterRejectedCode RC_SYS_TRANSIENT       = False
isCanisterRejectedCode RC_DESTINATION_INVALID = True
isCanisterRejectedCode RC_CANISTER_REJECT     = True
isCanisterRejectedCode RC_CANISTER_ERROR      = True

rejectAsCanister :: CanReject m => m a -> m a
rejectAsCanister act = catchError act (\(c, msg, err) -> if isCanisterRejectedCode c then reject c msg err else reject RC_CANISTER_ERROR msg (Just EC_CANISTER_REJECTED))

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

