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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Word as W
import Data.List
import Data.Maybe
import Control.Monad.State.Class
import Control.Monad.Except
import Data.Sequence (Seq(..))
import Data.Foldable (toList)
import Codec.Candid
import Data.Row ((.!))

import IC.Types
import IC.Canister
import IC.CBOR.Utils
import IC.Id.Fresh
import IC.Utils
import IC.Management
import IC.HashTree hiding (Blob)
import IC.Certificate
import IC.Certificate.Value
import IC.Certificate.CBOR
import IC.Crypto
import IC.Ref.Management
import IC.Ref.Types

-- Request handling

findRequest :: RequestID -> IC -> Maybe (CallRequest, (RequestStatus, CanisterId))
findRequest rid ic = M.lookup rid (requests ic)

setReqStatus :: ICM m => RequestID -> RequestStatus -> m ()
setReqStatus rid s = modify $ \ic ->
  ic { requests = M.adjust (\(r,(_,ecid)) -> (r,(s,ecid))) rid (requests ic) }

calleeOfCallRequest :: CallRequest -> EntityId
calleeOfCallRequest = \case
    CallRequest canister_id _ _ _ -> canister_id

-- Canister handling

doesCanisterExist :: ICM m => CanisterId -> m Bool
doesCanisterExist cid =
  gets (M.lookup cid . canisters) >>= \case
    Nothing -> return False
    Just CanState{ run_status = IsDeleted } -> return False
    _ -> return True

isCanisterRunning :: ICM m => CanisterId -> m Bool
isCanisterRunning cid = getRunStatus cid >>= \case
    IsRunning -> return True
    _         -> return False

-- Authentication and authorization of requests
--
-- The envelope has already been validated. So this includes
--  * Comparing the envelope validity with the contents
--  * Authorization of the sender
--  * ingress message inspection
--  * checking the correct effective id

type RequestValidation m = (MonadError ValidationError m, ICM m)

authCallRequest :: RequestValidation m => Timestamp -> CanisterId -> EnvValidity -> CallRequest -> m ()
authCallRequest t ecid ev r@(CallRequest canister_id user_id meth arg) = do
    checkEffectiveCanisterID ecid canister_id meth arg
    res <- runExceptT $ do
        valid_when ev t
        valid_for ev user_id
        valid_where ev canister_id
    case res of Left err -> throwError $ HTTPError $ T.unpack err
                Right () -> return ()
    inspectIngress r

authQueryRequest :: RequestValidation m => Timestamp -> CanisterId -> EnvValidity -> QueryRequest -> m ()
authQueryRequest t ecid ev (QueryRequest canister_id user_id meth arg) = do
    checkEffectiveCanisterID ecid canister_id meth arg
    res <- runExceptT $ do
        valid_when ev t
        valid_for ev user_id
        valid_where ev canister_id
    case res of Left err -> throwError $ HTTPError $ T.unpack err
                Right () -> return ()

authReadStateRequest :: RequestValidation m => Timestamp -> CanisterId -> EnvValidity -> ReadStateRequest -> m ()
authReadStateRequest t ecid ev (ReadStateRequest user_id paths) = do
    res <- runExceptT $ do
        valid_when ev t
        valid_for ev user_id
    case res of Left err -> throwError $ HTTPError $ T.unpack err
                Right () -> return ()
    let request_ids = nub $ concat $ map request_id paths
    unless (length request_ids <= 1) $
      throwError $ HTTPError "Can only request one request ID in request_status paths."
    -- Implement ACL for read requests here
    forM_ paths $ \case
      ("time":_) -> return ()
      ("subnet":_) -> return ()
      ("canister":cid:"module_hash":_) ->
        assertEffectiveCanisterId ecid (EntityId cid)
      ("canister":cid:"controllers":_) ->
        assertEffectiveCanisterId ecid (EntityId cid)
      ("canister":cid:"metadata":name:_) -> do
        assertEffectiveCanisterId ecid (EntityId cid)
        name <- case fromUtf8 name of
            Nothing -> throwError $ HTTPError "Invalid utf8 in metadata path"
            Just name -> pure name
        ex <- doesCanisterExist ecid
        when ex $ do
          mod <- getCanisterMod ecid
          cs <- getControllers ecid
          case M.lookup name (metadata mod) of
            Just (False, _) -> -- private
              unless (S.member user_id cs) $
                throwError $ HTTPError "User is not authorized to read this metadata field"
            _ -> return () -- public or absent
      ("request_status":rid: _) | BS.length rid /= 32 -> throwError $ HTTPError "Request IDs must be 32 bytes in length."
      ("request_status":rid: _) ->                            
        gets (findRequest rid) >>= \case
          Just (ar, (_, ecid')) -> do
            assertEffectiveCanisterId ecid ecid'
            unless (user_id == callerOfCallRequest ar) $
              throwError $ HTTPError "User is not authorized to read this request status"
            res <- runExceptT $ do
              valid_where ev (calleeOfCallRequest ar)
            case res of Left err -> throwError $ HTTPError $ T.unpack err
                        Right () -> return ()
          Nothing -> return ()
      _ -> throwError $ HTTPError "User is not authorized to read unspecified state paths"
  where
    request_id :: Path -> [Label]
    request_id ("request_status":rid:_) = [rid]
    request_id _ = []

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
       | method `elem` ["create_canister", "raw_rand", "http_request", "ecdsa_public_key", "sign_with_ecdsa"] ->
         throwError $ ExecutionError (RC_CANISTER_REJECT, "Management method " ++ method ++ " cannot be invoked via ingress calls", Nothing)
       | otherwise -> case Codec.Candid.decode @(R.Rec ("canister_id" R..== Principal)) arg of
                        Left err ->
                          throwError $ ExecutionError (RC_CANISTER_REJECT, "Candid failed to decode: " ++ err, Nothing)
                        Right r ->
                          assertEffectiveCanisterId ecid (principalToEntityId (r .! #canister_id))
  | otherwise = assertEffectiveCanisterId ecid cid

assertEffectiveCanisterId :: RequestValidation m => CanisterId -> CanisterId -> m ()
assertEffectiveCanisterId ecid cid = do
  unless (ecid == cid) $ do
    throwError $ HTTPError $ "expected effective canister_id " ++ prettyID cid ++ ", got " ++ prettyID ecid

inspectIngress :: RequestValidation m => CallRequest -> m ()
inspectIngress (CallRequest canister_id user_id method arg)
  | canister_id == managementCanisterId =
    if| method `elem` ["provisional_create_canister_with_cycles", "provisional_top_up_canister"]
      -> return ()
      | method `elem` [ "raw_rand", "deposit_cycles", "http_request", "ecdsa_public_key", "sign_with_ecdsa" ]
      -> throwError $ ExecutionError (RC_CANISTER_REJECT, "Management method " ++ method ++ " cannot be invoked via ingress calls", Nothing)
      | method `elem` managementMethods
      -> case decode @(R.Rec ("canister_id" R..== Principal)) arg of
        Left msg -> throwError $ ExecutionError (RC_CANISTER_REJECT, "Candid failed to decode: " ++ msg, Nothing)
        Right r -> do
            let canister_id = principalToEntityId $ r .! #canister_id
            onReject ((\err -> throwError $ ExecutionError (RC_DESTINATION_INVALID, err, Nothing)) . rejectMessage) $
                canisterMustExist canister_id
            controllers <- getControllers canister_id
            unless (user_id `S.member` controllers) $
                throwError $ ExecutionError (RC_CANISTER_ERROR, "Wrong sender", Nothing)
      | otherwise
      -> throwError $ ExecutionError (RC_DESTINATION_INVALID, "Unknown management method " ++ method, Nothing)
  | otherwise = do
    onReject ((\err -> throwError $ ExecutionError (RC_DESTINATION_INVALID, err, Nothing)) . rejectMessage) $
        canisterMustExist canister_id
    empty <- isCanisterEmpty canister_id
    when empty $ throwError $ ExecutionError (RC_DESTINATION_INVALID, "canister is empty", Nothing)
    wasm_state <- getCanisterState canister_id
    can_mod <- getCanisterMod canister_id
    env <- canisterEnv canister_id

    case inspect_message can_mod method user_id env arg wasm_state of
      Trap msg -> throwError $ ExecutionError (RC_CANISTER_ERROR, "canister trapped in inspect_message: " ++ msg, Nothing)
      Return False -> throwError $ ExecutionError (RC_CANISTER_REJECT, "message not accepted by inspect_message", Nothing)
      Return True -> return ()

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
    subnet_tree (EntityId subnet_id, _, _, _, ranges) = subnet_id =: node (
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

getPrunedCertificate :: (CanReject m, ICM m) => Timestamp -> CanisterId -> [Path] -> m Certificate
getPrunedCertificate time ecid paths = do
    root_subnet <- gets rootSubnet
    sk1 <- gets secretRootKey
    (subnet_id, _, _, sk2, ranges) <- getSubnetFromCanisterId ecid
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

rememberTrap :: ICM m => CallId -> String -> m ()
rememberTrap ctxt_id msg =
  modifyCallContext ctxt_id $ \ctxt -> ctxt { last_trap = Just msg }

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

-- Message handling

processMessage :: ICM m => Message -> m ()
processMessage m = case m of
  CallMessage ctxt_id entry -> onReject (rejectCallContext ctxt_id) $ do
    callee <- calleeOfCallID ctxt_id
    maybeSubnet <- getSubnetFromSubnetId callee
    if callee == managementCanisterId || isJust maybeSubnet then do
      caller <- callerOfCallID ctxt_id
      invokeManagementCanister caller maybeSubnet ctxt_id entry
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
      invokeEntry ctxt_id callee wasm_state can_mod env entry >>= \case
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
      FromUser rid _ -> do
        setReqStatus rid $ CallResponse $
          -- NB: Here cycles disappear
          case response of
            Reject (rc, msg) -> canisterRejected (rc, msg, Nothing)
            Reply blob -> Replied blob
        processSystemTasks
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

performCallActions :: (ICM m, CanReject m) => CallId -> CallActions -> m ()
performCallActions ctxt_id ca = do
  updateBalances ctxt_id (ca_new_calls ca) (ca_accept ca) (ca_mint ca)
  mapM_ (newCall ctxt_id) (ca_new_calls ca)
  mapM_ (respondCallContext ctxt_id) (ca_response ca)


updateBalances :: ICM m => CallId -> [MethodCall] -> Cycles -> Cycles -> m ()
updateBalances ctxt_id new_calls accepted minted = do
  cid <- calleeOfCallID ctxt_id

  -- Eventually update when we track cycle consumption
  let max_cycles = 0
  let cycles_consumed = 0

  prev_balance <- getBalance cid
  available <- getCallContextCycles ctxt_id
  if accepted <= available
  then do
    let to_spend = prev_balance + accepted + minted - max_cycles
    let transferred = sum [ call_transferred_cycles c | c <- new_calls]
    if transferred <= to_spend
    then do
      setBalance cid $ prev_balance
        + accepted
        + minted
        - cycles_consumed
        - transferred
      setCallContextCycles ctxt_id $ available - accepted
    else error "Internal error: More cycles transferred than available"
  else error "Internal error: More cycles accepted than available"


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

 
invokeEntry :: ICM m =>
    CallId -> CanisterId -> WasmState -> CanisterModule -> Env -> EntryPoint ->
    m (TrapOr (WasmState, UpdateResult))
invokeEntry ctxt_id canister_id wasm_state can_mod env entry = do
    needs_to_respond <- needsToRespondCallID ctxt_id
    available <- getCallContextCycles ctxt_id
    case entry of
      Public method dat -> do
        caller <- callerOfCallID ctxt_id
        case lookupUpdate method can_mod of
          Just f ->
            case f caller env needs_to_respond available dat wasm_state of
              Trap err -> return $ Trap err
              Return x -> do
                bumpCanisterVersion canister_id
                return $ Return x
          Nothing -> do
            let reject = Reject (RC_DESTINATION_INVALID, "method does not exist: " ++ method)
            return $ Return (wasm_state, (noCallActions { ca_response = Just reject}, noCanisterActions))
      Closure cb r refund -> do
        case callbacks can_mod cb env needs_to_respond available r refund wasm_state of
            Trap err -> do
              rememberTrap ctxt_id err
              case cleanup_callback cb of
                  Just closure -> case cleanup can_mod closure env wasm_state of
                      Trap err' -> return $ Trap err'
                      Return (wasm_state', ()) -> do
                          bumpCanisterVersion canister_id
                          return $ Return (wasm_state', (noCallActions, noCanisterActions))
                  Nothing -> return $ Trap err
            Return (wasm_state, actions) -> do
                bumpCanisterVersion canister_id
                return $ Return (wasm_state, actions)
      Heartbeat ->
        if exports_heartbeat can_mod then
          case heartbeat can_mod env wasm_state of
            Trap _ -> return $ Return (wasm_state, (noCallActions, noCanisterActions))
            Return (wasm_state, (calls, actions)) -> do
                bumpCanisterVersion canister_id
                return $ Return (wasm_state, (noCallActions { ca_new_calls = calls }, actions))
        else return $ Return (wasm_state, (noCallActions, noCanisterActions))
      GlobalTimer ->
        if exports_global_timer can_mod then do
          case canister_global_timer can_mod env wasm_state of
            Trap _ -> return $ Return (wasm_state, (noCallActions, noCanisterActions))
            Return (wasm_state, (calls, actions)) -> do
                bumpCanisterVersion canister_id
                return $ Return (wasm_state, (noCallActions { ca_new_calls = calls }, actions))
        else return $ Return (wasm_state, (noCallActions, noCanisterActions))
  where
    lookupUpdate method can_mod
        | Just f <- M.lookup method (update_methods can_mod) = Just f
        | Just f <- M.lookup method (query_methods can_mod)  = Just (asUpdate f)
        | otherwise = Nothing

newCall :: (ICM m, CanReject m) => CallId -> MethodCall -> m ()
newCall from_ctxt_id call = do
  caller <- calleeOfCallID from_ctxt_id
  caller_subnet_id <- getSubnetFromCanisterId caller
  let target = call_callee call
  target_subnet_id <- getSubnetFromSubnetId target
  unless (isRootSubnet caller_subnet_id) $ do
    case target_subnet_id of
      Nothing -> return ()
      Just _ -> reject RC_DESTINATION_INVALID "Only NNS canisters can call a subnet ID directly." (Just EC_CANISTER_NOT_FOUND)
  new_ctxt_id <- newCallContext $ CallContext
    { canister = target
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
