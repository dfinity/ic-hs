{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}

{-|
This module implements the main abstract logic of the Internet Computer. It
assumes a pure and abstracted view on Canisters (provided by "IC.Canister"),
and deals with abstract requests ('AsyncRequest', 'SyncRequest'), so HTTP and
CBOR-level processing has already happened.
-}
module IC.Ref
  ( IC(..)
  , IDChoice(..)
  , AsyncRequest(..)
  , callerOfAsync
  , SyncRequest(..)
  , RequestStatus(..)
  , CompletionValue(..)
  , ReqResponse
  , initialIC
  , authSyncRequest
  , authAsyncRequest
  , submitRequest
  , readRequest
  , runStep
  , runToCompletion
  -- $ Exported merely for introspection
  , CallContext(..)
  , Message(..)
  , CanState(..)
  , CallOrigin(..)
  , EntryPoint(..)
  )
where

import qualified Data.Map as M
import qualified Data.Row as R
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Control.Monad.State.Class
import Control.Monad.Except
import Data.Sequence (Seq(..))
import Data.Foldable (toList)
import Codec.Candid hiding (Seq)
import Data.Row (empty, Rec, (.==), (.+), (.!), type (.!))

import IC.Id.Forms hiding (Blob)
import IC.Types
import IC.Canister
import IC.Id.Fresh
import IC.Utils
import IC.Logger
import IC.Management

-- Abstract HTTP Interface

data IDChoice
    = SystemPicks
    | ForcedChoice CanisterId -- ^ only for use from ic-ref-run
    | Desired CanisterId
  deriving (Eq, Ord, Show)

data AsyncRequest
    = CreateRequest UserId IDChoice
    | InstallRequest CanisterId UserId Blob Blob Bool
    | UpgradeRequest CanisterId UserId Blob Blob
    | UpdateRequest CanisterId UserId MethodName Blob
    | SetControllerRequest CanisterId UserId EntityId
  deriving (Eq, Ord, Show)

data SyncRequest
    = QueryRequest CanisterId UserId MethodName Blob
    | StatusRequest Blob

data RequestStatus
  = Unknown -- never used inside the IC, only as ReqResponse
  | Received
  | Processing
  | Rejected (RejectCode, String)
  | Completed CompletionValue
  deriving (Show)

type ReqResponse = RequestStatus

data CompletionValue -- ^ we need to be more typed than the public spec here
  = CompleteUnit
  | CompleteCanisterId CanisterId
  | CompleteArg Blob
  deriving (Show)

-- IC state

-- Non-empty canisters have a module (static) and state (dynamic)
data CanState = CanState
  { wasm_state :: WasmState
  , can_mod :: CanisterModule
  }
  deriving (Show)

-- A canister entry point is either a publicly named function, or a closure
-- (callback + environment)
data EntryPoint
  = Public MethodName Blob
  | Closure Callback Response
  deriving (Show)

type CallId = Int
data CallContext = CallContext
  { canister :: CanisterId
  , origin :: CallOrigin
  , responded :: Responded
  , last_trap :: Maybe String
      -- ^ non-normative, but yields better reject messages
  }
  deriving (Show)

data CallOrigin
  = FromUser RequestID
  | FromCanister CallId Callback
  deriving (Show)

data Message
  = CallMessage
    { call_context :: CallId
    , entry :: EntryPoint
    }
  | ResponseMessage
    { call_context :: CallId
    , response :: Response
    }
  deriving (Show)

-- Finally, the full IC state:

data IC = IC
  { canisters :: CanisterId ↦ Maybe CanState
  , controllers :: CanisterId ↦ EntityId
  , requests :: RequestID ↦ (AsyncRequest, RequestStatus)
  , messages :: Seq Message
  , call_contexts :: CallId ↦ CallContext
  }
  deriving (Show)

-- The functions below want stateful access to a value of type 'IC', and be
-- able to log message (used for `ic0.debug_print`).
type ICM m = (MonadState IC m, Logger m)

initialIC :: IC
initialIC = IC mempty mempty mempty mempty mempty

-- Request handling

findRequest :: RequestID -> IC -> Maybe (AsyncRequest, RequestStatus)
findRequest rid ic = M.lookup rid (requests ic)

setReqStatus :: ICM m => RequestID -> RequestStatus -> m ()
setReqStatus rid s = modify $ \ic ->
  ic { requests = M.adjust (\(r,_) -> (r,s)) rid (requests ic) }

callerOfAsync :: AsyncRequest -> EntityId
callerOfAsync = \case
    CreateRequest user_id _ -> user_id
    InstallRequest _ user_id _ _ _ -> user_id
    UpgradeRequest _ user_id _ _ -> user_id
    UpdateRequest _ user_id _ _ -> user_id
    SetControllerRequest _ user_id _ -> user_id

callerOfRequest :: ICM m => RequestID -> m EntityId
callerOfRequest rid = gets (M.lookup rid . requests) >>= \case
    Just (ar,_) -> return (callerOfAsync ar)
    Nothing -> error "callerOfRequest"


-- Canister handling

createEmptyCanister :: ICM m => CanisterId -> EntityId -> m ()
createEmptyCanister cid controller = modify $ \ic -> ic
  { canisters = M.insert cid Nothing (canisters ic)
  , controllers = M.insert cid controller (controllers ic)
  }

insertCanister :: ICM m => CanisterId -> CanisterModule -> WasmState -> m ()
insertCanister cid can_mod wasm_state = modify $ \ic ->
  ic { canisters = M.insert cid (Just (CanState {can_mod, wasm_state})) (canisters ic) }

setCanisterState :: ICM m => CanisterId -> WasmState -> m ()
setCanisterState cid wasm_state = modify $ \ic ->
  ic { canisters = M.adjust (fmap (\cs -> cs { wasm_state })) cid (canisters ic) }

getController :: (CanReject m, ICM m) => CanisterId -> m EntityId
getController cid =
  gets (M.lookup cid . controllers)
    `orElse` reject RC_DESTINATION_INVALID ("canister does not exist: " ++ prettyID cid)

setController :: (CanReject m, ICM m) => CanisterId -> EntityId -> m ()
setController cid controller = modify $ \ic ->
  ic { controllers = M.insert cid controller (controllers ic) }

getCanisterState :: (CanReject m, ICM m) => CanisterId -> m (Maybe CanState)
getCanisterState cid =
  gets (M.lookup cid . canisters)
    `orElse` reject RC_DESTINATION_INVALID ("canister does not exist: " ++ prettyID cid)

getNonemptyCanisterState :: (CanReject m, ICM m) => CanisterId -> m CanState
getNonemptyCanisterState cid =
  getCanisterState cid
    `orElse` reject RC_DESTINATION_INVALID ("canister is empty:" ++ prettyID cid)

-- Authentication of requests

-- This is monadic, as authentication may depend on the state of the system
-- for request status: Whether the request exists and who owns it
-- in general: eventually there will be user key management

authUser :: ICM m => PublicKey -> EntityId -> m Bool
authUser pk id = return $ -- This is monadic to allow for key management
    isSelfAuthenticatingId pk (rawEntityId id)

authAsyncRequest :: ICM m => PublicKey -> AsyncRequest -> m Bool
authAsyncRequest pk ar = authUser pk (callerOfAsync ar)

authSyncRequest :: ICM m => PublicKey -> SyncRequest -> m Bool
authSyncRequest pk = \case
  StatusRequest rid ->
    gets (findRequest rid) >>= \case
      Just (ar,_) -> authUser pk (callerOfAsync ar)
      Nothing -> return False
  QueryRequest _ user_id _ _ ->
    authUser pk user_id

-- Synchronous requests

readRequest :: ICM m => SyncRequest -> m ReqResponse
readRequest req = onReject (return . Rejected) $ case req of

  StatusRequest rid ->
    gets (findRequest rid) >>= \case
      Just (_r,status) -> return status
      Nothing -> return Unknown

  QueryRequest canister_id user_id method arg -> do
    CanState wasm_state can_mod <- getNonemptyCanisterState canister_id
    f <- return (M.lookup method (query_methods can_mod))
      `orElse` reject RC_DESTINATION_INVALID "query method does not exist"

    case f user_id arg wasm_state of
      Trap msg -> reject RC_CANISTER_ERROR $ "canister trapped: " ++ msg
      Return (Reject (rc,rm)) -> reject rc rm
      Return (Reply res) -> return $ Completed (CompleteArg res)

-- Asynchronous requests

-- | Submission simply enqueues requests

submitRequest :: ICM m => RequestID -> AsyncRequest -> m ()
submitRequest rid r = modify $ \ic ->
  if M.member rid (requests ic)
  then ic
  else ic { requests = M.insert rid (r, Received) (requests ic) }


-- | Eventually, they are processed

processRequest :: ICM m => RequestID -> AsyncRequest -> m ()

processRequest rid req = (setReqStatus rid =<<) $ onReject (return . Rejected) $ case req of
  CreateRequest user_id id_choice -> do
    new_id <- case id_choice of
      SystemPicks -> gets (freshId . M.keys . canisters)
      ForcedChoice id -> return id
      Desired id -> do
        unless (isDerivedId (rawEntityId user_id) (rawEntityId id)) $
          reject RC_DESTINATION_INVALID "Desired canister id not derived from sender id"
        return id
    exists <- gets (M.member new_id . canisters)
    when exists $
      reject RC_DESTINATION_INVALID "Desired canister id already exists"
    createEmptyCanister new_id user_id
    return $ Completed (CompleteCanisterId new_id)

  InstallRequest canister_id user_id can_mod_data dat reinstall -> do
    can_mod <- return (parseCanister can_mod_data)
      `onErr` (\err -> reject RC_SYS_FATAL $ "Parsing failed: " ++ err)
    checkController canister_id user_id
    was_empty <- isNothing <$> getCanisterState canister_id
    when (not reinstall && not was_empty) $
      reject RC_DESTINATION_INVALID "canister is not empty during installation"
    when (reinstall && was_empty) $
      reject RC_DESTINATION_INVALID "canister is empty during reinstallation"
    wasm_state <- return (init_method can_mod canister_id user_id dat)
      `onTrap` (\msg -> reject RC_CANISTER_ERROR $ "Initialization trapped: " ++ msg)
    insertCanister canister_id can_mod wasm_state
    return $ Completed CompleteUnit

  UpgradeRequest canister_id user_id new_can_mod_data dat -> do
    new_can_mod <- return (parseCanister new_can_mod_data)
      `onErr` (\err -> reject RC_SYS_FATAL $ "Parsing failed: " ++ err)
    checkController canister_id user_id
    CanState old_wasm_state old_can_mod <- getNonemptyCanisterState canister_id
    mem <- return (pre_upgrade_method old_can_mod old_wasm_state user_id)
      `onTrap` (\msg -> reject RC_CANISTER_ERROR $ "Pre-upgrade trapped: " ++ msg)
    new_wasm_state <- return (post_upgrade_method new_can_mod user_id user_id mem dat)
      `onTrap` (\msg -> reject RC_CANISTER_ERROR $ "Post-upgrade trapped: " ++ msg)
    insertCanister canister_id new_can_mod new_wasm_state
    return $ Completed CompleteUnit

  UpdateRequest canister_id _user_id method arg -> do
    ctxt_id <- newCallContext $ CallContext
      { canister = canister_id
      , origin = FromUser rid
      , responded = Responded False
      , last_trap = Nothing
      }
    enqueueMessage $ CallMessage
      { call_context = ctxt_id
      , entry = Public method arg
      }
    return Processing

  SetControllerRequest canister_id user_id new_controller -> do
    checkController canister_id user_id
    setController canister_id new_controller
    return $ Completed CompleteUnit


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

respondCallContext :: ICM m => CallId -> Response -> m ()
respondCallContext ctxt_id response = do
  -- TODO: check no prior response
  modifyCallContext ctxt_id $ \ctxt -> ctxt { responded = Responded True }
  enqueueMessage $ ResponseMessage { call_context = ctxt_id, response }

replyCallContext :: ICM m => CallId -> Blob -> m ()
replyCallContext ctxt_id =
  respondCallContext ctxt_id . Reply

rejectCallContext :: ICM m => CallId -> (RejectCode, String) -> m ()
rejectCallContext ctxt_id =
  respondCallContext ctxt_id . Reject

rememberTrap :: ICM m => CallId -> String -> m ()
rememberTrap ctxt_id msg =
  modifyCallContext ctxt_id $ \ctxt -> ctxt { last_trap = Just msg }

callerOfCallID :: ICM m => CallId -> m EntityId
callerOfCallID ctxt_id = do
  ctxt <- getCallContext ctxt_id
  case origin ctxt of
    FromUser rid -> callerOfRequest rid
    FromCanister other_ctxt_id _callback -> calleeOfCallID other_ctxt_id

calleeOfCallID :: ICM m => CallId -> m EntityId
calleeOfCallID ctxt_id = canister <$> getCallContext ctxt_id

respondedCallID :: ICM m => CallId -> m Responded
respondedCallID ctxt_id = responded <$> getCallContext ctxt_id

starveCallContext :: ICM m => CallId -> m ()
starveCallContext ctxt_id = do
  ctxt <- getCallContext ctxt_id
  let msg | Just t <- last_trap ctxt = "canister trapped: " ++ t
          | otherwise                = "canister did not respond"
  rejectCallContext ctxt_id (RC_CANISTER_ERROR, msg)

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
      invokeManagementCanister caller entry >>= replyCallContext ctxt_id
    else do
      cs <- getNonemptyCanisterState callee
      invokeEntry ctxt_id cs entry >>= \case
        Trap msg -> do
          logTrap msg
          rememberTrap ctxt_id msg
        Return (new_state, (new_calls, mb_response)) -> do
          setCanisterState callee new_state
          mapM_ (newCall ctxt_id) new_calls
          mapM_ (respondCallContext ctxt_id) mb_response

  ResponseMessage ctxt_id response -> do
    ctxt <- getCallContext ctxt_id
    case origin ctxt of
      FromUser rid -> setReqStatus rid $
        case response of
          Reject (rc, msg) -> Rejected (rc, msg)
          Reply blob -> Completed (CompleteArg blob)
      FromCanister other_ctxt_id callback ->
        enqueueMessage $ CallMessage
          { call_context = other_ctxt_id
          , entry = Closure callback response
          }

managementCanisterId :: EntityId
managementCanisterId = EntityId mempty

invokeManagementCanister :: (CanReject m, ICM m) => EntityId -> EntryPoint -> m Blob
invokeManagementCanister caller = \case
    Public method_name arg ->
        BS.fromStrict <$> raw_service (T.pack method_name) (BS.toStrict arg)
    Closure{} -> error "closure invoked on management function "
  where
    raw_service = fromCandidService not_found err (managementCanister caller)
    not_found f =  reject RC_DESTINATION_INVALID $ "Unsupported management function " ++ T.unpack f
    err msg = reject RC_CANISTER_ERROR $ "Candid failed to decode: " ++ msg

managementCanister :: (CanReject m, ICM m) => EntityId -> Rec (ICManagement m)
managementCanister caller = empty
    .+ #create_canister .== rejectAsCanister . icCreateCanister caller
    .+ #install_code .== rejectAsCanister . icInstallCode caller
    .+ #set_controller .== rejectAsCanister . icSetController caller

icCreateCanister :: (ICM m, CanReject m) => EntityId -> ICManagement m .! "create_canister"
icCreateCanister caller r = do
    new_id <- case r .! #desired_id of
      Nothing -> gets (freshId . M.keys . canisters)
      Just id' -> do
        let id = principalToEntityId id'
        unless (isDerivedId (rawEntityId caller) (rawEntityId id)) $
          reject RC_DESTINATION_INVALID "Desired canister id not derived from sender id"
        return id
    exists <- gets (M.member new_id . canisters)
    when exists $
      reject RC_DESTINATION_INVALID "Desired canister id already exists"
    createEmptyCanister new_id caller
    return (#canister_id .== entityIdToPrincipal new_id)

icInstallCode :: (ICM m, CanReject m) => EntityId -> ICManagement m .! "install_code"
icInstallCode caller r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    let arg = BS.fromStrict (r .! #arg)
    new_can_mod <- return (parseCanister (BS.fromStrict (r .! #wasm_module)))
      `onErr` (\err -> reject RC_SYS_FATAL $ "Parsing failed: " ++ err)
    checkController canister_id caller
    was_empty <- isNothing <$> getCanisterState canister_id
    R.switch (r .! #mode) $ R.empty
      .+ #install .== (\() -> do
        unless was_empty $
          reject RC_DESTINATION_INVALID "canister is not empty during installation"
        wasm_state <- return (init_method new_can_mod canister_id caller arg)
          `onTrap` (\msg -> reject RC_CANISTER_ERROR $ "Initialization trapped: " ++ msg)
        insertCanister canister_id new_can_mod wasm_state
      ) .+ #reinstall .== (\() -> do
        when was_empty $
          reject RC_DESTINATION_INVALID "canister is empty during reinstallation"
        wasm_state <- return (init_method new_can_mod canister_id caller arg)
          `onTrap` (\msg -> reject RC_CANISTER_ERROR $ "Initialization trapped: " ++ msg)
        insertCanister canister_id new_can_mod wasm_state
      ) .+ #upgrade .== (\() -> do
        CanState old_wasm_state old_can_mod <- getNonemptyCanisterState canister_id
        mem <- return (pre_upgrade_method old_can_mod old_wasm_state caller)
          `onTrap` (\msg -> reject RC_CANISTER_ERROR $ "Pre-upgrade trapped: " ++ msg)
        new_wasm_state <- return (post_upgrade_method new_can_mod canister_id caller mem arg)
          `onTrap` (\msg -> reject RC_CANISTER_ERROR $ "Post-upgrade trapped: " ++ msg)
        insertCanister canister_id new_can_mod new_wasm_state
      )

icSetController :: (ICM m, CanReject m) => EntityId -> ICManagement m .! "set_controller"
icSetController caller r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    let new_controller = principalToEntityId (r .! #new_controller)
    checkController canister_id caller
    setController canister_id new_controller

checkController :: (ICM m, CanReject m) => CanisterId -> EntityId -> m ()
checkController canister_id user_id = do
    controller <- getController canister_id
    unless (controller == user_id) $
      reject RC_SYS_FATAL $
        prettyID user_id <> " is not authorized to manage canister " <>
        prettyID canister_id <> ", only " <> prettyID controller <> " is"

invokeEntry :: ICM m =>
    CallId -> CanState -> EntryPoint ->
    m (TrapOr (WasmState, UpdateResult))
invokeEntry ctxt_id (CanState wasm_state can_mod) entry = do
    responded <- respondedCallID ctxt_id
    case entry of
      Public method dat -> do
        caller <- callerOfCallID ctxt_id
        case M.lookup method (update_methods can_mod) of
          Just f -> return $ f caller responded dat wasm_state
          Nothing ->
            case M.lookup method (query_methods can_mod) of
              Just f -> return $ asUpdate f caller responded dat wasm_state
              Nothing -> do
                let reject = Reject (RC_DESTINATION_INVALID, "method does not exist: " ++ method)
                return $ Return (wasm_state, ([], Just reject))
      Closure cb r ->
        return $ callbacks can_mod cb responded r wasm_state

newCall :: ICM m => CallId -> MethodCall -> m ()
newCall from_ctxt_id call = do
  new_ctxt_id <- newCallContext $ CallContext
    { canister = call_callee call
    , origin = FromCanister from_ctxt_id (call_callback call)
    , responded = Responded False
    , last_trap = Nothing
    }
  enqueueMessage $ CallMessage
    { call_context = new_ctxt_id
    , entry = Public (call_method_name call) (call_arg call)
    }

-- Scheduling

-- | Pick next request in state `received`
nextReceived :: ICM m => m (Maybe (RequestID, AsyncRequest))
nextReceived = gets $ \ic -> listToMaybe
  [ (rid,r) | (rid, (r, Received)) <- M.toList (requests ic) ]

-- | Find a starved call context
nextStarved :: ICM m => m (Maybe CallId)
nextStarved = gets $ \ic -> listToMaybe
  [ c
  | (c, ctxt) <- M.toList (call_contexts ic)
  , Responded False <- return $ responded ctxt
  , null [ ()
      | ResponseMessage { call_context = c' } <- toList (messages ic)
      , c' == c ]
  , null [ ()
      | CallContext { responded = Responded False, origin = FromCanister c' _}
          <- M.elems (call_contexts ic)
      , c' == c
      ]
  ]

-- | Pick (and remove) next message from queue
popMessage :: ICM m => m (Maybe Message)
popMessage = state $ \ic ->
  case messages ic of
    Empty -> (Nothing, ic)
    m :<| ms -> (Just m, ic { messages = ms })


-- | Returns true if a step was taken
runStep :: ICM m => m Bool
runStep =
  nextReceived >>= \case
    Just (rid,r) -> processRequest rid r >> return True
    Nothing -> popMessage >>= \case
      Just m  -> processMessage m >> return True
      Nothing -> nextStarved >>= \case
        Just c  -> starveCallContext c >> return True
        Nothing -> return False

runToCompletion :: ICM m => m ()
runToCompletion = repeatWhileTrue runStep

-- Error handling plumbing

type CanReject = MonadError (RejectCode, String)
reject :: CanReject m => RejectCode -> String -> m a2
reject code msg = throwError (code, msg)

-- To maintain the abstraction that the management canister is a canister,
-- all its errors are turned into canister errors
rejectAsCanister :: CanReject m => m a -> m a
rejectAsCanister act = catchError act (\(_c,msg) -> reject RC_CANISTER_ERROR msg)

onReject :: ICM m =>
  ((RejectCode, String) -> m b) ->
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


