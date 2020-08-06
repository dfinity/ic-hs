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
  -- $ Exported merely for debug introspection
  , CallContext(..)
  , Message(..)
  , CanState(..)
  , CallOrigin(..)
  , EntryPoint(..)
  , RunStatus(..)
  )
where

import qualified Data.Map as M
import qualified Data.Row as R
import qualified Data.Row.Variants as V
import qualified Data.Text as T
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad.State.Class
import Control.Monad.Except
import Data.Sequence (Seq(..))
import Data.Foldable (toList)
import Codec.Candid
import Data.Row (empty, Rec, (.==), (.+), (.!), type (.!))
import Numeric.Natural

import IC.Id.Forms hiding (Blob)
import IC.Types
import IC.Canister
import IC.Id.Fresh
import IC.Utils
import IC.Logger
import IC.Management

-- Abstract HTTP Interface

data AsyncRequest
    = UpdateRequest Expiry CanisterId UserId MethodName Blob
  deriving (Eq, Ord, Show)

data SyncRequest
    = QueryRequest Expiry CanisterId UserId MethodName Blob
    | StatusRequest Expiry Blob

type Expiry = Natural

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

-- The canister state

data RunStatus
  = IsRunning
  | IsStopping [CallId]
  | IsStopped
  | IsDeleted -- not actually a run state, but convenient in this code
  deriving (Show)

data CanState = CanState
  { wasm_state :: Maybe WasmState -- absent when empty
  , can_mod :: Maybe CanisterModule -- absent when empty
  , run_status :: RunStatus
  , controller :: EntityId
  , time :: Timestamp
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
  { canisters :: CanisterId ↦ CanState
  , requests :: RequestID ↦ (AsyncRequest, RequestStatus)
  , messages :: Seq Message
  , call_contexts :: CallId ↦ CallContext
  }
  deriving (Show)

-- The functions below want stateful access to a value of type 'IC', and be
-- able to log message (used for `ic0.debug_print`).
type ICM m = (MonadState IC m, Logger m)

initialIC :: IC
initialIC = IC mempty mempty mempty mempty

-- Request handling

findRequest :: RequestID -> IC -> Maybe (AsyncRequest, RequestStatus)
findRequest rid ic = M.lookup rid (requests ic)

setReqStatus :: ICM m => RequestID -> RequestStatus -> m ()
setReqStatus rid s = modify $ \ic ->
  ic { requests = M.adjust (\(r,_) -> (r,s)) rid (requests ic) }

_expiryOfAsync :: AsyncRequest -> Expiry
_expiryOfAsync = \case
    UpdateRequest expiry _ _ _ _ -> expiry

_expiryOfSync :: SyncRequest -> Expiry
_expiryOfSync = \case
    QueryRequest expiry _ _ _ _ -> expiry
    StatusRequest expiry _ -> expiry

callerOfAsync :: AsyncRequest -> EntityId
callerOfAsync = \case
    UpdateRequest _ _ user_id _ _ -> user_id

callerOfRequest :: ICM m => RequestID -> m EntityId
callerOfRequest rid = gets (M.lookup rid . requests) >>= \case
    Just (ar,_) -> return (callerOfAsync ar)
    Nothing -> error "callerOfRequest"


-- Canister handling

createEmptyCanister :: ICM m => CanisterId -> EntityId -> Timestamp -> m ()
createEmptyCanister cid controller time = modify $ \ic ->
    ic { canisters = M.insert cid can (canisters ic) }
  where
    can = CanState
      { wasm_state = Nothing
      , can_mod = Nothing
      , run_status = IsRunning
      , controller = controller
      , time = time
      }

canisterMustExist :: (CanReject m, ICM m) => CanisterId -> m ()
canisterMustExist cid =
  gets (M.lookup cid . canisters) >>= \case
    Nothing ->
      reject RC_DESTINATION_INVALID ("canister does not exist: " ++ prettyID cid)
    Just CanState{ run_status = IsDeleted } ->
      reject RC_DESTINATION_INVALID ("canister no longer exists: " ++ prettyID cid)
    _ -> return ()

isCanisterEmpty :: (CanReject m, ICM m) => CanisterId -> m Bool
isCanisterEmpty cid = isNothing . wasm_state <$> getCanister cid


-- the following functions assume the canister does exist;
-- it would be an internal error if they dont

getCanister :: ICM m => CanisterId -> m CanState
getCanister cid =
  gets (M.lookup cid . canisters)
    `orElse` error "canister does not exist"

modCanister :: ICM m => CanisterId -> (CanState -> CanState) -> m ()
modCanister cid f = do
    void $ getCanister cid
    modify $ \ic -> ic { canisters = M.adjust f cid (canisters ic) }

setCanisterModule :: ICM m => CanisterId -> CanisterModule -> m ()
setCanisterModule cid can_mod = modCanister cid $
    \cs -> cs { can_mod = Just can_mod }

setCanisterState :: ICM m => CanisterId -> WasmState -> m ()
setCanisterState cid wasm_state = modCanister cid $
    \cs -> cs { wasm_state = Just wasm_state }

getController :: ICM m => CanisterId -> m EntityId
getController cid = controller <$> getCanister cid

setController :: ICM m => CanisterId -> EntityId -> m ()
setController cid controller = modCanister cid $
    \cs -> cs { controller = controller }

getRunStatus :: ICM m => CanisterId -> m RunStatus
getRunStatus cid = run_status <$> getCanister cid

setRunStatus :: ICM m => CanisterId -> RunStatus -> m ()
setRunStatus cid run_status = modCanister cid $
    \cs -> cs { run_status = run_status }

getCanisterState :: ICM m => CanisterId -> m WasmState
getCanisterState cid = fromJust . wasm_state <$> getCanister cid

getCanisterMod :: ICM m => CanisterId -> m CanisterModule
getCanisterMod cid = fromJust . can_mod <$> getCanister cid

getCanisterTime :: ICM m => CanisterId -> m Timestamp
getCanisterTime cid = time <$> getCanister cid

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
  StatusRequest _ rid ->
    gets (findRequest rid) >>= \case
      Just (ar,_) -> authUser pk (callerOfAsync ar)
      Nothing -> return False
  QueryRequest _ _ user_id _ _ ->
    authUser pk user_id

-- Synchronous requests

readRequest :: ICM m => SyncRequest -> m ReqResponse
readRequest req = onReject (return . Rejected) $ case req of

  StatusRequest _ rid ->
    gets (findRequest rid) >>= \case
      Just (_r,status) -> return status
      Nothing -> return Unknown

  QueryRequest _ canister_id user_id method arg -> do
    canisterMustExist canister_id
    getRunStatus canister_id >>= \case
       IsRunning -> return ()
       _ -> reject RC_CANISTER_ERROR "canister is stopped"
    empty <- isCanisterEmpty canister_id
    when empty $ reject RC_DESTINATION_INVALID "canister is empty"
    wasm_state <- getCanisterState canister_id
    can_mod <- getCanisterMod canister_id
    time <- getCanisterTime canister_id

    f <- return (M.lookup method (query_methods can_mod))
      `orElse` reject RC_DESTINATION_INVALID "query method does not exist"


    case f user_id time arg wasm_state of
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

processRequest :: ICM m => (RequestID, AsyncRequest) -> m ()
processRequest (rid, req) = onReject (setReqStatus rid . Rejected) $ case req of
  UpdateRequest _expiry canister_id _user_id method arg -> do
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
      rejectAsCanister $ do
        -- using MaybeT to signal that no resonse is to be sent yet
        r <- runMaybeT $ invokeManagementCanister caller ctxt_id entry
        forM_ r $ replyCallContext ctxt_id
    else do
      canisterMustExist callee
      getRunStatus callee >>= \case
          IsRunning -> return ()
          _ -> reject RC_CANISTER_ERROR "canister is stopped"
      empty <- isCanisterEmpty callee
      when empty $ reject RC_DESTINATION_INVALID "canister is empty"
      wasm_state <- getCanisterState callee
      can_mod <- getCanisterMod callee
      time <- getCanisterTime callee
      invokeEntry ctxt_id wasm_state can_mod time entry >>= \case
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

invokeManagementCanister :: (CanReject m, ICM m) => EntityId -> CallId -> EntryPoint -> MaybeT m Blob
invokeManagementCanister caller cid = \case
    Public method_name arg -> raw_service (T.pack method_name) arg
    Closure{} -> error "closure invoked on management function "
  where
    raw_service = fromCandidService not_found err (managementCanister caller cid)
    not_found f = reject RC_DESTINATION_INVALID $ "Unsupported management function " ++ T.unpack f
    err msg = reject RC_CANISTER_ERROR $ "Candid failed to decode: " ++ msg

managementCanister :: (CanReject m, ICM m) => EntityId -> CallId -> Rec (ICManagement (MaybeT m))
managementCanister caller ctxt_id = empty
    .+ #create_canister .== icCreateCanister caller
    .+ #install_code    .== icInstallCode caller
    .+ #set_controller  .== icSetController caller
    .+ #start_canister  .== icStartCanister caller
    .+ #stop_canister   .== icStopCanister caller ctxt_id
    .+ #canister_status .== icCanisterStatus caller
    .+ #delete_canister .== icDeleteCanister caller

icCreateCanister :: (ICM m, CanReject m) => EntityId -> ICManagement m .! "create_canister"
icCreateCanister caller _r = do
    new_id <- gets (freshId . M.keys . canisters)
    let currentTime = 0 -- ic-ref lives in the 70ies
    createEmptyCanister new_id caller currentTime
    return (#canister_id .== entityIdToPrincipal new_id)

icInstallCode :: (ICM m, CanReject m) => EntityId -> ICManagement m .! "install_code"
icInstallCode caller r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    let arg = r .! #arg
    new_can_mod <- return (parseCanister (r .! #wasm_module))
      `onErr` (\err -> reject RC_SYS_FATAL $ "Parsing failed: " ++ err)
    canisterMustExist canister_id
    checkController canister_id caller
    was_empty <- isCanisterEmpty canister_id
    time <- getCanisterTime canister_id

    let
      reinstall = do
        wasm_state <- return (init_method new_can_mod canister_id caller time arg)
          `onTrap` (\msg -> reject RC_CANISTER_ERROR $ "Initialization trapped: " ++ msg)
        setCanisterModule canister_id new_can_mod
        setCanisterState canister_id wasm_state

      install = do
        unless was_empty $
          reject RC_DESTINATION_INVALID "canister is not empty during installation"
        reinstall

      upgrade = do
        when was_empty $
          reject RC_DESTINATION_INVALID "canister is empty during upgrade"
        old_wasm_state <- getCanisterState canister_id
        old_can_mod <- getCanisterMod canister_id
        mem <- return (pre_upgrade_method old_can_mod old_wasm_state caller time)
          `onTrap` (\msg -> reject RC_CANISTER_ERROR $ "Pre-upgrade trapped: " ++ msg)
        new_wasm_state <- return (post_upgrade_method new_can_mod canister_id caller time mem arg)
          `onTrap` (\msg -> reject RC_CANISTER_ERROR $ "Post-upgrade trapped: " ++ msg)
        setCanisterModule canister_id new_can_mod
        setCanisterState canister_id new_wasm_state

    R.switch (r .! #mode) $ R.empty
      .+ #install .== (\() -> install)
      .+ #reinstall .== (\() -> reinstall)
      .+ #upgrade .== (\() -> upgrade)

icSetController :: (ICM m, CanReject m) => EntityId -> ICManagement m .! "set_controller"
icSetController caller r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    let new_controller = principalToEntityId (r .! #new_controller)
    canisterMustExist canister_id
    checkController canister_id caller
    setController canister_id new_controller

checkController :: (ICM m, CanReject m) => CanisterId -> EntityId -> m ()
checkController canister_id user_id = do
    controller <- getController canister_id
    unless (controller == user_id) $
      reject RC_SYS_FATAL $
        prettyID user_id <> " is not authorized to manage canister " <>
        prettyID canister_id <> ", only " <> prettyID controller <> " is"

icStartCanister :: (ICM m, CanReject m) => EntityId -> ICManagement m .! "start_canister"
icStartCanister caller r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    canisterMustExist canister_id
    checkController canister_id caller
    getRunStatus canister_id >>= \case
        IsRunning -> return ()
        IsStopping pending -> forM_ pending $ \ctxt_id ->
            rejectCallContext ctxt_id (RC_CANISTER_ERROR, "Canister has been restarted")
        IsStopped -> setRunStatus canister_id IsRunning
        IsDeleted -> error "deleted canister encountered"

icStopCanister :: (ICM m, CanReject m) => EntityId -> CallId -> ICManagement (MaybeT m) .! "stop_canister"
icStopCanister caller ctxt_id r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    canisterMustExist canister_id
    checkController canister_id caller
    getRunStatus canister_id >>= \case
        IsRunning -> do
            setRunStatus canister_id (IsStopping [ctxt_id])
            MaybeT (return Nothing)
        IsStopping pending -> do
            setRunStatus canister_id (IsStopping (pending ++ [ctxt_id]))
            MaybeT (return Nothing)
        IsStopped -> return ()
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

icCanisterStatus :: (ICM m, CanReject m) => EntityId -> ICManagement m .! "canister_status"
icCanisterStatus caller r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    canisterMustExist canister_id
    checkController canister_id caller
    s <- getRunStatus canister_id >>= \case
        IsRunning -> return (V.IsJust #running ())
        IsStopping _pending -> return (V.IsJust #stopping ())
        IsStopped -> return (V.IsJust #stopped ())
        IsDeleted -> error "deleted canister encountered"
    return $ #status .== s

icDeleteCanister :: (ICM m, CanReject m) => EntityId -> ICManagement m .! "delete_canister"
icDeleteCanister caller r = do
    let canister_id = principalToEntityId (r .! #canister_id)
    canisterMustExist canister_id
    checkController canister_id caller
    getRunStatus canister_id >>= \case
        IsRunning -> reject RC_SYS_FATAL "Cannot delete running canister"
        IsStopping _pending -> reject RC_SYS_FATAL "Cannot delete stopping canister"
        IsStopped -> return ()
        IsDeleted -> error "deleted canister encountered"
    setRunStatus canister_id IsDeleted

invokeEntry :: ICM m =>
    CallId -> WasmState -> CanisterModule -> Timestamp -> EntryPoint ->
    m (TrapOr (WasmState, UpdateResult))
invokeEntry ctxt_id wasm_state can_mod time entry = do
    responded <- respondedCallID ctxt_id
    case entry of
      Public method dat -> do
        caller <- callerOfCallID ctxt_id
        case M.lookup method (update_methods can_mod) of
          Just f -> return $ f caller time responded dat wasm_state
          Nothing ->
            case M.lookup method (query_methods can_mod) of
              Just f -> return $ asUpdate f caller time responded dat wasm_state
              Nothing -> do
                let reject = Reject (RC_DESTINATION_INVALID, "method does not exist: " ++ method)
                return $ Return (wasm_state, ([], Just reject))
      Closure cb r ->
        return $ callbacks can_mod cb time responded r wasm_state

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

-- A call context is still waiting for a response if…
willReceiveResponse :: IC -> CallId -> Bool
willReceiveResponse ic c = c `elem`
  -- there is another call context promising to respond to this
  [ c'
  | CallContext { responded = Responded False, origin = FromCanister c' _}
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
  | (c, CallContext { responded = Responded False } ) <- M.toList (call_contexts ic)
  , not $ willReceiveResponse ic c
  ]

-- | Find a canister in stopping state that is, well, stopped
nextStoppedCanister :: ICM m => m (Maybe CanisterId)
nextStoppedCanister = gets $ \ic -> listToMaybe
  [ cid
  | (cid, CanState { run_status = IsStopping _ }) <- M.toList (canisters ic)
  -- no open call context
  , null [ ()
    | (c, ctxt) <- M.toList (call_contexts ic)
    , canister ctxt == cid
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


