{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-|
This module implements the main abstract logic of the Internet Computer. It
assumes a pure and abstracted view on Canisters (provided by "IC.Canister"),
and deals with abstract requests ('AsyncRequest', 'SyncRequest'), so HTTP and
CBOR-level processing has already happened.
-}
module IC.Ref
  ( IC(..)
  , AsyncRequest(..)
  , SyncRequest(..)
  , RequestStatus(..)
  , CompletionValue(..)
  , ReqResponse
  , initialIC
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
import Data.Maybe
import Control.Monad.State.Class
import Data.Sequence (Seq(..))
import Data.Foldable (toList)

import IC.Types
import IC.Canister
import IC.Id.Fresh
import IC.Utils
import IC.Logger

-- Abstract HTTP Interface

data AsyncRequest
    = CreateRequest UserId (Maybe CanisterId)
    | InstallRequest CanisterId UserId Blob Blob Bool
    | UpgradeRequest CanisterId UserId Blob Blob
    | UpdateRequest CanisterId UserId MethodName Blob
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

-- Canisters have a module (static) and state (dynamic)
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
  | FromInit EntityId
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

callerOfRequest :: ICM m => RequestID -> m EntityId
callerOfRequest rid = gets (M.lookup rid . requests) >>= \case
    Just (CreateRequest user_id _, _) -> return user_id
    Just (InstallRequest _ user_id _ _ _, _) -> return user_id
    Just (UpgradeRequest _ user_id _ _, _) -> return user_id
    Just (UpdateRequest _ user_id _ _, _) -> return user_id
    Nothing -> error "callerOfRequest"


-- Canister handling

createEmptyCanister :: ICM m => CanisterId -> m ()
createEmptyCanister cid = modify $ \ic ->
  ic { canisters = M.insert cid Nothing (canisters ic) }

insertCanister :: ICM m => CanisterId -> CanisterModule -> WasmState -> m ()
insertCanister cid can_mod wasm_state = modify $ \ic ->
  ic { canisters = M.insert cid (Just (CanState {can_mod, wasm_state})) (canisters ic) }

setCanisterState :: ICM m => CanisterId -> WasmState -> m ()
setCanisterState cid wasm_state = modify $ \ic ->
  ic { canisters = M.adjust (fmap (\cs -> cs { wasm_state })) cid (canisters ic) }

-- Synchronous requests

readRequest :: ICM m => SyncRequest -> m ReqResponse

readRequest (StatusRequest rid) =
  gets (findRequest rid) >>= \case
    Just (_r,status) -> return status
    Nothing -> return Unknown

readRequest (QueryRequest canister_id user_id method arg) =
  gets (M.lookup canister_id . canisters) >>= \case
    Nothing -> return $ Rejected (RC_DESTINATION_INVALID, "canister does not exist")
    Just Nothing -> return $ Rejected (RC_DESTINATION_INVALID, "canister is empty")
    Just (Just (CanState wasm_state can_mod)) ->
      case M.lookup method (query_methods can_mod) of
        Nothing -> return $ Rejected (RC_DESTINATION_INVALID, "query method does not exist")
        Just f ->
          case f user_id arg wasm_state of
            Trap msg -> return $ Rejected (RC_CANISTER_ERROR, "canister trapped: " ++ msg)
            Return (Reply res) -> return $ Completed (CompleteArg res)
            Return (Reject (rc,rm)) -> return $ Rejected (rc, rm)

-- Asynchronous requests

-- | Submission simply enqueues requests

submitRequest :: ICM m => RequestID -> AsyncRequest -> m ()
submitRequest rid r = modify $ \ic ->
  ic { requests = M.insert rid (r, Received) (requests ic) }


-- | Eventually, they are processed

processRequest :: ICM m => RequestID -> AsyncRequest -> m ()

processRequest rid (CreateRequest _user_id (Just desired)) = do
  let res = setReqStatus rid
  exists <- gets (M.member desired . canisters)
  if exists
  then res $ Rejected (RC_DESTINATION_INVALID, "Desired canister id already exists")
  else do
    createEmptyCanister desired
    res $ Completed $ CompleteCanisterId desired

processRequest rid (CreateRequest _user_id Nothing) = do
  existing_canisters <- gets (M.keys . canisters)
  let new_id = freshId existing_canisters
  createEmptyCanister new_id
  setReqStatus rid $ Completed $ CompleteCanisterId new_id

processRequest rid (InstallRequest canister_id user_id can_mod dat reinstall) = do
  let res = setReqStatus rid
  case parseCanister can_mod of
    Left err -> res $ Rejected (RC_SYS_FATAL, "Parsing failed: " ++ err)
    Right can_mod ->
      gets (M.lookup canister_id . canisters) >>= \case
        Nothing -> res $ Rejected (RC_DESTINATION_INVALID, "canister does not exist: " ++ show canister_id)
        Just old_canister -> case (reinstall, isJust old_canister) of
          (False, True) -> res $ Rejected (RC_DESTINATION_INVALID, "canister is not empty during installation")
          (True, False) -> res $ Rejected (RC_DESTINATION_INVALID, "canister is empty during reinstallation")
          _ -> case init_method can_mod canister_id user_id dat of
            Trap msg ->
              res $ Rejected (RC_CANISTER_ERROR, "Initialization trapped: " ++ msg)
            Return wasm_state -> do
              insertCanister canister_id can_mod wasm_state
              res $ Completed CompleteUnit

processRequest rid (UpgradeRequest canister_id user_id new_can_mod dat) = do
  let res = setReqStatus rid
  case parseCanister new_can_mod of
    Left err -> res $ Rejected (RC_SYS_FATAL, "Parsing failed: " ++ err)
    Right new_can_mod ->
      gets (M.lookup canister_id . canisters) >>= \case
        Nothing -> res $ Rejected (RC_DESTINATION_INVALID, "canister does not exist: " ++ show canister_id)
        Just Nothing -> res $ Rejected (RC_DESTINATION_INVALID, "canister is empty")
        Just (Just (CanState old_wasm_state old_can_mod)) ->
          case pre_upgrade_method old_can_mod old_wasm_state user_id of
            Trap msg -> res $ Rejected (RC_CANISTER_ERROR, "Pre-upgrade trapped: " ++ msg)
            Return mem ->
              case post_upgrade_method new_can_mod user_id user_id mem dat of
                Trap msg -> res $ Rejected (RC_CANISTER_ERROR, "post-upgrade trapped: " ++ msg)
                Return new_wasm_state -> do
                  insertCanister canister_id new_can_mod new_wasm_state
                  res $ Completed CompleteUnit

processRequest rid (UpdateRequest canister_id _user_id method arg) = do
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

rememberTrap :: ICM m => CallId -> String -> m ()
rememberTrap ctxt_id msg =
  modifyCallContext ctxt_id $ \ctxt -> ctxt { last_trap = Just msg }

callerOfCallID :: ICM m => CallId -> m EntityId
callerOfCallID ctxt_id = do
  ctxt <- getCallContext ctxt_id
  case origin ctxt of
    FromUser rid -> callerOfRequest rid
    FromCanister other_ctxt_id _callback -> calleeOfCallID other_ctxt_id
    FromInit entity_id -> return entity_id

calleeOfCallID :: ICM m => CallId -> m EntityId
calleeOfCallID ctxt_id = canister <$> getCallContext ctxt_id

respondedCallID :: ICM m => CallId -> m Responded
respondedCallID ctxt_id = responded <$> getCallContext ctxt_id

starveCallContext :: ICM m => CallId -> m ()
starveCallContext ctxt_id = do
  ctxt <- getCallContext ctxt_id
  let msg | Just t <- last_trap ctxt = "canister trapped: " ++ t
          | otherwise                = "canister did not respond"
  respondCallContext ctxt_id $ Reject (RC_CANISTER_ERROR, msg)

-- Message handling

enqueueMessage :: ICM m => Message -> m ()
enqueueMessage m = modify $ \ic -> ic { messages = messages ic :|> m }

processMessage :: ICM m => Message -> m ()
processMessage (CallMessage ctxt_id entry) = do
  callee <- calleeOfCallID ctxt_id
  let res r = respondCallContext ctxt_id r
  gets (M.lookup callee . canisters) >>= \case
    Nothing -> res $ Reject (RC_DESTINATION_INVALID, "canister does not exist: " ++ show callee)
    Just Nothing -> res $ Reject (RC_DESTINATION_INVALID, "canister is empty")
    Just (Just cs) ->
      invokeEntry ctxt_id cs entry >>= \case
        Trap msg -> do
          logTrap msg
          rememberTrap ctxt_id msg
        Return (new_state, (new_calls, mb_response)) -> do
          setCanisterState callee new_state
          mapM_ (newCall ctxt_id) new_calls
          mapM_ res mb_response

processMessage (ResponseMessage ctxt_id response) = do
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
    FromInit _ -> error "unexpected Response in Init"

invokeEntry :: ICM m =>
    CallId -> CanState -> EntryPoint ->
    m (TrapOr (WasmState, UpdateResult))
invokeEntry ctxt_id (CanState wasm_state can_mod) entry = do
    responded <- respondedCallID ctxt_id
    case entry of
      Public method dat -> do
        caller <- callerOfCallID ctxt_id
        case M.lookup method (update_methods can_mod) of
          Just f ->
            return $ f caller responded dat wasm_state
          Nothing -> do
            let reject = Reject (RC_DESTINATION_INVALID, "update method does not exist: " ++ method)
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
