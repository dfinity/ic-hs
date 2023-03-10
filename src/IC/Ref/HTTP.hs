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
This module implements the canister HTTP outcall logic of the Internet Computer.
-}
module IC.Ref.HTTP (icHttpRequest)
where

import qualified Data.CaseInsensitive as CI
import qualified Data.Map as M
import qualified Data.Row as R
import qualified Data.Row.Variants as V
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vec
import Data.List
import Control.Monad.Except
import Codec.Candid
import Data.Row ((.==), (.+), (.!), type (.!))
import Network.URI (parseURI)

import IC.Types
import IC.Constants
import IC.Canister
import IC.Utils
import IC.Management
import IC.Ref.IO (sendHttpRequest)
import IC.Ref.Types

icHttpRequest :: (ICM m, CanReject m) => EntityId -> Maybe Subnet -> CallId -> ICManagement m .! "http_request"
icHttpRequest caller maybe_subnet ctxt_id r = do
  available <- getCallContextCycles ctxt_id
  (_, subnet_type, subnet_size, _, _) <- case maybe_subnet of
      Nothing -> getSubnetFromCanisterId caller
      Just subnet -> return subnet
  let fee = fromIntegral $ http_request_fee r (subnet_type, subnet_size)
  let url = T.unpack $ r .! #url
  let max_resp_size = max_response_size r
  let transform_principal_check =
        case (r .! #transform) of
          Nothing -> True
          Just t -> case t .! #function of
            FuncRef p _ -> principalToEntityId p == caller
  if
    | not transform_principal_check ->
      reject RC_CANISTER_REJECT "transform needs to be exported by the caller canister" (Just EC_CANISTER_REJECTED)
    | max_resp_size > max_response_bytes_limit ->
      reject RC_CANISTER_REJECT ("max_response_bytes cannot exceed " ++ show max_response_bytes_limit) (Just EC_CANISTER_REJECTED)
    | utf8_length (r .! #url) > max_http_request_url_length ->
      reject RC_CANISTER_REJECT "Failed to parse URL: uri too long" (Just EC_INVALID_ARGUMENT)
    | http_request_size r > max_request_bytes_limit ->
      reject RC_CANISTER_REJECT ("number of bytes to represent all request header names and values and request body exceeds the limit of " ++ show max_request_bytes_limit) (Just EC_CANISTER_REJECTED)
    | not (check_http_request_headers_number r) ->
      reject RC_CANISTER_REJECT ("number of request http headers exceeds the limit of " ++ show http_headers_max_number) (Just EC_CANISTER_REJECTED)
    | not (check_http_request_headers_name_length r) ->
      reject RC_CANISTER_REJECT ("number of bytes to represent some request http header name exceeds the limit of " ++ show http_headers_max_name_value_length) (Just EC_CANISTER_REJECTED)
    | not (check_http_request_headers_value_length r) ->
      reject RC_CANISTER_REJECT ("number of bytes to represent some request http header value exceeds the limit of " ++ show http_headers_max_name_value_length) (Just EC_CANISTER_REJECTED)
    | not (check_http_request_headers_total_size r) ->
      reject RC_CANISTER_REJECT ("total number of bytes to represent request http headers exceeds the limit of " ++ show http_headers_max_total_size) (Just EC_CANISTER_REJECTED)
    | available < fee ->
      reject RC_CANISTER_REJECT ("http_request request sent with " ++ show available ++ " cycles, but " ++ show fee ++ " cycles are required.") (Just EC_CANISTER_REJECTED)
    | otherwise -> do
      setCallContextCycles ctxt_id (available - fee)
      if
        | parseURI url == Nothing ->
          reject RC_SYS_FATAL "url must be valid according to RFC-3986" (Just EC_INVALID_ARGUMENT)
        | not (isPrefixOf "https://" url) ->
          reject RC_SYS_FATAL "url must start with https://" (Just EC_INVALID_ARGUMENT)
        | otherwise -> do
          method <- if
            | (r .! #method) == V.IsJust #get () -> return $ T.encodeUtf8 "GET"
            | (r .! #method) == V.IsJust #post () -> return $ T.encodeUtf8 "POST"
            | (r .! #method) == V.IsJust #head () -> return $ T.encodeUtf8 "HEAD"
            | otherwise -> reject RC_SYS_FATAL ("unknown HTTP method") (Just EC_CANISTER_REJECTED)
          let headers = map (\r -> (CI.mk $ T.encodeUtf8 $ r .! #name, T.encodeUtf8 $ r .! #value)) $ Vec.toList (r .! #headers)
          let body = case r .! #body of Nothing -> ""
                                        Just b -> b
          resp <- liftIO $ sendHttpRequest getRootCerts (r .! #url) method headers body
          if
            | http_response_size resp > max_resp_size ->
              reject RC_SYS_FATAL ("response body size cannot exceed " ++ show max_resp_size ++ " bytes") (Just EC_CANISTER_REJECTED)
            | not (check_http_response_headers_number resp) ->
              reject RC_SYS_FATAL ("number of response http headers exceeds the limit of " ++ show http_headers_max_number) (Just EC_CANISTER_REJECTED)
            | not (check_http_response_headers_name_length resp) ->
              reject RC_SYS_FATAL ("number of bytes to represent some response http header name exceeds the limit of " ++ show http_headers_max_name_value_length) (Just EC_CANISTER_REJECTED)
            | not (check_http_response_headers_value_length resp) ->
              reject RC_SYS_FATAL ("number of bytes to represent some response http header value exceeds the limit of " ++ show http_headers_max_name_value_length) (Just EC_CANISTER_REJECTED)
            | not (check_http_response_headers_total_size resp) ->
              reject RC_SYS_FATAL ("total number of bytes to represent response http headers exceeds the limit of " ++ show http_headers_max_total_size) (Just EC_CANISTER_REJECTED)
            | otherwise -> do
              case (r .! #transform) of
                Nothing -> return resp
                Just t -> case t .! #function of
                  FuncRef p m -> do
                      let cid = principalToEntityId p
                      let arg = R.empty
                            .+ #response .== resp
                            .+ #context .== t .! #context
                      can_mod <- getCanisterMod cid
                      wasm_state <- getCanisterState cid
                      env <- canisterEnv cid
                      case M.lookup (T.unpack m) (query_methods can_mod) of
                        Nothing -> reject RC_DESTINATION_INVALID "transform function with a given name does not exist" (Just EC_METHOD_NOT_FOUND)
                        Just f -> case f managementCanisterId env (Codec.Candid.encode arg) wasm_state of
                          Return (Reply r) -> case Codec.Candid.decode @HttpResponse r of
                            Left _ -> reject RC_CANISTER_ERROR "could not decode the response" (Just EC_INVALID_ENCODING)
                            Right resp ->
                              if fromIntegral (BS.length r) > max_response_bytes_limit then
                                reject RC_SYS_FATAL ("transformed response body size cannot exceed " ++ show max_response_bytes_limit ++ " bytes") (Just EC_CANISTER_REJECTED)
                              else
                                return resp
                          _ -> reject RC_CANISTER_ERROR "transform did not return a response properly" (Just EC_CANISTER_DID_NOT_REPLY)
