{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}

module IC.Test.Agent.UnsafeCalls
    (
      ic_canister_status,
      ic_create,
      ic_delete_canister,
      ic_deposit_cycles,
      ic_ecdsa_public_key,
      ic_http_get_request,
      ic_http_post_request,
      ic_http_head_request,
      ic_long_url_http_request,
      ic_install,
      ic_provisional_create,
      ic_raw_rand,
      ic_set_controllers,
      ic_sign_with_ecdsa,
      ic_start_canister,
      ic_stop_canister,
      ic_top_up,
      ic_uninstall,
      ic_update_settings,
    ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as Vec
import qualified Data.Text as T
import Numeric.Natural
import Test.Tasty.HUnit
import Codec.Candid (Principal(..))
import Data.Row
import qualified Data.Word as W

import IC.Management
import IC.Id.Forms
import IC.Test.Agent
import IC.Types(TestSubnetConfig)
import IC.Utils
import IC.Test.Agent.Calls

ic_create :: (HasCallStack, HasAgentConfig, PartialSettings r) => IC00 -> Blob -> Rec r -> IO Blob
ic_create ic00 ecid ps = do
  r <- callIC ic00 ecid #create_canister $ empty
    .+ #settings .== Just (fromPartialSettings ps)
  return (rawPrincipal (r .! #canister_id))

ic_provisional_create ::
    (HasCallStack, HasAgentConfig, PartialSettings r) =>
    IC00 -> Blob -> Maybe Principal -> Maybe Natural -> Rec r -> IO Blob
ic_provisional_create ic00 ecid specified_id cycles ps = do
  r <- callIC ic00 ecid #provisional_create_canister_with_cycles $ empty
    .+ #amount .== cycles
    .+ #settings .== Just (fromPartialSettings ps)
    .+ #specified_id .== specified_id
  return (rawPrincipal (r .! #canister_id))

ic_install :: (HasCallStack, HasAgentConfig) => IC00 -> InstallMode -> Blob -> Blob -> Blob -> IO ()
ic_install ic00 mode canister_id wasm_module arg = do
  callIC ic00 canister_id #install_code $ empty
    .+ #mode .== mode
    .+ #canister_id .== Principal canister_id
    .+ #wasm_module .== wasm_module
    .+ #arg .== arg

ic_uninstall :: (HasCallStack, HasAgentConfig) => IC00 -> Blob -> IO ()
ic_uninstall ic00 canister_id = do
  callIC ic00 canister_id #uninstall_code $ empty
    .+ #canister_id .== Principal canister_id

ic_update_settings :: (HasAgentConfig, PartialSettings r) => IC00 -> Blob -> Rec r -> IO ()
ic_update_settings ic00 canister_id r = do
  callIC ic00 canister_id #update_settings $ empty
    .+ #canister_id .== Principal canister_id
    .+ #settings .== fromPartialSettings r

ic_set_controllers :: HasAgentConfig => IC00 -> Blob -> [Blob] -> IO ()
ic_set_controllers ic00 canister_id new_controllers = do
  callIC ic00 canister_id #update_settings $ empty
    .+ #canister_id .== Principal canister_id
    .+ #settings .== fromPartialSettings (#controllers .== Vec.fromList (map Principal new_controllers))

ic_start_canister :: HasAgentConfig => IC00 -> Blob -> IO ()
ic_start_canister ic00 canister_id = do
  callIC ic00 canister_id #start_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_stop_canister :: HasAgentConfig => IC00 -> Blob -> IO ()
ic_stop_canister ic00 canister_id = do
  callIC ic00 canister_id #stop_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_canister_status ::
    forall a b. (a -> IO b) ~ (ICManagement IO .! "canister_status") =>
    HasAgentConfig => IC00 -> Blob -> IO b
ic_canister_status ic00 canister_id = do
  callIC ic00 canister_id #canister_status $ empty
    .+ #canister_id .== Principal canister_id

ic_deposit_cycles :: HasAgentConfig => IC00 -> Blob -> IO ()
ic_deposit_cycles ic00 canister_id = do
  callIC ic00 canister_id #deposit_cycles $ empty
    .+ #canister_id .== Principal canister_id

ic_top_up :: HasAgentConfig => IC00 -> Blob -> Natural -> IO ()
ic_top_up ic00 canister_id amount = do
  callIC ic00 canister_id #provisional_top_up_canister $ empty
    .+ #canister_id .== Principal canister_id
    .+ #amount .== amount

ic_delete_canister :: HasAgentConfig => IC00 -> Blob -> IO ()
ic_delete_canister ic00 canister_id = do
  callIC ic00 canister_id #delete_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_raw_rand :: HasAgentConfig => IC00 -> Blob -> IO Blob
ic_raw_rand ic00 ecid =
  callIC ic00 ecid #raw_rand ()

ic_http_get_request ::
    forall a b. (a -> IO b) ~ (ICManagement IO .! "http_request") =>
    HasAgentConfig => IC00WithCycles -> TestSubnetConfig -> String -> Maybe W.Word64 -> Maybe (String, Blob) -> Blob -> IO b
ic_http_get_request ic00 (_, subnet_type, subnet_size, _) path max_response_bytes transform canister_id =
  callIC (ic00 $ http_request_fee request (subnet_type, subnet_size)) "" #http_request request
  where
    request = empty
      .+ #url .== (T.pack $ "https://" ++ httpbin ++ "/" ++ path)
      .+ #max_response_bytes .== max_response_bytes
      .+ #method .== enum #get
      .+ #headers .== Vec.empty
      .+ #body .== Nothing
      .+ #transform .== (toTransformFn transform canister_id)

ic_http_post_request :: HasAgentConfig =>
    (a -> IO b) ~ (ICManagement IO .! "http_request") =>
    IC00WithCycles -> TestSubnetConfig -> String -> Maybe W.Word64 -> Maybe BS.ByteString -> Vec.Vector HttpHeader -> Maybe (String, Blob) -> Blob -> IO b
ic_http_post_request ic00 (_, subnet_type, subnet_size, _) path max_response_bytes body headers transform canister_id =
  callIC (ic00 $ http_request_fee request (subnet_type, subnet_size)) "" #http_request request
  where
    request = empty
      .+ #url .== (T.pack $ "https://" ++ httpbin ++ "/" ++ path)
      .+ #max_response_bytes .== max_response_bytes
      .+ #method .== enum #post
      .+ #headers .== headers
      .+ #body .== body
      .+ #transform .== (toTransformFn transform canister_id)

ic_http_head_request :: HasAgentConfig =>
    (a -> IO b) ~ (ICManagement IO .! "http_request") =>
    IC00WithCycles -> TestSubnetConfig -> String -> Maybe W.Word64 -> Maybe BS.ByteString -> Vec.Vector HttpHeader -> Maybe (String, Blob) -> Blob -> IO b
ic_http_head_request ic00 (_, subnet_type, subnet_size, _) path max_response_bytes body headers transform canister_id =
  callIC (ic00 $ http_request_fee request (subnet_type, subnet_size)) "" #http_request request
  where
    request = empty
      .+ #url .== (T.pack $ "https://" ++ httpbin ++ "/" ++ path)
      .+ #max_response_bytes .== max_response_bytes
      .+ #method .== enum #head
      .+ #headers .== headers
      .+ #body .== body
      .+ #transform .== (toTransformFn transform canister_id)

ic_long_url_http_request :: HasAgentConfig =>
  forall a b. (a -> IO b) ~ (ICManagement IO .! "http_request") =>
  IC00WithCycles -> TestSubnetConfig -> String -> W.Word64 -> Maybe (String, Blob) -> Blob -> IO b
ic_long_url_http_request ic00 (_, subnet_type, subnet_size, _) proto len transform canister_id =
  callIC (ic00 $ http_request_fee request (subnet_type, subnet_size)) "" #http_request request
  where
    l = fromIntegral len - (length $ proto ++ httpbin ++ "/ascii/")
    path = take l $ repeat 'x'
    request = empty
      .+ #url .== (T.pack $ proto ++ httpbin ++ "/ascii/" ++ path)
      .+ #max_response_bytes .== Nothing
      .+ #method .== enum #get
      .+ #headers .== Vec.empty
      .+ #body .== Nothing
      .+ #transform .== (toTransformFn transform canister_id)

ic_ecdsa_public_key ::
    forall a b. (a -> IO b) ~ (ICManagement IO .! "ecdsa_public_key") =>
    HasAgentConfig => IC00 -> Blob -> Maybe Blob -> Vec.Vector Blob -> IO b
ic_ecdsa_public_key ic00 ecid canister_id path =
  callIC ic00 ecid #ecdsa_public_key $ empty
    .+ #derivation_path .== path
    .+ #canister_id .== (fmap Principal canister_id)
    .+ #key_id .== (empty
       .+ #curve .== enum #secp256k1
       .+ #name .== (T.pack "0")
    )

ic_sign_with_ecdsa ::
    forall a b. (a -> IO b) ~ (ICManagement IO .! "sign_with_ecdsa") =>
    HasAgentConfig => IC00 -> Blob -> Blob -> IO b
ic_sign_with_ecdsa ic00 ecid msg =
  callIC ic00 ecid #sign_with_ecdsa $ empty
    .+ #derivation_path .== Vec.empty
    .+ #message_hash .== msg
    .+ #key_id .== (empty
       .+ #curve .== enum #secp256k1
       .+ #name .== (T.pack "0")
    )
