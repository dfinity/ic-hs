{-
Plumbing related to Candid and the management canister.
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IC.Management (module IC.Management, CanisterId, EntityId(..)) where

import qualified Data.ByteString.Lazy as BS
import Data.Row
import Numeric.Natural
import Codec.Candid

import IC.Types

data InstallMode = Install | Reinstall | Upgrade
instance Candid InstallMode where
    type Rep InstallMode = 'VariantT
        '[ '( 'N "install", 'NullT)
        ,  '( 'N "reinstall", 'NullT)
        ,  '( 'N "upgrade", 'NullT)
        ]
    toCandid Install = Left ()
    toCandid Reinstall = Right (Left ())
    toCandid Upgrade = Right (Right (Left ()))
    fromCandid (Left ()) = Install
    fromCandid (Right (Left ())) = Reinstall
    fromCandid (Right (Right (Left ()))) = Upgrade
    fromCandid (Right (Right (Right x))) = case x of {}

instance Candid EntityId where
    type Rep EntityId = 'PrincipalT
    toCandid = BS.toStrict . rawEntityId
    fromCandid = EntityId . BS.fromStrict

type WasmModule = Blob

type ICManagement m =
  ( "create_canister" .==
    (Rec ("desired_id" .== Maybe EntityId) -> m (Rec ("canister_id" .== EntityId)))
  .+ "install_code" .==
    (Rec ( "mode" .== InstallMode .+ "canister_id" .== EntityId .+
         "wasm_module" .== BS.ByteString .+ "arg" .== BS.ByteString .+
         "compute_allocation" .== Maybe Natural)
    -> m ())
  .+ "set_controller" .==
    (Rec ("canister_id" .== EntityId .+ "new_controller" .== EntityId)
    -> m ())
  )
