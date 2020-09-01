{-
Plumbing related to Candid and the management canister.
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IC.Management where

import Codec.Candid
import IC.Types

-- This needs cleaning up
principalToEntityId :: Principal -> EntityId
principalToEntityId = EntityId . rawPrincipal

entityIdToPrincipal :: EntityId -> Principal
entityIdToPrincipal = Principal . rawEntityId

type InstallMode = [candidType|
    variant {install : null; reinstall : null; upgrade : null}
  |]

type RunState = [candidType|
    variant { running; stopping; stopped }
  |]

type ICManagement m =
  [candid|
    service ic : {
      create_canister : () -> (record {canister_id : principal});
      install_code : (record {
        mode : variant {install : null; reinstall : null; upgrade : null};
        canister_id : principal;
        wasm_module : blob;
        arg : blob;
        compute_allocation : opt nat;
      }) -> ();
      set_controller : (record {
        canister_id : principal;
        new_controller : principal;
      }) -> ();
      start_canister : (record {
        canister_id : principal;
      }) -> ();
      stop_canister : (record {
        canister_id : principal;
      }) -> ();
      canister_status : (record {
        canister_id : principal;
      }) -> (record {
        status : variant { running; stopping; stopped }
      });
      delete_canister : (record {
        canister_id : principal;
      }) -> ();
      raw_rand : () -> (blob);
    }
  |]
