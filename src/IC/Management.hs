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

-- copied from ../spec/ic.did
type ICManagement m = [candid|
type canister_id = principal;
type user_id = principal;
type wasm_module = blob;
type unit = blob;
service ic : {
  create_canister : () -> (record {canister_id : canister_id});
  install_code : (record {
    mode : variant {install; reinstall; upgrade};
    canister_id : canister_id;
    wasm_module : wasm_module;
    arg : blob;
    compute_allocation : opt nat;
    memory_allocation : opt nat;
  }) -> ();
  set_controller : (record {canister_id : canister_id; new_controller : principal}) -> ();
  start_canister : (record {canister_id : canister_id}) -> ();
  stop_canister : (record {canister_id : canister_id}) -> ();
  canister_status : (record {canister_id : canister_id}) -> (record {
      status : variant { running; stopping; stopped };
      module_hash: opt blob;
      controller: principal;
      memory_size: nat;
      cycles: nat;
  });
  delete_canister : (record {canister_id : canister_id}) -> ();
  deposit_cycles : (record {canister_id : canister_id}) -> ();
  raw_rand : () -> (blob);

  // provisional interfaces for the pre-ledger world
  provisional_create_canister_with_cycles :
    (record { amount: opt nat }) -> (record {canister_id : canister_id});
  provisional_top_up_canister :
    (record { canister_id: canister_id; amount: nat }) -> ();
}
  |]
