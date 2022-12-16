{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module IC.Test.Spec.TECDSA (tests) where

import Test.Tasty
import qualified Data.Vector as Vec
import IC.Test.Spec.Utils
import IC.Test.Agent
import IC.Test.Agent.Calls
import Data.Row as R
import Test.Tasty.HUnit
import IC.Test.Universal (noop)
import IC.Hash (sha256)

tests :: HasAgentConfig => Blob -> TestTree
tests ecid = testGroup "tECDSA"
    [ testCase "sign and verify" $ do
      cid <- install ecid noop
      cid2 <- install ecid noop
      sig1 <- ic_sign_with_ecdsa (ic00via cid) cid (sha256 "internet computer")
      sig2 <- ic_sign_with_ecdsa (ic00via cid2) cid2 (sha256 "internet computer")
      -- if canister id is unset, default to a caller id
      pk1 <- ic_ecdsa_public_key (ic00via cid) cid Nothing Vec.empty
      pk2 <- ic_ecdsa_public_key (ic00via cid) cid (Just cid2) Vec.empty

      assertBool "incorrect signature" $ verifySignature (sha256 "internet computer") (sig1 .! #signature) (pk1 .! #public_key)
      assertBool "correct signature, should be incorrect" $ not $ verifySignature (sha256 "internet computer") (sig1 .! #signature) (pk2 .! #public_key)
      assertBool "incorrect signature" $ not $ verifySignature (sha256 "internet computer") (sig2 .! #signature) (pk1 .! #public_key)
      assertBool "correct signature, should be incorrect" $ verifySignature (sha256 "internet computer") (sig2 .! #signature) (pk2 .! #public_key)

    , simpleTestCase "invalid derivation path" ecid $ \cid -> do
      ic_ecdsa_public_key' (ic00via cid) cid Nothing (Vec.singleton "clearly not Word32") >>= isReject [5]

    , simpleTestCase "id of non-existent canister" ecid $ \cid -> do
      ic_ecdsa_public_key' (ic00via cid) cid (Just "Clearly not a valid EntityId") Vec.empty >>= isReject [3]
    ]
