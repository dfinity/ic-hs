-- Unit test for IC.Test.Crypto.WebAuthn
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ViewPatterns #-}
module IC.Test.WebAuthn (webAuthnTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.ByteString.Lazy as BS

import qualified IC.Crypto.WebAuthn as WebAuthn

webAuthnTests :: TestTree
webAuthnTests = testGroup "WebAuthn crypto tests"
  [ testProperty "create-sign-verify" $
      \(BS.pack -> seed) (BS.pack -> msg) ->
        let sk = WebAuthn.createKey seed in
        let sig = WebAuthn.sign sk msg in
        WebAuthn.verify (WebAuthn.toPublicKey sk) msg sig
  , testProperty "invalid sig" $
      \(BS.pack -> seed) (BS.pack -> msg) (BS.pack -> sig) ->
        let sk = WebAuthn.createKey seed in
        not (WebAuthn.verify (WebAuthn.toPublicKey sk) msg sig)
  , testProperty "wrong message" $
      \(BS.pack -> seed) (BS.pack -> msg1) (BS.pack -> msg2) ->
        let sk = WebAuthn.createKey seed in
        let sig = WebAuthn.sign sk msg2 in
        msg1 /= msg2 ==> not (WebAuthn.verify (WebAuthn.toPublicKey sk) msg1 sig)
  ]

