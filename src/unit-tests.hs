{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified IC.Crypto.BLS as BLS
import IC.Test.HashTree
import IC.Test.BLS
import IC.Test.WebAuthn
import IC.Test.ECDSA
import IC.HTTP.GenR
import IC.HTTP.RequestId

main :: IO ()
main = do
    BLS.init
    defaultMain tests

tests :: TestTree
tests = testGroup "ic-ref unit tests"
  [ testCase "Request id calculation from public spec" $
     let gr = GRec $ mconcat
          [ "request_type" =: GText "call"
          , "canister_id" =: GBlob "\x00\x00\x00\x00\x00\x00\x04\xD2"
          , "method_name" =: GText "hello"
          , "arg" =: GBlob "DIDL\x00\xFD*"
          ]
      in requestId gr @?= "\x87\x81\x29\x1c\x34\x7d\xb3\x2a\x9d\x8c\x10\xeb\x62\xb7\x10\xfc\xe5\xa9\x3b\xe6\x76\x47\x4c\x42\xba\xbc\x74\xc5\x18\x58\xf9\x4b"
  , hashTreeTests
  , blsTests
  -- These are slow, make sure to run only a few of them
  , adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n`div`20)) webAuthnTests
  , ecdsaTests
  ]
