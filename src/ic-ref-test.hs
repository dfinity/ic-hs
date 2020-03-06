{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

-- Todo: Break module into plumbing and actual tests

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.HashMap.Lazy as HM
import Network.HTTP.Client
import Network.HTTP.Types
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans

import IC.HTTP.GenR
import IC.HTTP.GenR.Parse
import IC.HTTP.CBOR (decode, encode)
import IC.HTTP.RequestId
import IC.Test.Options

type Blob = BS.ByteString

main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where
    ingredients = defaultIngredients ++ [includingOptions [endpointOption]]

tests :: TestTree
tests = testGroup "Tests" [icTests, idTests]

-- To use IC.HTTP.CBOR with HUnit
instance Parse IO where parseError = assertFailure . T.unpack

asRight :: HasCallStack => Either T.Text a -> IO a
asRight (Left err) = assertFailure (T.unpack err)
asRight (Right gr) = return gr

getCBOR :: HasCallStack => Endpoint -> String -> IO (Response BS.ByteString)
getCBOR (Endpoint endpoint) path = do
  -- this uses a fresh manger for each request. If we care about performance,
  -- this could revisited (to share connections)
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ endpoint ++ path
  httpLbs request manager

postCBOR :: HasCallStack => Endpoint -> String -> GenR -> IO (Response BS.ByteString)
postCBOR (Endpoint endpoint) path gr = do
  -- this uses a fresh manger for each request. If we care about performance,
  -- this could revisited (to share connections)
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ endpoint ++ path
  request <- return $ request
    { method = "POST"
    , requestBody = RequestBodyLBS $ BS.toLazyByteString $ encode gr
    , requestHeaders = [(hContentType, "application/cbor")]
    }
  httpLbs request manager

okCBOR :: HasCallStack => Response BS.ByteString -> IO GenR
okCBOR response = do
  responseStatus response @?= ok200
  asRight $ decode $ responseBody response

code4xx :: HasCallStack => Response BS.ByteString -> IO ()
code4xx response = assertBool
    ("Status " ++ show c ++ " is not 4xx")
    (400 <= c && c < 500)
  where c = statusCode (responseStatus response)

-- Runs test once for each field with that field removed.
omitFields :: GenR -> (GenR -> Assertion) -> [TestTree]
omitFields (GRec hm) act =
    [ let hm' = HM.delete f hm
      in testCase ("Omitting " ++ T.unpack f) $ act (GRec hm')
    | f <- fields
    ]
  where fields = HM.keys hm
omitFields _ _ = error "omitFields needs a GRec"


doesn'tExist :: Blob
doesn'tExist = "DEADBEEF"


queryToNonExistant :: GenR
queryToNonExistant = rec
    [ "request_type" =: GText "query"
    , "canister_id" =: GBlob (BS.toStrict doesn'tExist)
    , "method_name" =: GText "foo"
    , "arg" =: GBlob "nothing to see here"
    ]

icTests :: TestTree
icTests = askOption $ \ep -> testGroup "Public Spec acceptance tests"
  [ testCase "status endpoint" $ do
      gr <- getCBOR ep "/api/v1/status" >>= okCBOR
      flip record gr $ do
        _ <- field text "ic_api_version"
        _ <- optionalField text "impl_source"
        _ <- optionalField text "impl_version"
        _ <- optionalField text "impl_revision"
        return ()

  , testGroup "query: required fields" $
      omitFields queryToNonExistant $ \req ->
        postCBOR ep "/api/v1/read" req >>= code4xx

  , testCase "query to non-existing canister (status and reject code)" $ do
      gr <- postCBOR ep "/api/v1/read" queryToNonExistant >>= okCBOR
      flip record gr $ do
        s <- field text "status"
        lift $ s @?= "rejected"
        n <- field nat "reject_code"
        lift $ n @?= 3
        _ <- field text "reject_message"
        return ()
  ]


idTests :: TestTree
idTests = testGroup "Reference request id calculation"
  [ testCase "example from public spec" $
     let gr = GRec $ mconcat
          [ "request_type" =: GText "call"
          , "canister_id" =: GBlob "\x00\x00\x00\x00\x00\x00\x04\xD2"
          , "method_name" =: GText "hello"
          , "arg" =: GBlob "DIDL\x00\xFD*"
          ]
      in requestId gr @?= "\x87\x81\x29\x1c\x34\x7d\xb3\x2a\x9d\x8c\x10\xeb\x62\xb7\x10\xfc\xe5\xa9\x3b\xe6\x76\x47\x4c\x42\xba\xbc\x74\xc5\x18\x58\xf9\x4b"
  ]
