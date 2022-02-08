{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module IC.HTTP.IO where

import qualified Data.Row as R
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vec
import qualified Network.HTTP.Client as C
import Network.HTTP.Types.Status (statusCode)
import Data.CaseInsensitive (original)
import Data.Row ((.==), (.+))

import IC.Management (HttpResponse)

sendHttpRequest :: T.Text -> IO HttpResponse
sendHttpRequest url = do
    m <- C.newManager C.defaultManagerSettings
    initReq <- C.parseRequest (T.unpack url)
    let req = initReq {
      C.method = "GET"
    }
    toHttpResponse <$> C.httpLbs req m
  where
    toHttpResponse r = R.empty
      .+ #status .== (fromIntegral (statusCode $ C.responseStatus r))
      .+ #headers .== (Vec.fromList $ map (\(n, v) -> (T.decodeUtf8 (original n), T.decodeUtf8 v)) (C.responseHeaders r))
      .+ #body .== (C.responseBody r)