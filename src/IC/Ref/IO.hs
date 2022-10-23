{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module IC.Ref.IO where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Row as R
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vec
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as C
import Network.HTTP.Types.Status (statusCode)
import Network.Connection (TLSSettings(..))
import Data.CaseInsensitive (original)
import Data.Row ((.==), (.+))

import IC.Management (HttpResponse)

sendHttpRequest :: Bool -> T.Text -> BS.ByteString -> [(CI.CI BS.ByteString, BS.ByteString)] -> LBS.ByteString -> IO HttpResponse
sendHttpRequest noTls url method headers body = do
    let noTlsSettings = TLSSettingsSimple { settingDisableCertificateValidation = True
      , settingDisableSession = False
      , settingUseServerName = False
      }
    m <- C.newManager $ if noTls then C.mkManagerSettings noTlsSettings Nothing else C.tlsManagerSettings
    initReq <- C.parseRequest (T.unpack url)
    let req = initReq {
      C.method = method,
      C.requestHeaders = headers,
      C.requestBody = C.RequestBodyLBS body
    }
    toHttpResponse <$> C.httpLbs req m
  where
    toHeaderEntry (n, v) = R.empty
      .+ #name  .== (T.decodeUtf8 (original n))
      .+ #value .== (T.decodeUtf8 v)
    toHttpResponse r = R.empty
      .+ #status .== (fromIntegral (statusCode $ C.responseStatus r))
      .+ #headers .== (Vec.fromList $ map toHeaderEntry (C.responseHeaders r))
      .+ #body .== (C.responseBody r)
