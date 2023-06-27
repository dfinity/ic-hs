{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module IC.Ref.IO (sendHttpRequest) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Row as R
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vec
import qualified Data.X509 as C
import qualified Data.X509.CertificateStore as C
import qualified Data.X509.Validation as C
import qualified Network.Connection as C
import qualified Network.TLS as C
import qualified Network.TLS.Extra.Cipher as C
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as C
import Network.HTTP.Types.Status (statusCode)
import Control.Exception
import Data.CaseInsensitive (original)
import Data.Default.Class (def)
import Data.Row ((.==), (.+))

import IC.Management (HttpResponse)

sendHttpRequest :: [C.SignedCertificate] -> T.Text -> BS.ByteString -> [(CI.CI BS.ByteString, BS.ByteString)] -> LBS.ByteString -> IO (Either String HttpResponse)
sendHttpRequest certs url method headers body = do
    let validate = \ca_store -> C.validateDefault (C.makeCertificateStore $ certs ++ (C.listCertificates ca_store))
    let client_params = (C.defaultParamsClient "" BS.empty) {
          C.clientHooks = def {C.onServerCertificate = validate}
        , C.clientSupported = def { C.supportedCiphers = C.ciphersuite_default }
        }
    let manager_settings = C.mkManagerSettings (C.TLSSettings client_params) Nothing
    m <- C.newTlsManagerWith manager_settings
    initReq <- C.parseRequest (T.unpack url)
    let req = initReq {
      C.method = method,
      C.requestHeaders = headers,
      C.requestBody = C.RequestBodyLBS body
    }
    resp <- try (C.httpLbs req m) :: IO (Either SomeException (C.Response LBS.ByteString))
    case resp of
      Left e -> return $ Left $ show e
      Right r -> return $ Right $ toHttpResponse r
  where
    toHeaderEntry (n, v) = R.empty
      .+ #name  .== (T.decodeUtf8 (original n))
      .+ #value .== (T.decodeUtf8 v)
    toHttpResponse r = R.empty
      .+ #status .== (fromIntegral (statusCode $ C.responseStatus r))
      .+ #headers .== (Vec.fromList $ map toHeaderEntry (C.responseHeaders r))
      .+ #body .== (C.responseBody r)
