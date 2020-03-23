{- |
Implements the special forms of ids (https://docs.dfinity.systems/public/#id-classes)
-}
{-# LANGUAGE TypeApplications #-}
module IC.Id.Forms where

import qualified Data.ByteString.Lazy as BS
import Crypto.Hash (hashlazy, SHA256)
import Data.ByteArray (convert)

type Blob = BS.ByteString

mkOpaqueId :: Blob -> Blob
mkOpaqueId b =
    b <> BS.singleton 1

isOpaqueId :: Blob -> Bool
isOpaqueId b = BS.drop (BS.length b - 1) b == BS.singleton 1

mkSelfAuthenticatingId :: Blob -> Blob
mkSelfAuthenticatingId pubkey =
    h pubkey <> BS.singleton 2

isSelfAuthenticatingId :: Blob -> Blob -> Bool
isSelfAuthenticatingId pubkey id =
    mkSelfAuthenticatingId pubkey == id

mkDerivedId :: Blob -> Blob -> Blob
mkDerivedId _ bytes | BS.length bytes /= 8 = error "mkDerivedId: Need exactly 8 extra bytes"
mkDerivedId registering bytes =
    h registering <> bytes <> BS.singleton 3

isDerivedId :: Blob -> Blob -> Bool
isDerivedId registering blob =
    BS.length blob == 256`div`8 + 8 + 1 &&
    BS.last blob == 3 &&
    BS.take (256`div`8) blob == h registering

h :: BS.ByteString -> BS.ByteString
h = BS.fromStrict . convert . hashlazy @SHA256