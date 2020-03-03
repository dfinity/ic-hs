{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- |
This module defines ToJSON instances of the IC state.

We put them into their own module, despite the usual advise against orphan
instances, to emphasize that these are there just for debugging purposes using
`ic-ref`.

(Why JSON? Because Browsers render them nicely in a interactive display where
you can open and collapse subcomponents – much easier to get this feature this
way, compared to writing custom HTML output.)
-}

module IC.Debug.JSON (Generic, ToJSON(..), ToJSONKey(..), genericToJSONKey, defaultJSONKeyOptions, contramapToJSONKeyFunction, placeholder) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types

import qualified Data.ByteString.Lazy as BS
import qualified Wasm.Syntax.Values as W
import qualified Wasm.Syntax.AST as W
import qualified Text.Hex as H
import qualified Data.Text as T

instance ToJSON W.Value where

instance ToJSON BS.ByteString where
    toJSON = String . H.encodeHex . BS.toStrict

instance ToJSONKey BS.ByteString where
    toJSONKey = toJSONKeyText (H.encodeHex . BS.toStrict)

instance ToJSON (W.Module f) where
    toJSON _ = String "(module)"

placeholder :: String -> a -> Value
placeholder s = const (String (T.pack s))
