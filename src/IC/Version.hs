{-# LANGUAGE OverloadedStrings #-}
module IC.Version where

import Data.Text
import SourceId

specVersion, implVersion :: Text
specVersion = "0.10.2"
implVersion = pack SourceId.id
