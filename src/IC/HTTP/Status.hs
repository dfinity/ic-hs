{-# LANGUAGE OverloadedStrings #-}
-- | the response to the status request
module IC.HTTP.Status where

import IC.HTTP.GenR
import Data.HashMap.Lazy

r :: GenR
r = GRec $ mconcat
        [ "ic_api_version" =: GText "unversioned"
        , "impl_source" =: GText "https://github.com/dfinity-lab/ic-ref"
        ]
  where
    -- Convenient syntax
    (=:) = Data.HashMap.Lazy.singleton
