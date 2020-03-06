{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Utilities to deconstruct a generic record.
-}
module IC.HTTP.GenR.Parse where

import Numeric.Natural
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Control.Monad.State
import qualified Data.HashMap.Lazy as HM
import GHC.Stack

import IC.HTTP.GenR

-- A monad to parse a record
-- (reading each field once, checking for left-over fields in the end)
type RecordM m = StateT (HM.HashMap T.Text GenR) m
type Field a = forall m. Parse m => GenR -> m a
class Monad m => Parse m where parseError :: HasCallStack => T.Text -> m a
instance Parse (Either T.Text) where  parseError = Left

record :: Parse m => RecordM m a -> GenR -> m a
record m (GRec hm) = (`evalStateT` hm) $ do
    x <- m
    -- Check for left-over fields
    hm <- get
    unless (HM.null hm) $ lift $
        parseError $ "Unexpected fields: " <> T.intercalate ", " (HM.keys hm)
    return x
record _ _ = parseError "Expected CBOR record"


field :: HasCallStack => Parse m => Field a -> T.Text -> RecordM m a
field parse name = do
    hm <- get
    put (HM.delete name hm)
    lift $ case HM.lookup name hm of
        Nothing -> parseError $ "Missing expected field \"" <> name <> "\""
        Just gr -> parse gr

optionalField ::  HasCallStack => Parse m => Field a -> T.Text -> RecordM m (Maybe a)
optionalField parse name = do
    hm <- get
    put (HM.delete name hm)
    case HM.lookup name hm of
        Nothing -> return Nothing
        Just gr -> lift $ Just <$> parse gr

text :: Field T.Text
text (GText t) = return t
text _ = parseError "Expected text value"

blob :: Field BS.ByteString
blob (GBlob b) = return (BS.fromStrict b)
blob _ = parseError "Expected blob"

nat :: Field Natural
nat (GNat n) = return n
nat _ = parseError "Expected natural number"

percentage :: Field Natural
percentage gr = do
    n <- nat gr
    unless (0 <= n && n <= 100) $
        parseError "Expected a percentage (0..100)"
    return n



