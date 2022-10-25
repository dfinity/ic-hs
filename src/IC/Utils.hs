{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{- |
Generic utilities related to standard or imported data structures that we do
don’t want to see in non-plumbing code.
-}
module IC.Utils where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Word as W

freshKey :: M.Map Int a -> Int
freshKey m | M.null m = 0
           | otherwise = fst (M.findMax m) + 1

repeatWhileTrue :: Monad m => m Bool -> m ()
repeatWhileTrue act = act >>= \case
    True -> repeatWhileTrue act
    False -> return ()


duplicates :: Ord a => [a] -> [a]
duplicates = go S.empty
  where
    go _s [] = []
    go s (x:xs) | x `S.member` s = x : go s' xs
                | otherwise      =     go s' xs
      where s' = S.insert x s


-- Wrappers to hide strict/lazy conversion from view
toUtf8 :: T.Text -> BS.ByteString
toUtf8 = BS.fromStrict . T.encodeUtf8

fromUtf8 :: BS.ByteString -> Maybe T.Text
fromUtf8 b = case T.decodeUtf8' (BS.toStrict b) of
    Left _ -> Nothing
    Right t -> Just t

-- Compute UTF-8 length of Text
utf8_length :: T.Text -> W.Word64
utf8_length = fromIntegral . BS.length . toUtf8

-- ic-ref config
data RefConfig = RefConfig
    { tc_noTls :: Bool
    }

makeRefConfig :: Bool -> IO RefConfig
makeRefConfig noTls = do
    return RefConfig
        { tc_noTls = noTls
        }

type HasRefConfig = (?refConfig :: RefConfig)

withRefConfig :: RefConfig -> (forall. HasRefConfig => a) -> a
withRefConfig tc act = let ?refConfig = tc in act

refConfig :: HasRefConfig => RefConfig
refConfig = ?refConfig

getNoTls :: HasRefConfig => Bool
getNoTls = tc_noTls refConfig
