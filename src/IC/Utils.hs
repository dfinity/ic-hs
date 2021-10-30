{-# LANGUAGE LambdaCase #-}
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
