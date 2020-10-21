{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module IC.Test.DelayedIO where

import Control.Concurrent
import Control.Exception


-- | `delayed act` returns an IO action that, upon first execution executes
-- `act`, and caches the result (including a possible exception) for all
-- further executions, in a thread-safe way
delayed :: IO a -> IO (IO a)
delayed act = do
  ref <- newMVar Nothing
  return $ do
    eoa <- modifyMVar ref $ \case
      Nothing -> do
        x <- try @SomeException act
        return (Just x, Just x)
      Just x -> return (Just x, Just x)
    case eoa of
      Just (Left e) -> throwIO e
      Just (Right x) -> return x
      Nothing -> error "delayed: inconsinstent value in ref"


