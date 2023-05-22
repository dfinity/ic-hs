{- |

This module contains a test suite for the Internet Computer

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module IC.Test.Spec.Timer (canister_timer_tests) where

import Data.ByteString.Builder
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad
import Data.Row as R
import Data.Time.Clock.POSIX
import Codec.Candid (Principal(..))
import qualified Codec.Candid as Candid
import Control.Concurrent
import System.Timeout

import IC.Management (InstallMode)
import IC.Test.Universal
import IC.Test.Agent
import IC.Test.Agent.UnsafeCalls
import IC.Test.Spec.Utils

-- * Helpers

waitFor :: HasAgentConfig => IO Bool -> IO ()
waitFor act = do
    result <- timeout (tc_timeout agentConfig * (10::Int) ^ (6::Int)) doActUntil
    when (result == Nothing) $ assertFailure "Polling timed out"
  where
    doActUntil = do
      stop <- act
      unless stop (threadDelay 1000 *> doActUntil)

-- * The test suite (see below for helper functions)

canister_timer_tests :: HasAgentConfig => Blob -> [TestTree]
canister_timer_tests ecid =
    let on_timer_prog n = onGlobalTimer $ callback ((ignore $ stableGrow $ int 1) >>> (stableWrite (int 0) $ i64tob $ int64 $ fromIntegral n)) in
    let set_timer_prog time = ((ignore $ stableGrow $ int 1) >>> (stableWrite (int 0) $ i64tob $ apiGlobalTimerSet $ int64 time)) in
    let install_canister_with_global_timer n = install ecid $ on_timer_prog n in
    let reset_stable cid = call cid ((ignore $ stableGrow $ int 1) >>> (stableWrite (int 0) $ i64tob $ int64 42) >>> replyData "") in
    let get_stable cid = call cid (replyData $ stableRead (int 0) (int 8)) in
    let get_far_past_time = return 1 in
    let get_current_time = floor . (* 1e9) <$> getPOSIXTime in
    let get_far_future_time = floor . (* 1e9) <$> (+) 100000 <$> getPOSIXTime in
    let get_far_far_future_time = floor . (* 1e9) <$> (+) 1000000 <$> getPOSIXTime in
    let set_timer cid time = call cid (replyData $ i64tob $ apiGlobalTimerSet $ int64 time) in
    let blob = toLazyByteString . word64LE . fromIntegral in
    let wait_for_timer cid n = waitFor $ (blob n ==) <$> get_stable cid in
    [ testCase "in update" $ do
      cid <- install_canister_with_global_timer (2::Int)
      _ <- reset_stable cid
      far_future_time <- get_far_future_time
      timer1 <- set_timer cid far_future_time
      timer2 <- set_timer cid far_future_time
      ctr <- get_stable cid
      timer1 @?= blob 0
      timer2 @?= blob far_future_time
      ctr @?= blob 42
    , testCase "in init" $ do
      far_future_time <- get_far_future_time
      cid <- install ecid $ on_timer_prog (2::Int) >>> set_timer_prog far_future_time
      timer1 <- get_stable cid
      timer2 <- set_timer cid far_future_time
      timer1 @?= blob 0
      timer2 @?= blob far_future_time
    , testCase "in pre-upgrade" $ do
      far_past_time <- get_far_past_time
      far_future_time <- get_far_future_time
      cid <- install ecid $ (on_timer_prog (2::Int) >>> onPreUpgrade (callback $ set_timer_prog far_past_time))
      _ <- reset_stable cid
      universal_wasm <- getTestWasm "universal-canister"
      _ <- ic_install ic00 (enum #upgrade) cid universal_wasm (run noop)
      timer1 <- get_stable cid
      timer2 <- set_timer cid far_future_time
      timer1 @?= blob 0
      timer2 @?= blob 0
    , testCase "in post-upgrade" $ do
      cid <- install_canister_with_global_timer (2::Int)
      _ <- reset_stable cid
      far_future_time <- get_far_future_time
      timer1 <- set_timer cid far_future_time
      far_far_future_time <- get_far_far_future_time
      universal_wasm <- getTestWasm "universal-canister"
      _ <- ic_install ic00 (enum #upgrade) cid universal_wasm (run $ set_timer_prog far_far_future_time)
      timer2 <- get_stable cid
      timer3 <- set_timer cid far_future_time
      timer1 @?= blob 0
      timer2 @?= blob 0
      timer3 @?= blob far_far_future_time
    , testCase "in post-upgrade on stopped canister" $ do
      cid <- install_canister_with_global_timer (2::Int)
      _ <- reset_stable cid
      far_future_time <- get_far_future_time
      timer1 <- set_timer cid far_future_time
      past_time <- get_far_past_time
      universal_wasm <- getTestWasm "universal-canister"
      _ <- ic_stop_canister ic00 cid
      waitFor $ do
        cs <- ic_canister_status ic00 cid
        return $ cs .! #status == enum #stopped
      _ <- ic_install ic00 (enum #upgrade) cid universal_wasm (run $ on_timer_prog (2::Int) >>> set_timer_prog past_time)
      _ <- ic_start_canister ic00 cid
      wait_for_timer cid 2
      timer2 <- set_timer cid far_future_time
      timer1 @?= blob 0
      timer2 @?= blob 0
    , testCase "in post-upgrade on stopping canister" $ do
      cid <- install_canister_with_global_timer (2::Int)
      _ <- reset_stable cid
      far_future_time <- get_far_future_time
      timer1 <- set_timer cid far_future_time
      cid2 <- install ecid noop
      ic_set_controllers ic00 cid [defaultUser, cid2]
      universal_wasm <- getTestWasm "universal-canister"
      past_time <- get_far_past_time
      let upgrade = update_call "" "install_code" $ defUpdateArgs {
              uc_arg = Candid.encode $ empty
                .+ #mode .== ((enum #upgrade) :: InstallMode)
                .+ #canister_id .== Principal cid
                .+ #wasm_module .== universal_wasm
                .+ #arg .== (run $ on_timer_prog (2::Int) >>> set_timer_prog past_time)
            }
      let stop_and_upgrade = (oneway_call "" "stop_canister" $ defOneWayArgs {
              ow_arg = Candid.encode $ empty .+ #canister_id .== Principal cid
            }) >>> upgrade
      let relay = oneway_call cid2 "update" $ defOneWayArgs {
              ow_arg = run stop_and_upgrade
            }
      call' cid relay >>= isReject [5] -- we get an error here because, to keep the canister stopping, we cannot reply after performing the one-way call
      ic_start_canister ic00 cid
      wait_for_timer cid 2
      timer2 <- set_timer cid far_future_time
      timer1 @?= blob 0
      timer2 @?= blob 0
    , testCase "in timer callback" $ do
      past_time <- get_far_past_time
      far_future_time <- get_far_future_time
      cid <- install ecid $ onGlobalTimer $ callback $ set_timer_prog far_future_time -- the timer callback sets timer to far_future_time and stores the previous value of timer to stable memory
      _ <- reset_stable cid -- stores 42 to stable memory
      timer1 <- set_timer cid past_time -- sets timer to 1 and returns previous value of timer (0)
      wait_for_timer cid 0 -- wait until stable memory stores 0 (previous value of timer assigned to stable memory by the timer callback)
      timer2 <- set_timer cid far_future_time -- sets timer to far_future_time and returns previous value of timer (far_future_time set by the timer callback)
      timer1 @?= blob 0
      timer2 @?= blob far_future_time
    , testCase "deactivate timer" $ do
      cid <- install_canister_with_global_timer (2::Int)
      _ <- reset_stable cid
      far_future_time <- get_far_future_time
      timer1 <- set_timer cid far_future_time
      timer2 <- set_timer cid 0
      timer3 <- set_timer cid far_future_time
      ctr <- get_stable cid
      timer1 @?= blob 0
      timer2 @?= blob far_future_time
      timer3 @?= blob 0
      ctr @?= blob 42
    , testCase "set timer far in the past" $ do
      cid <- install_canister_with_global_timer (2::Int)
      _ <- reset_stable cid
      past_time <- get_far_past_time
      timer1 <- set_timer cid past_time
      wait_for_timer cid 2
      future_time <- get_far_future_time
      timer2 <- set_timer cid future_time
      timer1 @?= blob 0
      timer2 @?= blob 0
    , testCase "set timer at current time" $ do
      cid <- install_canister_with_global_timer (2::Int)
      _ <- reset_stable cid
      current_time <- get_current_time
      timer1 <- set_timer cid current_time
      wait_for_timer cid 2
      future_time <- get_far_future_time
      timer2 <- set_timer cid future_time
      timer1 @?= blob 0
      timer2 @?= blob 0
    , testCase "stop and start canister" $ do
      cid <- install_canister_with_global_timer (2::Int)
      _ <- reset_stable cid
      far_future_time <- get_far_future_time
      timer1 <- set_timer cid far_future_time
      timer2 <- set_timer cid far_future_time
      _ <- ic_stop_canister ic00 cid
      _ <- ic_start_canister ic00 cid
      timer3 <- set_timer cid far_future_time
      ctr <- get_stable cid
      timer1 @?= blob 0
      timer2 @?= blob far_future_time
      timer3 @?= blob far_future_time
      ctr @?= blob 42
    , testCase "uninstall and install canister" $ do
      cid <- install_canister_with_global_timer (2::Int)
      _ <- reset_stable cid
      far_future_time <- get_far_future_time
      timer1 <- set_timer cid far_future_time
      timer2 <- set_timer cid far_future_time
      universal_wasm <- getTestWasm "universal-canister"
      _ <- ic_uninstall ic00 cid
      _ <- ic_install ic00 (enum #install) cid universal_wasm (run $ on_timer_prog (2::Int))
      timer3 <- set_timer cid far_future_time
      timer1 @?= blob 0
      timer2 @?= blob far_future_time
      timer3 @?= blob 0
    , testCase "upgrade canister" $ do
      cid <- install_canister_with_global_timer (2::Int)
      _ <- reset_stable cid
      far_future_time <- get_far_future_time
      timer1 <- set_timer cid far_future_time
      timer2 <- set_timer cid far_future_time
      universal_wasm <- getTestWasm "universal-canister"
      _ <- ic_install ic00 (enum #upgrade) cid universal_wasm (run $ on_timer_prog (2::Int))
      timer3 <- set_timer cid far_future_time
      timer1 @?= blob 0
      timer2 @?= blob far_future_time
      timer3 @?= blob 0
    , testCase "reinstall canister" $ do
      cid <- install_canister_with_global_timer (2::Int)
      _ <- reset_stable cid
      far_future_time <- get_far_future_time
      timer1 <- set_timer cid far_future_time
      timer2 <- set_timer cid far_future_time
      universal_wasm <- getTestWasm "universal-canister"
      _ <- ic_install ic00 (enum #reinstall) cid universal_wasm (run $ on_timer_prog (2::Int))
      timer3 <- set_timer cid far_future_time
      timer1 @?= blob 0
      timer2 @?= blob far_future_time
      timer3 @?= blob 0
    ]
