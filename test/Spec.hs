{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll)
import DiscoRip.Client

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "DiscoRip.Client"
  [ testCase "reconnect loop backs off and dies after stop" $ do
      let config = ClientConfig "test-client" False True
      -- Start the client, it will enter workerLoop and try to connect
      handle@Handle{worker} <- start config (\_ -> pure ())

      -- Wait a short while so it hits some backoffs
      threadDelay 2000000 -- 2 seconds

      -- It should still be alive
      status1 <- poll worker
      case status1 of
        Nothing -> pure ()
        Just _ -> assertFailure "Worker should still be alive"

      -- Stop the client
      stop handle

      -- Wait for it to die
      threadDelay 1000000 -- 1 second

      -- It should be dead now
      status2 <- poll worker
      case status2 of
        Nothing -> assertFailure "Worker should be dead"
        Just _ -> pure ()
  ]
