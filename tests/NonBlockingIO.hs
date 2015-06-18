{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}
module Main where

import Data.Bits
import Data.Monoid
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import System.Socket
import System.Socket.Family.Inet
import System.Exit

main :: IO ()
main = do 
  t0001

-- | This is to test interruptability of (blocking) calls like
--   accept. The implementation may either run the call "safe"
--   in another thread if it is really blocking or wait on events
--   in which case the control is at the RTS's IO manager.
--   In both cases this test should be able to cancel the accept
--   async and therefore terminate.
--   If the test hangs, this means the runtime system got sedated
--   possibly due to a blocking system call in a non-threaded
--   environment.
t0001 :: IO ()
t0001 = do
  s <- socket                             `onException` e 0 :: IO (Socket Inet Stream TCP)
  setSockOpt s (SO_REUSEADDR True)        `onException` e 1
  bind s (SocketAddressInet 8080 inaddrLOOPBACK) `onException` e 2
  listen s 5                              `onException` e 3
  a <- async (accept s)                   `onException` e 4
  threadDelay 1000000 -- make sure the async call really got enough time to start
  p <- poll a                             `onException` e 5
  case p of
    Just (Left ex) -> do
      throwIO ex                          `onException` e 6
    Just (Right _) -> do
      throwIO (AssertionFailed "")        `onException` e 7
    Nothing ->  do
      cancel a                            `onException` e 8
      return ()
  where
    e i = print ("t0001." ++ show i)
