{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Socket
import Control.Concurrent

main :: IO ()
main = do
  s <- socket :: IO (Socket AF_INET6 SOCK_STREAM IPPROTO_TCP)
  bind s localhost
  forkIO $ do
    send s "abc"
    return ()
  forkIO $ do
    send s "def"
    return ()
  listen s 5
  threadDelay 1000000000