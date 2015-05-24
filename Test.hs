{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Socket
import Control.Concurrent

main :: IO ()
main = do
  s <- socket :: IO (Socket AF_INET6 SOCK_STREAM IPPROTO_TCP)
  bind s localhost
  forkIO $ do
    send s "aaa"
    send s "sss"
    return ()
  forkIO $ do
    send s "bbb"
    send s "ttt"
    return ()
  listen s 5
  threadDelay 10000000
  close s
  threadDelay 10000000
