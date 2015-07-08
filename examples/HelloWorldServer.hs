{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Socket
import System.Socket.Family.Inet as Inet
import Data.Monoid
import Data.ByteString
import Control.Monad
import Control.Concurrent
import Control.Exception

main :: IO ()
main = do
  s <- socket :: IO (Socket Inet Stream TCP)
  setSocketOption s (ReuseAddress True)
  bind s addr
  listen s 5
  forever $ do
    (peer,_) <- accept s
    forkIO $ do
      sendAll peer "Hello world!" mempty `finally` close peer
  where
    addr = SocketAddressInet Inet.loopback 8080