{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Socket
import System.Socket.Family.INET (inaddrLOOPBACK)
import Data.ByteString
import Data.Monoid
import Control.Monad
import Control.Concurrent
import Control.Exception

main :: IO ()
main = do
  s <- socket :: IO (Socket INET STREAM TCP)
  setSockOpt s (SO_REUSEADDR True)
  bind s (SockAddrIn 8080 inaddrLOOPBACK)
  listen s 5
  forever $ do
    (peer,addr) <- accept s
    forkIO $ do
      sendAll peer "Hello world!" mempty `finally` close peer