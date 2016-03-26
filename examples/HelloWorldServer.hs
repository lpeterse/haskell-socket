{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception ( finally )
import Control.Monad ( forever )

import System.Socket
import System.Socket.Family.Inet6 ( Inet6, SocketAddress (..), inet6Any, V6Only (..) )
import System.Socket.Type.Stream ( Stream, sendAll )
import System.Socket.Protocol.TCP ( TCP )

main :: IO ()
main = do
  listeningSocket <- socket :: IO (Socket Inet6 Stream TCP)
  setSocketOption listeningSocket (ReuseAddress True)
  setSocketOption listeningSocket (V6Only False)
  bind listeningSocket (SocketAddressInet6 inet6Any 8080 0 0)
  listen listeningSocket 5
  forever $ do
    (peerSocket, peerAddress) <- accept listeningSocket
    print peerAddress
    sendAll peerSocket "Hello world!" msgNoSignal `finally` close peerSocket
