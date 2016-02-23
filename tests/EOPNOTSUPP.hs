{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async

import qualified Data.ByteString.Lazy as LBS

import System.Socket
import System.Socket.Family.Inet as Inet
import System.Socket.Type.Datagram
import System.Socket.Protocol.UDP

main :: IO ()
main = do
  bracket
      ( socket `onException` print "E01" :: IO (Socket Inet Datagram UDP)
      )
      (\server-> do
            close server                                        `onException` print "E03"
      )
      (\server-> do
            bind server addr                                    `onException` print "E06"
            e1 <- try (listen server 5)
            case e1 of
              Right _ -> error "E08"
              Left  e -> if e == eOperationNotSupported
                           then return ()
                           else throwIO e                       `onException` print "E09"
      )
  where
    addr          = SocketAddressInet Inet.loopback 7777
