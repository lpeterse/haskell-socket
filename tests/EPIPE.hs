{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async

import Data.Int
import Data.Monoid
import qualified Data.ByteString.Lazy as LBS

import System.Socket
import System.Socket.Family.Inet as Inet
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP

main :: IO ()
main = do
  bracket
      ( do  server <- socket `onException` print "E01" :: IO (Socket Inet Stream TCP)
            client <- socket `onException` print "E02" :: IO (Socket Inet Stream TCP)
            return (server, client)
      )
      (\(server,client)-> do
            close server                                        `onException` print "E03"
            close client                                        `onException` print "E04"
      )
      (\(server,client)-> do
            bind server addr                                    `onException` print "E06"
            listen server 5                                     `onException` print "E07"

            connect client addr                                 `onException` print "E08"
            (peerSock, _) <- accept server                      `onException` print "E09"
            _ <- send client "This should be received." mempty  `onException` print "E10"
            _ <- receive peerSock 4096 mempty                   `onException` print "E11"
            close peerSock                                      `onException` print "E12"

            threadDelay 1000000

            e1 <- try $ send client "This might fail."  mempty  `onException` print "E13"
            case e1 of
              Right _ -> return ()
              Left  e -> if e == ePipe
                           then return ()
                           else throwIO e                       `onException` print "E14"

            threadDelay 1000000

            e2 <- try $ send client "This should fail." mempty  `onException` print "E15"
            case e2 of
              Right _ -> error "E16"
              Left e  -> if e == ePipe
                           then return ()
                           else throwIO e                       `onException` print "E17"
      )
  where
    addr          = SocketAddressInet Inet.loopback 7777
