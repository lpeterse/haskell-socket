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

-- | This tries to send and receive an extremely huge message (currently 128MB).
main :: IO ()
main =
  bracket
      ( do  server <- socket `onException` print "E01" :: IO (Socket Inet Stream TCP)
            client <- socket `onException` print "E02" :: IO (Socket Inet Stream TCP)
            return (server, client)
      )
      (\(server,client)-> do
            close server                              `onException` print "E03"
            close client                              `onException` print "E04"
      )
      (\(server,client)-> do
            bind server addr                          `onException` print "E06"
            listen server 5                           `onException` print "E07"
            serverRecv <- async $ do
              (peerSock, peerAddr) <- accept server   `onException` print "E08"
              receiveAll peerSock msgSize mempty         `onException` print "E09"

            threadDelay 100000
            connect client addr                       `onException` print "E11"
            sendAll client msg mempty                 `onException` print "E12"
            close client

            msgReceived <- wait serverRecv            `onException` print "E13"
            when (msgReceived /= msg) $                             error "E14"
      )
  where
    msgSize       = 128*1024*1024 + 1 :: Int64
    msg           = LBS.replicate msgSize 23
    addr          = InetAddress Inet.loopback 7777
