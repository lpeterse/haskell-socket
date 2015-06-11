module Main where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async

import Data.Monoid
import qualified Data.ByteString as B

import System.Socket
import System.Socket.Family.INET

-- | This tries to send and receive an extremely huge message (currently 128MB).
main :: IO ()
main =
  bracket
      ( do  server <- socket `onException` print "E01" :: IO (Socket INET STREAM TCP)
            client <- socket `onException` print "E02" :: IO (Socket INET STREAM TCP)
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
              recvCount peerSock 0                    `onException` print "E09"

            threadDelay 100000
            connect client addr                       `onException` print "E11"
            sendAll client msg mempty                 `onException` print "E12"
            close client

            count <- wait serverRecv                  `onException` print "E13"
            when (count /= msgSize) $                               error "E14"
      )
  where
    msgSize       = 128*1024*1024
    msg           = B.replicate msgSize 23
    addr          = SockAddrIn 7777 inaddrLOOPBACK
    recvCount s i = do
      b <- recv s 4096 mempty
      if B.null b
        then return i
        else recvCount s $! i + B.length b
