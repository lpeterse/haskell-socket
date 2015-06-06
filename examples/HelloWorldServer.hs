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
    print "accept"
    (peer,addr) <- accept s `onException` print "E01"
    print "accepted"
    print addr
    sendAll peer "Hello world!" mempty 
     `onException`
       ( do print "E02"
       )
     `finally` 
       ( do print "close"
            close peer
            print "closed"
       )
    print "end"
