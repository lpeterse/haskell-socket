{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import System.Socket
import Control.Concurrent
import Control.Exception

localhost :: SockAddrIn
localhost =
  SockAddrIn
  { sinPort     = 80
  , sinAddr     = BS.pack [5,9,235,145]
  }

localhost3 :: SockAddrIn
localhost3 =
  SockAddrIn
  { sinPort     = 80
  , sinAddr     = BS.pack [127,0,0,1]
  }

localhost2 :: SockAddrIn
localhost2 =
  SockAddrIn
  { sinPort     = 443
  , sinAddr     = BS.pack [5,9,235,145]
  }

dropping :: SockAddrIn
dropping =
  SockAddrIn
  { sinPort     = 444
  , sinAddr     = BS.pack [5,9,235,145]
  }

main :: IO ()
main = do
  s <- socket :: IO (Socket AF_INET SOCK_STREAM IPPROTO_TCP)
  forkIO $ do
      threadDelay 1000000000
      print "close"
      close s
  --forkIO $ do 
  --  threadDelay 1000000
  --  print "connect2"
  --  connect s localhost2 `catch` (\e@(SomeException _)-> print e)
  --  print "connected2"
  forkIO $ do 
    threadDelay 2000000
    print "connect3"
    connect s localhost `catch` (\e@(SomeException _)-> print e)
  connect s dropping `catch` (\e@(SomeException _)-> print e)
  print "connected 1"
  threadDelay 10000000

  connect s localhost
  print "connected 2"
  threadDelay 1000000

  threadDelay 1000000000
