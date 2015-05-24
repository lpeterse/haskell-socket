{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import System.Socket
import Control.Concurrent
import Control.Exception
import Control.Monad

localhost :: SockAddrIn
localhost =
  SockAddrIn
  { sinPort     = 1025
  , sinAddr     = BS.pack [127,0,0,1]
  }

localhost6 :: SockAddrIn6
localhost6 =
  SockAddrIn6
  { sin6Port     = 1025
  , sin6Flowinfo = 0
  , sin6ScopeId  = 0
  , sin6Addr     = BS.pack [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,1]
  }

main :: IO ()
main = do
  forkIO $ do
    server `catch` (\e-> putStrLn $ "server" ++ show (e :: SomeException))

  threadDelay 100000

  forkIO $ do
    client `catch` (\e-> putStrLn $ "client" ++ show (e :: SomeException))

  threadDelay 1000000000

server :: IO ()
server = do
  s <- socket :: IO (Socket AF_INET6 SOCK_STREAM IPPROTO_TCP)
  bind s localhost6
  listen s 5
  forever $ do
    (p, addr) <- accept s
    print addr
    msg <- recv p 4096
    print (msg, addr)

client :: IO ()
client = do
  s <- socket :: IO (Socket AF_INET6 SOCK_STREAM IPPROTO_TCP)
  connect s localhost6
  print "client connected"
  threadDelay 100000
  i <- send s "foobarnerd"
  print "client sent"
  print i


