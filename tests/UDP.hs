{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import System.Socket
import System.Socket.Family.INET
import System.Socket.Family.INET6
import System.Exit

main :: IO ()
main = do 
  test "INET"  (undefined :: Socket INET  DGRAM  UDP)  localhost
  test "INET6" (undefined :: Socket INET6 DGRAM  UDP)  localhost6

-- Test stateless sockets (i.e. UDP).
test :: (Family f, Type t, Protocol p) => String -> Socket f t p -> SockAddr f -> IO ()
test inet dummy addr = do 
  server <- socket `asTypeOf` return dummy                   `onException` p 1
  client <- socket `asTypeOf` return dummy                   `onException` p 2

  setSockOpt server (SO_REUSEADDR True)                      `onException` p 3
  bind server addr                                           `onException` p 4
  serverRecv <- async $ do
    recvFrom server 4096 mempty                              `onException` p 5
  sendTo client helloWorld mempty addr                       `onException` p 6
  (msg,peerAddr) <- wait serverRecv                          `onException` p 7
  when (msg /= helloWorld) $                                               e 8

  close client                                               `onException` p 10
  close server                                               `onException` p 11

  where
    helloWorld = "Hello world!"
    e i        = error (inet ++ ": " ++ show i)
    p i        = print (inet ++ ": " ++ show i)

localhost :: SockAddrIn
localhost =
  SockAddrIn
  { sinPort      = 7777
  , sinAddr      = inaddrLOOPBACK
  }

localhost6 :: SockAddrIn6
localhost6 =
  SockAddrIn6
  { sin6Port     = 7777
  , sin6Addr     = in6addrLOOPBACK
  , sin6Flowinfo = 0
  , sin6ScopeId  = 0
  }
