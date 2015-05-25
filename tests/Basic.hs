{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}
module Main where

import Data.ByteString (pack)
import Control.Monad
import Control.Exception
import Control.Concurrent.Async
import System.Socket
import System.Exit

main :: IO ()
main = do 
  test "test0001.01" $ test0001 (undefined :: Socket AF_INET  SOCK_STREAM IPPROTO_TCP) localhost
  test "test0001.02" $ test0001 (undefined :: Socket AF_INET6 SOCK_STREAM IPPROTO_TCP) localhost6

-- Test connection oriented sockets (i.e. TCP).
test0001 :: (AddressFamily f, Type t, Protocol p) => Socket f t p -> SockAddr f -> IO (Either String String)
test0001 dummy addr = do
  server <- socket `asTypeOf` return dummy
  bind server addr
  listen server 5
  serverRecv <- async $ do
    (peerSock, peerAddr) <- accept server
    recv peerSock 4096
  client <- socket `asTypeOf` return server
  connect client addr
  send client helloWorld
  msg <- wait serverRecv
  close server
  close client

  if (msg /= helloWorld)
    then return (Left  "Received message was bogus.")
    else return (Right "")

  where
    helloWorld = "Hello world!"


localhost :: SockAddrIn
localhost =
  SockAddrIn
  { sinPort      = 7777
  , sinAddr      = pack [127,0,0,1]
  }

localhost6 :: SockAddrIn6
localhost6 =
  SockAddrIn6
  { sin6Port     = 7777
  , sin6Addr     = pack [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,1]
  , sin6Flowinfo = 0
  , sin6ScopeId  = 0
  }

test :: String -> IO (Either String String) -> IO ()
test n t = do
  putStr ("Test " ++ show n ++ ": ")
  catch
    ( do  r <- t
          case r of
            Left  x -> putStr "FAIL " >> putStrLn x >> exitFailure
            Right x -> putStr "OK   " >> putStrLn x
    )
    (\e-> putStr "EXCP " >> putStrLn (show (e :: SomeException)) >> exitFailure)