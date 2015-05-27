{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}
module Main where

import Data.Monoid
import Data.ByteString (pack)
import Control.Monad
import Control.Exception
import Control.Concurrent.Async
import Foreign.C.Error
import System.Socket
import System.Exit

main :: IO ()
main = do 
  test "test0001.01" $ test0001 (undefined :: Socket SockAddrIn  STREAM TCP) localhost
  test "test0001.02" $ test0001 (undefined :: Socket SockAddrIn6 STREAM TCP) localhost6
  test "test0001.03" $ test0001 (undefined :: Socket SockAddrIn  STREAM SCTP) localhost
  test "test0001.04" $ test0001 (undefined :: Socket SockAddrIn6 STREAM SCTP) localhost6
  test "test0002.01" $ test0002 (undefined :: Socket SockAddrIn  DGRAM  UDP) localhost
  test "test0002.02" $ test0002 (undefined :: Socket SockAddrIn6 DGRAM  UDP) localhost6

-- Test connection oriented sockets (i.e. TCP).
test0001 :: (Address a, Type t, Protocol p) => Socket a t p -> a -> IO (Either String String)
test0001 dummy addr = do
  eServer <- try (socket `asTypeOf` return dummy)
  case eServer of
    Left e@(SocketException er) -> if er == ePROTONOSUPPORT
                                     then return (Right "Protocol not supported. System dependant.")
                                     else throwIO e
    Right server -> do
      bind server addr
      listen server 5
      serverRecv <- async $ do
        (peerSock, peerAddr) <- accept server
        recv peerSock 4096 mempty
      client <- socket `asTypeOf` return server
      connect client addr
      send client helloWorld mempty
      msg <- wait serverRecv
      close server
      close client
      if (msg /= helloWorld)
        then return (Left  "Received message was bogus.")
        else return (Right "")
      where
        helloWorld = "Hello world!"

-- Test stateless sockets (i.e. UDP).
test0002 :: (Address a, Type t, Protocol p) => Socket a t p -> a -> IO (Either String String)
test0002 dummy addr = do
  server <- socket `asTypeOf` return dummy
  bind server addr
  serverRecv <- async $ do
    recvFrom server 4096 mempty
  client <- socket `asTypeOf` return server
  sendTo client helloWorld mempty addr
  (msg,peerAddr) <- wait serverRecv
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