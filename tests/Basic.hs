{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}
module Main where

import Data.Monoid
import Data.ByteString (pack)
import qualified Data.ByteString.Lazy as LBS
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Foreign.C.Error
import System.Socket
import System.Socket.Address.SockAddrIn
import System.Exit

main :: IO ()
main = do 
  test "test0001.01" $ test0001 (undefined :: Socket SockAddrIn  STREAM TCP)  localhost
  test "test0001.02" $ test0001 (undefined :: Socket SockAddrIn6 STREAM TCP)  localhost6
  test "test0001.03" $ test0001 (undefined :: Socket SockAddrIn  STREAM SCTP) localhost
  test "test0001.04" $ test0001 (undefined :: Socket SockAddrIn6 STREAM SCTP) localhost6
  test "test0002.01" $ test0002 (undefined :: Socket SockAddrIn  DGRAM  UDP)  localhost
  test "test0002.02" $ test0002 (undefined :: Socket SockAddrIn6 DGRAM  UDP)  localhost6
  test "test0003.01" $ test0003 (undefined :: Socket SockAddrIn  STREAM TCP)  localhost
  test "test0003.02" $ test0003 (undefined :: Socket SockAddrIn6 STREAM TCP)  localhost6
  test "test0003.03" $ test0003 (undefined :: Socket SockAddrIn  STREAM SCTP) localhost
  test "test0003.04" $ test0003 (undefined :: Socket SockAddrIn6 STREAM SCTP) localhost6
  test "test0004.01" $ test0004 (undefined :: Socket SockAddrIn  STREAM SCTP) localhost
  test "test0004.02" $ test0004 (undefined :: Socket SockAddrIn6 STREAM SCTP) localhost6

-- Test send and receive on connection oriented sockets (i.e. TCP).
test0001 :: (Address a, Type t, Protocol p) => Socket a t p -> a -> IO (Either String String)
test0001 dummy addr =
  handleJust
    (\(SocketException e)-> if e == ePROTONOSUPPORT then Just () else Nothing)
    (const $ return (Right "Protocol is not supported, but that may happen."))
    $ bracket
      ( do  server <- socket `asTypeOf` return dummy
            client <- socket `asTypeOf` return dummy
            return (server, client)
      )
      (\(server,client)-> do
            close server
            close client
      )
      (\(server,client)-> do
            setSockOpt server (SO_REUSEADDR True)
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
      )
  where
    helloWorld = "Hello world!"

-- Test stateless sockets (i.e. UDP).
test0002 :: (Address a, Type t, Protocol p) => Socket a t p -> a -> IO (Either String String)
test0002 dummy addr =
  handleJust
    (\(SocketException e)-> if e == ePROTONOSUPPORT then Just () else Nothing)
    (const $ return (Right "Protocol is not supported, but that may happen."))
    $ bracket
      ( do  server <- socket `asTypeOf` return dummy
            client <- socket `asTypeOf` return dummy
            return (server, client)
      )
      (\(server,client)-> do
            close server
            close client
      )
      (\(server,client)-> do
            setSockOpt server (SO_REUSEADDR True)
            bind server addr
            serverRecv <- async $ do
              recvFrom server 4096 mempty
            client <- socket `asTypeOf` return server
            sendTo client helloWorld mempty addr
            (msg,peerAddr) <- wait serverRecv
            if (msg /= helloWorld)
              then return (Left  "Received message was bogus.")
              else return (Right "")
      )
  where
    helloWorld = "Hello world!"

-- Test sendMsg and recvMsg (TODO!) on connection oriented sockets (i.e. TCP).
test0003 :: (Address a, Type t, Protocol p) => Socket a t p -> a -> IO (Either String String)
test0003 dummy addr =
  handleJust
    (\(SocketException e)-> if e == ePROTONOSUPPORT then Just () else Nothing)
    (const $ return (Right "Protocol is not supported, but that may happen."))
    $ bracket
        ( do  server <- socket `asTypeOf` return dummy
              client <- socket `asTypeOf` return dummy
              return (server, client)
        )
        (\(server,client)-> do
              close server
              close client
        )
        (\(server,client)-> do
              setSockOpt server (SO_REUSEADDR True)
              bind server addr
              listen server 5
              serverRecv <- async $ do
                (peerSock, peerAddr) <- accept server
                recvMsg peerSock 4096 mempty
              client <- socket `asTypeOf` return server
              connect client addr
              sendMsg client helloWorld mempty
              (msg, flags) <- wait serverRecv
              if (msg /= helloWorld)
                then return (Left  "Received message was bogus.")
                else return (Right "")
        )
  where
    helloWorld = LBS.fromChunks ["Hello world!", "All your base are belong to us!"]


-- Test sendMsg and recvMsg wrt. fragmentation and message boundaries (only supported by SCTP)
test0004 :: Address a => Socket a STREAM SCTP -> a -> IO (Either String String)
test0004 dummy addr =
  handleJust
    (\(SocketException e)-> if e == ePROTONOSUPPORT then Just () else Nothing)
    (const $ return (Right "Protocol is not supported, but that may happen."))
    $ bracket
        ( do  server <- socket `asTypeOf` return dummy
              client <- socket `asTypeOf` return dummy
              return (server, client)
        )
        (\(server,client)-> do
              close server                            `onException` print "E01"
              close client                            `onException` print "E02"
        )
        (\(server,client)-> do
              setSockOpt server (SO_REUSEADDR True)   `onException` print "E03"
              bind server addr                        `onException` print "E04"
              listen server 5                         `onException` print "E05"
              serverRecv <- async $ do
                (peerSock, peerAddr) <- accept server `onException` print "E06"
                -- note the extremely small buffers!
                r1 <- recvMsg peerSock    4  mempty   `onException` print "E07"
                r2 <- recvMsg peerSock 1024  mempty   `onException` print "E08"
                r3 <- recvMsg peerSock   32  mempty   `onException` print "E09"
                r4 <- recvMsg peerSock 1024  mempty   `onException` print "E10"
                return (r1, r2, r3, r4)
              client <- socket `asTypeOf` return server
              connect client addr                     `onException` print "E11"
              sendMsg client msg1 mempty              `onException` print "E12"
              sendMsg client msg2 mempty              `onException` print "E13"
              close client                            `onException` print "E14"
              ((m1,f1), (m2,f2), (m3,f3), (m4,f4)) <- wait serverRecv
              when (m1 /= LBS.take 4 msg1)  (error "First message should start with 'Hello'.")
              when (f1 /= mempty)           (error "First message should not have flags set.")
              when (m2 /= LBS.drop 4 msg1)  (error "Second message should contain the rest of msg1.")
              when (f2 /= msgEOR)           (error "Second message should terminate the record.")
              when (m3 /= LBS.take 32 msg2) (error "First message should start with 32 chars of msg2.")
              when (f3 /= mempty)           (error "First message should not have flags set.")
              when (m4 /= LBS.drop 32 msg2) (error "Second message should contain the rest of msg2.")
              when (f4 /= msgEOR)           (error "Second message should terminate the record.")
              return (Right "")
        )
  where
    -- this message is 43 characters long
    msg1 = LBS.fromChunks ["Hello world!", "All your base are belong to us!"]
    -- this one is 38 characters long
    msg2 = LBS.fromChunks ["Uns gefaellt das.", "Viel Spass am Geraet!"]

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