{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( race )
import Control.Exception ( try, bracket, throwIO, catch )
import Control.Monad ( when, unless, void )
import Prelude hiding ( head )
import Data.Maybe ( isJust )

import Test.Tasty
import Test.Tasty.HUnit

import System.Socket
import System.Socket.Family.Inet
import System.Socket.Family.Inet6
import System.Socket.Type.Stream
import System.Socket.Type.Datagram
import System.Socket.Protocol.TCP
import System.Socket.Protocol.UDP

main :: IO ()
main  = defaultMain $ testGroup "Tests" [ group01, group02, group03, group80, group99 ]

port :: InetPort
port  = 39000

port6 :: Inet6Port
port6 = 39000

group01 :: TestTree
group01 = testGroup "connect" [ testGroup "Inet/Stream/TCP" t1 ]
  where
    t1 =
      [ testCase "connect to closed port on inetLoopback" $ bracket
          ( socket :: IO (Socket Inet Stream TCP))
          close
          ( \s-> do
              r <- try $ connect s (SocketAddressInet inetLoopback port)
              case r of
                Left e   | e == eConnectionRefused -> return ()
                         | otherwise               -> throwIO e
                Right () -> assertFailure "connection should have failed"
          )

      , testCase "connect to closed port on inetNone" $ bracket
          ( socket :: IO (Socket Inet Stream TCP))
          close
          ( \s-> do
              r <- try $ connect s (SocketAddressInet inetNone port)
              case r of
                Left e   | e == eNetworkUnreachable  -> return ()
                         | e == eAddressNotAvailable -> return ()
                         | otherwise                 -> throwIO e
                Right () -> assertFailure "connection should have failed"
          )

      , testCase "connect to open socket on localhost" $ bracket
          ( do  server <- socket :: IO (Socket Inet Stream TCP)
                client <- socket :: IO (Socket Inet Stream TCP)
                return (server, client)
          )
          (\(server,client)-> do
                close server
                close client
          )
          (\(server,client)-> do
                setSocketOption server (ReuseAddress True)
                bind server (SocketAddressInet inetLoopback port)
                listen server 5
                connect client (SocketAddressInet inetLoopback port)
          )

      , testCase "connect closed socket" $ bracket
          ( do  server <- socket :: IO (Socket Inet Stream TCP)
                client <- socket :: IO (Socket Inet Stream TCP)
                return (server, client)
          )
          (\(server,client)-> do
                close server
                close client
          )
          (\(server,client)-> do
                setSocketOption server (ReuseAddress True)
                bind server (SocketAddressInet inetLoopback port)
                listen server 5
                close client
                connect client (SocketAddressInet inetLoopback port)
          ) `catch` \e-> case e of
              _ | e == eBadFileDescriptor -> return ()
                | otherwise               -> assertFailure "expected eBadFileDescriptor"

      , testCase "connect already connected socket" $ bracket
          ( do  server <- socket :: IO (Socket Inet Stream TCP)
                client <- socket :: IO (Socket Inet Stream TCP)
                return (server, client)
          )
          (\(server,client)-> do
                close server
                close client
          )
          (\(server,client)-> do
                setSocketOption server (ReuseAddress True)
                bind server (SocketAddressInet inetLoopback port)
                listen server 5
                connect client (SocketAddressInet inetLoopback port)
                connect client (SocketAddressInet inetLoopback port)
                assertFailure "should have thrown eIsConnected"
          ) `catch` \e-> case e of
              _ | e == eIsConnected -> return ()
                | otherwise         -> assertFailure "expected eIsConnected"
      ]

group02 :: TestTree
group02  = testGroup "listen"
  [ testGroup "Inet/Datagram/UDP"
      [ testCase "listen on bound socket" $ bracket
          ( socket :: IO (Socket Inet Datagram UDP) ) close $ \sock-> do
              bind sock (SocketAddressInet inetLoopback port)
              setSocketOption sock (ReuseAddress True)
              listen sock 5 `catch` \e-> case e of
                _ | e == eOperationNotSupported -> return ()
                _                               -> assertFailure "expected eOperationNotSupported"
      ]
  ]

group03 :: TestTree
group03 = testGroup "send" [ testGroup "Inet/Stream/TCP"
    [ testCase "trigger ePipe exception" $ bracket
        ( do
            server <- socket :: IO (Socket Inet Stream TCP)
            client <- socket :: IO (Socket Inet Stream TCP)
            return (server, client)
        )
        ( \(server,client)-> do
          close server
          close client
        )
        ( \(server,client)-> do
          let addr = SocketAddressInet inetLoopback port
          bind server addr
          listen server 5
          connect client addr
          (peerSock, _) <- accept server
          _ <- send client "This should be received." mempty
          _ <- receive peerSock 4096 mempty
          close peerSock
          threadDelay 1000000
          e1 <- try $ send client "This might fail." mempty
          case e1 of
            Right _ -> return ()
            Left  e -> unless (e == ePipe) (throwIO e)
          threadDelay 1000000
          e2 <- try $ send client "This should fail." mempty
          case e2 of
            Right _ -> assertFailure "expected ePipe"
            Left e  -> unless (e == ePipe) (throwIO e)
        )
    ]
  ]

group80 :: TestTree
group80 = testGroup "setSocketOption" [ testGroup "V6Only"
    [ testCase "present" $ bracket
        ( do
          server <- socket :: IO (Socket Inet6 Datagram UDP)
          client <- socket :: IO (Socket Inet Datagram UDP)
          return (server, client)
        )
        ( \(server,client)-> do
          close server
          close client
        )
        ( \(server,client)-> do
          setSocketOption server (V6Only True)
          bind server (SocketAddressInet6 inet6Any port6 0 0)
          threadDelay 1000000 -- wait for the listening socket being set up
          sendTo client "PING" mempty (SocketAddressInet inetLoopback port)
          eith <- race
            ( void $ receiveFrom server 4096 mempty )
            ( threadDelay 1000000 )
          case eith of
            Left () -> assertFailure "expected timeout"
            Right () -> return ()  -- timeout is the expected behaviour
        )
    , testCase "absent" $ bracket
        ( do
          server <- socket :: IO (Socket Inet6 Datagram UDP)
          client <- socket :: IO (Socket Inet Datagram UDP)
          return (server, client)
        )
        ( \(server,client)-> do
          close server
          close client
        )
        ( \(server,client)-> do
          setSocketOption server (V6Only False)
          bind server (SocketAddressInet6 inet6Any port6 0 0)
          threadDelay 1000000 -- wait for the listening socket being set up
          sendTo client "PING" mempty (SocketAddressInet inetLoopback port)
          eith <- race
            ( void $ receiveFrom server 4096 mempty )
            ( threadDelay 1000000 )
          case eith of
            Left () -> return ()
            Right () -> assertFailure "expected packet"
        )
    ]
  ]

group99 :: TestTree
group99  = testGroup "getAddrInfo" [

    testCase "getAddrInfo \"127.0.0.1\" \"80\"" $ do
      ais <- getAddressInfo
              (Just "127.0.0.1")
              (Just "80")
              aiNumericHost :: IO [AddressInfo Inet Stream TCP]
      when (length ais /= 1) $ assertFailure "expected 1 result"
      let [ai] = ais
      when (isJust $ canonicalName ai) $ assertFailure "expected no canonical name"
      let sa = socketAddress ai
      when (inetAddress sa /= inetLoopback) $ assertFailure "expected loopback address"
      when (inetPort    sa /= 80) $ assertFailure "expected port 80"

  , testCase "getAddrInfo \"\" \"\"" $
      void (getAddressInfo Nothing Nothing mempty :: IO [AddressInfo Inet Stream TCP]) `catch` \e-> case e of
            _ | e == eaiNoName -> return ()
            _                  -> assertFailure "expected eaiNoName"

  ]