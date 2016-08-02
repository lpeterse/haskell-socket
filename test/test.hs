{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception ( try, bracket, throwIO, catch )
import Control.Monad ( when, void )
import Prelude hiding ( head )
import Data.Maybe ( isJust )

import Test.Tasty
import Test.Tasty.HUnit

import System.Socket
import System.Socket.Family.Inet
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP

main :: IO ()
main  = defaultMain $ testGroup "Tests" [ group01, group02 ]

port :: InetPort
port  = 39000

group01 :: TestTree
group01 = testGroup "Inet4/TCP"
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
group02  = testGroup "getAddrInfo" [

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
