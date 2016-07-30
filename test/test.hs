{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception ( try, bracket, throwIO )
import Prelude hiding (head)

import Test.Tasty
import Test.Tasty.HUnit

import System.Socket
import System.Socket.Family.Inet
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP

main :: IO ()
main  = defaultMain $ testGroup "System" [ testGroup "Socket" [tgSocket]]

tgSocket :: TestTree
tgSocket = testGroup "connect"
  [ testCase "connect to closed port on inetLoopback" $ bracket
      ( socket :: IO (Socket Inet Stream TCP))
      close
      ( \s-> do
          r <- try $ connect s (SocketAddressInet inetLoopback 39000)
          case r of
            Left e   | e == eConnectionRefused -> return ()
                     | otherwise               -> throwIO e
            Right () -> assertFailure "connection should have failed"
      )
  , testCase "connect to closed port on inetNone" $ bracket
      ( socket :: IO (Socket Inet Stream TCP))
      close
      ( \s-> do
          r <- try $ connect s (SocketAddressInet inetNone 64999)
          case r of
            Left e   | e == eNetworkUnreachable  -> return ()
                     | e == eAddressNotAvailable -> return ()
                     | otherwise                 -> throwIO e
            Right () -> assertFailure "connection should have failed"
      )
  ]
