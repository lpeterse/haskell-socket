{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Bits
import Data.Monoid
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import System.Socket
import System.Socket.Family.Inet  as Inet
import System.Socket.Family.Inet6 as Inet6
import System.Exit

main :: IO ()
main = do 
  t0001
  t0002

t0001 :: IO ()
t0001 = 
  bracket
    ( do
        server <- socket                              `onException` p 0 :: IO (Socket Inet6 Datagram UDP)
        client <- socket                              `onException` p 1 :: IO (Socket Inet  Datagram UDP)
        return (server, client)
    )
    (\(server,client)-> do
        close server                                  `onException` p 2
        close client                                  `onException` p 3
    )
    (\(server,client)-> do
        setSockOpt server (V6Only True)                     `onException` p 4
        bind server (SocketAddressInet6 7777 Inet6.any 0 0) `onException` p 5

        threadDelay 1000000 -- wait for the listening socket being set up
        sendTo client "PING" mempty (SocketAddressInet 7777 Inet.loopback)
                                                            `onException` p 6
        eith <- race
          ( receiveFrom server 4096 mempty `onException` p 7 >> return () )
          ( threadDelay 1000000 )
        case eith of
          Left  () -> e 8        -- we didn't expect receiving a msg
          Right () -> return ()  -- timeout is the expected behaviour
    )
  where
    e i  = error ("t0001." ++ show i)
    p i  = print ("t0001." ++ show i)

t0002 :: IO ()
t0002 = 
  bracket
    ( do
        server <- socket                              `onException` p 0 :: IO (Socket Inet6 Datagram UDP)
        client <- socket                              `onException` p 1 :: IO (Socket Inet  Datagram UDP)
        return (server, client)
    )
    (\(server,client)-> do
        close server                                  `onException` p 2
        close client                                  `onException` p 3
    )
    (\(server,client)-> do
        setSockOpt server (V6Only False)              `onException` p 4
        bind server (SocketAddressInet6 7778 Inet6.any 0 0) `onException` p 5

        threadDelay 1000000 -- wait for the listening socket being set up
        sendTo client "PING" mempty (SocketAddressInet 7778 Inet.loopback)
                                                      `onException` p 6
        eith <- race
          ( receiveFrom server 4096 mempty `onException` p 7 >> return ())
          ( threadDelay 1000000 )
        case eith of
          Left  () -> return ()  -- we received the expected msg
          Right () -> e 8        -- timeout occured
    )
  where
    e i  = error ("t0002." ++ show i)
    p i  = print ("t0002." ++ show i)
