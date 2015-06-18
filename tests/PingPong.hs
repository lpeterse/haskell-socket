{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Bits
import Data.Monoid
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import System.Socket
import System.Socket.Family.Inet
import System.Exit

main :: IO ()
main = do 
  t0001

t0001 :: IO ()
t0001 = do
  server <- socket                        `onException` e 0 :: IO (Socket Inet Stream TCP)
  client <- socket                        `onException` e 1 :: IO (Socket Inet Stream TCP)
  setSockOpt server (SO_REUSEADDR True)   `onException` e 2
  bind server addr                        `onException` e 3
  listen server 5                         `onException` e 4
  connect client addr                     `onException` e 5
  (peer,_) <- accept server               `onException` e 6
  
  x <- async (loop client 0)
  y <- async (loop peer 0)

  send peer "Ping!" mempty

  threadDelay 2000000 -- let's see how much we get through in 2s
  cancel x
  cancel y

  i <- wait x
  print (show i ++ "/2s")
  when (i < 10000) (e 16)

  where
    addr = SocketAddressInet 8080 inaddrLOOPBACK
    e i  = print ("t0001." ++ show i)
    loop sock index = ( do
      ping <- receive sock 4096 mempty
      when (ping /= "Ping!") (e 14)
      send sock ping mempty
      loop sock (index + 1)
     ) `catch` (\ThreadKilled-> return index) `onException` e 15
