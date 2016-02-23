{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where

import Data.Monoid
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Foreign.Storable
import System.Socket
import System.Socket.Family
import System.Socket.Family.Inet   as Inet
import System.Socket.Family.Inet6  as Inet6
import System.Exit

main :: IO ()
main = do
  test "Inet"  (undefined :: Socket Inet  Datagram  UDP)  localhost
  test "Inet6" (undefined :: Socket Inet6 Datagram  UDP)  localhost6

-- Test stateless sockets (i.e. UDP).
test :: (Family f, Type t, Protocol p, Storable (Address f)) => String -> Socket f t p -> Address f -> IO ()
test inet dummy addr = do
  server <- socket `asTypeOf` return dummy                   `onException` p 1
  client <- socket `asTypeOf` return dummy                   `onException` p 2

  bind server addr                                           `onException` p 4

  ((msg,peeraddr),_) <- concurrently
   ( do
      receiveFrom server 4096 mempty                            `onException` p 5
   )
   ( do
      -- This is a race condition:
      --   The server must listen before the client sends his msg or the packt goes
      --   to nirvana. Still, a second here should be enough. If not, there's
      --   something wrong worth investigating.
      threadDelay 1000000
      sendTo client helloWorld mempty addr                   `onException` p 6
   )

  when (msg /= helloWorld) $                                               e 8

  close client                                               `onException` p 10
  close server                                               `onException` p 11

  where
    helloWorld = "Hello world!"
    e i        = error (inet ++ ": " ++ show i)
    p i        = print (inet ++ ": " ++ show i)

localhost :: Address Inet
localhost =
  InetAddress
  { Inet.port      = 7777
  , Inet.address   = Inet.loopback
  }

localhost6 :: Address Inet6
localhost6 =
  Inet6Address
  { Inet6.port     = 7777
  , Inet6.address  = Inet6.loopback
  , Inet6.flowInfo = mempty
  , Inet6.scopeId  = 0
  }
