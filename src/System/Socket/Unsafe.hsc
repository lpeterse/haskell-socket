module System.Socket.Unsafe (
  -- * unsafeSend
    unsafeSend
  ) where

import Data.Function

import Control.Monad
import Control.Exception
import Control.Concurrent.MVar

import Foreign.C.Error
import Foreign.Ptr

import System.Socket.Internal.Socket
import System.Socket.Internal.Event
import System.Socket.Internal.FFI
import System.Socket.Address
import System.Socket.Type
import System.Socket.Protocol

#include "sys/socket.h"

unsafeSend :: (Address a, Type t, Protocol  p) => Socket a t p -> Ptr b -> Int -> IO Int
unsafeSend (Socket mfd) ptr len = do
  fix $ \wait-> do
    threadWaitWriteMVar mfd
    bytesSend <- withMVar mfd $ \fd-> do
      when (fd < 0) $ do
        throwIO (SocketException eBADF)
      fix $ \retry-> do
        i <- c_send fd ptr (fromIntegral len) (#const MSG_NOSIGNAL)
        if (i < 0) then do
          e <- getErrno
          if e == eWOULDBLOCK || e == eAGAIN 
            then return i
          else if e == eINTR
            then retry
            else throwIO (SocketException e)
        -- Send succeeded. Return the bytes send.
        else return i
    -- We cannot loop from within the block above, because this would keep the MVar locked.
    if bytesSend < 0
      then wait
      else return (fromIntegral bytesSend)