module System.Socket.Unsafe (
  -- * unsafeSend
    unsafeSend
  -- * unsafeSendTo
  , unsafeSendTo
  -- * unsafeRecv
  , unsafeRecv
  -- * unsafeRecvFrom
  , unsafeRecvFrom
  ) where

import Data.Function
import Data.Monoid

import Control.Monad
import Control.Exception
import Control.Concurrent.MVar

import Foreign.C.Error
import Foreign.C.Types
import Foreign.Ptr

import System.Socket.Internal.Socket
import System.Socket.Internal.Event
import System.Socket.Internal.FFI
import System.Socket.Address
import System.Socket.Type
import System.Socket.Protocol

#include "sys/socket.h"

unsafeSend :: (Address a, Type t, Protocol  p) => Socket a t p -> Ptr b -> CSize -> MsgFlags -> IO CInt
unsafeSend (Socket mfd) bufPtr bufSize flags = do
  fix $ \again-> do
    ewb <- withMVar mfd $ \fd-> do
      when (fd < 0) $ do
        throwIO (SocketException eBADF)
      fix $ \retry-> do
        i <- c_send fd bufPtr bufSize (flags `mappend` msgNOSIGNAL)
        if (i < 0) then do
          e <- getErrno
          if e == eWOULDBLOCK || e == eAGAIN
            then threadWaitRead' fd >>= return . Left
          else if e == eINTR
            then retry
            else throwIO (SocketException e)
        -- Send succeeded. Return the bytes send.
        else return (Right i)
    case ewb of
      Left  wait          -> wait >> again
      Right bytesSent     -> return bytesSent

unsafeSendTo :: (Address a, Type t, Protocol  p) => Socket a t p -> Ptr b -> CSize -> MsgFlags -> Ptr a -> CInt -> IO CInt
unsafeSendTo (Socket mfd) bufPtr bufSize flags addrPtr addrSize = do
  fix $ \again-> do
    ewb <- withMVar mfd $ \fd-> do
      when (fd < 0) $ do
        throwIO (SocketException eBADF)
      fix $ \retry-> do
        i <- c_sendto fd bufPtr (fromIntegral bufSize) (flags `mappend` msgNOSIGNAL) addrPtr addrSize
        if (i < 0) then do
          e <- getErrno
          if e == eWOULDBLOCK || e == eAGAIN
            then threadWaitRead' fd >>= return . Left
          else if e == eINTR
            then retry
            else throwIO (SocketException e)
        -- Send succeeded. Return the bytes send.
        else return (Right i)
    case ewb of
      Left  wait          -> wait >> again
      Right bytesSent     -> return (fromIntegral bytesSent)

unsafeRecv :: (Address a, Type t, Protocol  p) => Socket a t p -> Ptr b -> CSize -> MsgFlags -> IO CInt
unsafeRecv (Socket mfd) bufPtr bufSize flags =
  fix $ \again-> do
    ewb <- withMVar mfd $ \fd-> do
        when (fd < 0) $ do
          throwIO (SocketException eBADF)
        fix $ \retry-> do
          i <- c_recv fd bufPtr bufSize flags
          if (i < 0) then do
            e <- getErrno
            if e == eWOULDBLOCK || e == eAGAIN then do
              threadWaitRead' fd >>= return . Left
            else if e == eINTR
              then retry
              else throwIO (SocketException e)
          else return (Right i)
    case ewb of
      Left  wait          -> wait >> again
      Right bytesReceived -> return bytesReceived

unsafeRecvFrom :: (Address a, Type t, Protocol  p) => Socket a t p -> Ptr b -> CSize -> MsgFlags -> Ptr a -> Ptr CInt -> IO CInt
unsafeRecvFrom (Socket mfd) bufPtr bufSize flags addrPtr addrSizePtr = do
  fix $ \again-> do
    ewb <- withMVar mfd $ \fd-> do
        when (fd < 0) $ do
          throwIO (SocketException eBADF)
        fix $ \retry-> do
          i <- c_recvfrom fd bufPtr bufSize flags addrPtr addrSizePtr
          if (i < 0) then do
            e <- getErrno
            if e == eWOULDBLOCK || e == eAGAIN then do
              threadWaitRead' fd >>= return . Left
            else if e == eINTR
              then retry
              else throwIO (SocketException e)
          else return (Right i)
    case ewb of
      Left  wait          -> wait >> again
      Right bytesReceived -> return (fromIntegral bytesReceived)
