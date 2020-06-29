--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket.Unsafe
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Unsafe (
    Socket (..)
  -- * Unsafe operations
  -- ** unsafeSend
  , unsafeSend
  -- ** unsafeSendTo
  , unsafeSendTo
  -- ** unsafeReceive
  , unsafeReceive
  -- ** unsafeReceiveFrom
  , unsafeReceiveFrom
  -- ** unsafeGetSocketOption
  , unsafeGetSocketOption
  -- ** unsafeSetSocketOption
  , unsafeSetSocketOption
  -- * Waiting for events
  -- ** waitRead
  , waitRead
  -- ** waitWrite
  , waitWrite
  -- ** waitConnected
  , waitConnected
  -- ** tryWaitRetryLoop
  , tryWaitRetryLoop
  ) where

import Control.Concurrent.MVar
import Control.Exception ( throwIO )
import Control.Monad
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import System.Posix.Types ( Fd(..) )
import System.Socket.Internal.Socket
import System.Socket.Internal.SocketOption
import System.Socket.Internal.Platform
import System.Socket.Internal.Exception
import System.Socket.Internal.Message

-- | Like `System.Socket.send`, but using `Ptr` and length instead of a `ByteString`.
--
--   This function is /unsafe/. @bufPtr@ must refer to a buffer which size is at least @bufSize@ bytes.
unsafeSend :: Socket a t p -> Ptr b -> CSize -> MessageFlags -> IO CInt
unsafeSend s bufPtr bufSize flags =
  tryWaitRetryLoop s waitWrite (\fd-> c_send fd bufPtr bufSize flags )

-- | Like `System.Socket.sendTo`, but using `Ptr` and length instead of a `ByteString`.
--
--   This function is /unsafe/. @bufPtr@ must refer to a buffer which size is at least @bufSize@ bytes.
unsafeSendTo :: Socket f t p -> Ptr b -> CSize -> MessageFlags -> Ptr (SocketAddress f) -> CInt -> IO CInt
unsafeSendTo s bufPtr bufSize flags addrPtr addrSize =
  tryWaitRetryLoop s waitWrite (\fd-> c_sendto fd bufPtr (fromIntegral bufSize) flags addrPtr addrSize)

-- | Like `System.Socket.receive`, but using `Ptr` and length instead of a `ByteString`.
--
--   This function is /unsafe/. @bufPtr@ must refer to a buffer which size is at least @bufSize@ bytes.
unsafeReceive :: Socket a t p -> Ptr b -> CSize -> MessageFlags -> IO CInt
unsafeReceive s bufPtr bufSize flags =
  tryWaitRetryLoop s waitRead (\fd-> c_recv fd bufPtr bufSize flags)

-- | Like `System.Socket.receiveFrom`, but using `Ptr` and length instead of a `ByteString`.
--
--   This function is /unsafe/. @bufPtr@ must refer to a buffer which size is at least @bufSize@ bytes.
unsafeReceiveFrom :: Socket f t p -> Ptr b -> CSize -> MessageFlags -> Ptr (SocketAddress f) -> Ptr CInt -> IO CInt
unsafeReceiveFrom s bufPtr bufSize flags addrPtr addrSizePtr =
  tryWaitRetryLoop s waitRead (\fd-> c_recvfrom fd bufPtr bufSize flags addrPtr addrSizePtr)

tryWaitRetryLoop :: Socket f t p -> (Socket f t p -> Int -> IO ()) -> (Fd -> Ptr CInt -> IO CInt) -> IO CInt
tryWaitRetryLoop s@(Socket mfd) wait action = loop 0
  where
    loop iteration = do
      -- acquire lock on socket
      mi <- withMVar mfd $ \fd-> alloca $ \errPtr-> do
          when (fd < 0) (throwIO eBadFileDescriptor)
          i <- action fd errPtr
          if i < 0 then do
            err <- SocketException <$> peek errPtr
            unless (err == eWouldBlock || err == eAgain || err == eInterrupted)
                   (throwIO err)
            return Nothing
          else
            -- The following is quite interesting for debugging:
            -- when (iteration /= 0) (print iteration)
            return (Just i)
      -- lock on socket is release here
      case mi of
        Just i ->
          return i
        Nothing -> do
          wait s iteration -- this call is (eventually) blocking
          loop $! iteration + 1 -- tail recursion

