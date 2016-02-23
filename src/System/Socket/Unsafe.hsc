module System.Socket.Unsafe (
  -- * unsafeSend
    unsafeSend
  -- * unsafeSendTo
  , unsafeSendTo
  -- * unsafeReceive
  , unsafeReceive
  -- * unsafeReceiveFrom
  , unsafeReceiveFrom
  -- * Socket Options
  -- ** unsafeGetSocketOption
  , unsafeGetSocketOption
  -- ** unsafeSetSocketOption
  , unsafeSetSocketOption
  -- * Waiting For Events
  -- ** unsafeSocketWaitRead
  , unsafeSocketWaitRead
  -- ** unsafeSocketWaitWrite
  , unsafeSocketWaitWrite
  -- * Other Helpers
  -- ** tryWaitRetryLoop
  , tryWaitRetryLoop
  ) where

import Data.Function

import Control.Monad
import Control.Exception
import Control.Concurrent.MVar

import Foreign.C.Types
import Foreign.Ptr

import System.Socket.Internal.Socket
import System.Socket.Internal.Platform
import System.Socket.Internal.Exception
import System.Socket.Internal.Message
import System.Socket.Family

import System.Posix.Types (Fd)

#include "hs_socket.h"

unsafeSend :: Socket a t p -> Ptr a -> CSize -> MessageFlags -> IO CInt
unsafeSend s bufPtr bufSize flags = do
  tryWaitRetryLoop s unsafeSocketWaitWrite (\fd-> c_send fd bufPtr bufSize flags )

unsafeSendTo :: Socket f t p -> Ptr b -> CSize -> MessageFlags -> Ptr (Address f) -> CInt -> IO CInt
unsafeSendTo s bufPtr bufSize flags addrPtr addrSize = do
  tryWaitRetryLoop s unsafeSocketWaitWrite (\fd-> c_sendto fd bufPtr (fromIntegral bufSize) flags addrPtr addrSize)

unsafeReceive :: Socket a t p -> Ptr b -> CSize -> MessageFlags -> IO CInt
unsafeReceive s bufPtr bufSize flags =
  tryWaitRetryLoop s unsafeSocketWaitRead (\fd-> c_recv fd bufPtr bufSize flags)

unsafeReceiveFrom :: Socket f t p -> Ptr b -> CSize -> MessageFlags -> Ptr (Address f) -> Ptr CInt -> IO CInt
unsafeReceiveFrom s bufPtr bufSize flags addrPtr addrSizePtr = do
  tryWaitRetryLoop s unsafeSocketWaitRead (\fd-> c_recvfrom fd bufPtr bufSize flags addrPtr addrSizePtr)

tryWaitRetryLoop :: Socket f t p -> (Fd -> Int-> IO (IO ())) -> (Fd -> IO CInt) -> IO CInt
tryWaitRetryLoop (Socket mfd) getWaitAction action = loop 0
  where
    loop iteration = do
      ewr <- withMVar mfd $ \fd-> do
          when (fd < 0) $ do
            throwIO eBadFileDescriptor
          fix $ \retry-> do
            i <- action fd
            if (i < 0) then do
              e <- c_get_last_socket_error
              if e == eWouldBlock || e == eAgain then do
                getWaitAction fd iteration >>= return . Left
              else if e == eInterrupted
                then retry
                else throwIO e
            else do
              -- The following is quite interesting for debugging:
              -- when (iteration /= 0) (print iteration)
              return (Right i)
      case ewr of
        Left  wait   -> do
          wait
          loop $! iteration + 1
        Right result -> do
          return result
