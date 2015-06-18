module System.Socket.Unsafe (
  -- * tryWaitAndRetry
    tryWaitAndRetry
  -- * unsafeSend
  , unsafeSend
  -- * unsafeSendTo
  , unsafeSendTo
  -- * unsafeReceive
  , unsafeReceive
  -- * unsafeReceiveFrom
  , unsafeReceiveFrom
  ) where

import Data.Function
import Data.Monoid

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS

import Control.Monad
import Control.Exception
import Control.Concurrent.MVar

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

import System.Socket.Internal.Socket
import System.Socket.Internal.Platform
import System.Socket.Internal.Exception
import System.Socket.Internal.Msg
import System.Socket.Internal.Platform
import System.Socket.Family

import System.Posix.Types (Fd)

#include "hs_socket.h"

unsafeSend :: Socket a t p -> Ptr a -> CSize -> MsgFlags -> IO CInt
unsafeSend s bufPtr bufSize flags = do
  tryWaitAndRetry s socketWaitWrite' (\fd-> c_send fd bufPtr bufSize (flags `mappend` msgNOSIGNAL) )

unsafeSendTo :: Socket f t p -> Ptr b -> CSize -> MsgFlags -> Ptr (SocketAddress f) -> CInt -> IO CInt
unsafeSendTo s bufPtr bufSize flags addrPtr addrSize = do
  tryWaitAndRetry s socketWaitWrite' (\fd-> c_sendto fd bufPtr (fromIntegral bufSize) (flags `mappend` msgNOSIGNAL) addrPtr addrSize)

unsafeReceive :: Socket a t p -> Ptr b -> CSize -> MsgFlags -> IO CInt
unsafeReceive s bufPtr bufSize flags =
  tryWaitAndRetry s socketWaitRead' (\fd-> c_recv fd bufPtr bufSize flags)

unsafeReceiveFrom :: Socket f t p -> Ptr b -> CSize -> MsgFlags -> Ptr (SocketAddress f) -> Ptr CInt -> IO CInt
unsafeReceiveFrom s bufPtr bufSize flags addrPtr addrSizePtr = do
  tryWaitAndRetry s socketWaitRead' (\fd-> c_recvfrom fd bufPtr bufSize flags addrPtr addrSizePtr)

tryWaitAndRetry :: Socket f t p -> (Fd -> Int-> IO (IO ())) -> (Fd -> IO CInt) -> IO CInt
tryWaitAndRetry (Socket mfd) getWaitAction action = loop 0
  where
    loop iteration = do
      ewr <- withMVar mfd $ \fd-> do
          when (fd < 0) $ do
            throwIO eBADF
          fix $ \retry-> do
            i <- action fd
            if (i < 0) then do
              e <- c_get_last_socket_error
              if e == eWOULDBLOCK || e == eAGAIN then do
                getWaitAction fd iteration >>= return . Left
              else if e == eINTR
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