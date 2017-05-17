--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket.Internal.Platform
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Internal.Platform where

import Control.Concurrent ( threadDelay )
import Control.Concurrent.MVar ( withMVar )
import Control.Exception ( throwIO )
import Control.Monad ( when )
import Data.Bits
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal
import System.Posix.Types ( Fd(..) )
import System.Socket.Internal.Socket
import System.Socket.Internal.Message
import System.Socket.Internal.Exception

-- | Wait until the socket becomes readable (Windows specific version).
--
--  This operation does not actually do anything with the socket. It just
--  calls `threadDelay` with a delay exponentially increasing in the second
--  parameter (0 => 1μs, 20 or greater => 1s). When the operation returns,
--  the socket shall be tried to read from - it does not imply that there is
--  actually data to read and it might be necessary to wait again!
--
--  On Windows, socket descriptors don't behave like file
--  descriptors, but require a specific select mechanism. Unfortunately,
--  this is not (yet) implemented in GHC's RTS and it cannot be called from here
--  as it would block.
--
--  In practice, this means that your application may suffer from reduced
--  responsibility (up to 1s in case the socket has not seen events for 20
--  cycles). Usually, this is only the case for the first of several sequential
--  reads from a socket. Subsequent reads will be executed without delay until
--  there is no more data to read, so it is still possible to achieve high
--  throughput. The central achievement of this approach is that all socket
--  operations offered by the library are interruptable and no special
--  considerations (apart the from one above) apply when running socket code on
--  Windows.
waitRead :: Socket f t p -> Int -> IO ()
waitRead  _ iteration =
  threadDelay $ 1 `shiftL` min iteration 20

-- | Wait until the socket becomes writable (Windows specific version).
--
--   See `waitRead` for technical details.
waitWrite :: Socket f t p -> Int -> IO ()
waitWrite _ iteration =
  threadDelay $ 1 `shiftL` min iteration 20

-- | Wait until the socket is confirmed to be connected (Windows specific version).
--
--   This operation uses an exponential-backoff algorithm as described in
--   `waitRead`. On each iteration `select` is called with a minmal timeout
--   to poll the connection status. This approach keeps the operation interruptable.
waitConnected :: Socket f t p -> IO ()
waitConnected (Socket mfd) =
  alloca $ \errPtr-> loop errPtr 0
  where
    loop errPtr iteration = do
      i <- withMVar mfd $ \fd-> do
        when (fd < 0) (throwIO eBadFileDescriptor)
        c_connect_status fd errPtr
      case i of
        0 -> return ()
        1 -> do
          -- Wait with exponential backoff.
          threadDelay $ 1 `shiftL` min iteration 20
          -- Try again.
          loop errPtr $! iteration + 1
        _ -> SocketException <$> peek errPtr >>= throwIO

type CSSize
   = CInt

foreign import ccall unsafe "hs_socket"
  c_socket  :: CInt -> CInt -> CInt -> Ptr CInt -> IO Fd

foreign import ccall unsafe "hs_close"
  c_close   :: Fd -> Ptr CInt -> IO CInt

foreign import ccall unsafe "hs_bind"
  c_bind    :: Fd -> Ptr a -> CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "hs_connect"
  c_connect :: Fd -> Ptr a -> CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "hs_connect_status"
  c_connect_status :: Fd -> Ptr CInt -> IO CInt

foreign import ccall unsafe "hs_accept"
  c_accept  :: Fd -> Ptr a -> Ptr CInt -> Ptr CInt -> IO Fd

foreign import ccall unsafe "hs_listen"
  c_listen  :: Fd -> CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "hs_send"
  c_send    :: Fd -> Ptr a -> CSize -> MessageFlags -> Ptr CInt -> IO CSSize

foreign import ccall unsafe "hs_sendto"
  c_sendto  :: Fd -> Ptr a -> CSize -> MessageFlags -> Ptr b -> CInt -> Ptr CInt -> IO CSSize

foreign import ccall unsafe "hs_recv"
  c_recv    :: Fd -> Ptr a -> CSize -> MessageFlags -> Ptr CInt -> IO CSSize

foreign import ccall unsafe "hs_recvfrom"
  c_recvfrom :: Fd -> Ptr a -> CSize -> MessageFlags -> Ptr b -> Ptr CInt -> Ptr CInt -> IO CSSize

foreign import ccall unsafe "hs_getsockopt"
  c_getsockopt  :: Fd -> CInt -> CInt -> Ptr a -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "hs_setsockopt"
  c_setsockopt  :: Fd -> CInt -> CInt -> Ptr a -> CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "memset"
  c_memset       :: Ptr a -> CInt -> CSize -> IO ()

foreign import ccall safe "hs_getaddrinfo"
  c_getaddrinfo  :: CString -> CString -> Ptr a -> Ptr (Ptr a) -> IO CInt

foreign import ccall unsafe "hs_freeaddrinfo"
  c_freeaddrinfo :: Ptr a -> IO ()

foreign import ccall safe "hs_getnameinfo"
  c_getnameinfo  :: Ptr a -> CInt -> CString -> CInt -> CString -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "hs_getsockname"
  c_getsockname  :: Fd -> Ptr a -> Ptr CInt -> Ptr CInt -> IO CInt
