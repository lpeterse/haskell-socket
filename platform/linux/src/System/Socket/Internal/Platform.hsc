module System.Socket.Internal.Platform where

import Control.Monad (join)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import GHC.Conc (threadWaitRead, threadWaitWrite)
import System.Posix.Types ( Fd(..) )
import System.Socket.Internal.Message

#include "hs_socket.h"

unsafeSocketWaitWrite :: Fd -> Int -> IO (IO ())
unsafeSocketWaitWrite fd _ = do
  return $ threadWaitWrite fd

-- | Blocks until a socket should be tried for reading.
--
-- > safeSocketWaitRead = do
-- >   wait <- withMVar msock $ \sock-> do
-- >     -- Register while holding a lock on the socket descriptor.
-- >     unsafeSocketWaitRead sock 0
-- >   -- Do the waiting without keeping the socket descriptor locked.
-- >   wait
unsafeSocketWaitRead :: Fd   -- ^ Socket descriptor
               -> Int  -- ^ How many times has it been tried unsuccessfully so far? (currently only relevant on Windows)
               -> IO (IO ()) -- ^ The outer action registers the waiting, the inner does the actual wait.
unsafeSocketWaitRead fd _ = do
  return $ threadWaitRead fd

unsafeSocketWaitConnected :: Fd -> IO ()
unsafeSocketWaitConnected fd = do
  join $ unsafeSocketWaitWrite fd 0

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

foreign import ccall safe "getaddrinfo"
  c_getaddrinfo  :: CString -> CString -> Ptr a -> Ptr (Ptr a) -> IO CInt

foreign import ccall unsafe "freeaddrinfo"
  c_freeaddrinfo :: Ptr a -> IO ()

foreign import ccall safe "getnameinfo"
  c_getnameinfo  :: Ptr a -> CInt -> CString -> CInt -> CString -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "gai_strerror"
  c_gai_strerror  :: CInt -> IO CString
