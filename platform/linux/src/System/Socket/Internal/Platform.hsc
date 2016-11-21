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

import Control.Monad ( when )
import Control.Concurrent.MVar
import Control.Concurrent ( threadWaitReadSTM, threadWaitWriteSTM )
import Control.Exception ( bracketOnError, mapException, throwIO )
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import GHC.Conc.Sync ( atomically )
import System.Posix.Types ( Fd(..) )
import System.Socket.Internal.Socket
import System.Socket.Internal.Message
import System.Socket.Internal.Exception

#include "hs_socket.h"

waitRead :: Socket f t p -> Int -> IO ()
waitRead (Socket mfd) _ = mapException
  ( const eBadFileDescriptor :: IOError -> SocketException )
  ( bracketOnError
      ( withMVar mfd $ \fd -> do
          when (fd < 0) (throwIO eBadFileDescriptor)
          threadWaitReadSTM fd
      ) snd ( atomically . fst )
  )

waitWrite :: Socket f t p -> Int -> IO ()
waitWrite (Socket mfd) _ = mapException
  ( const eBadFileDescriptor :: IOError -> SocketException )
  ( bracketOnError
      ( withMVar mfd $ \fd -> do
          when (fd < 0) (throwIO eBadFileDescriptor)
          threadWaitWriteSTM fd
      ) snd ( atomically . fst )
  )

waitConnected :: Socket f t p -> IO ()
waitConnected  = flip waitWrite 0

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
