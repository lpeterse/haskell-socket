--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket.Internal.Platform
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Internal.Platform
  ( waitRead, waitWrite, waitConnected, c_socket, c_close, c_connect,
    c_accept, c_bind, c_listen, c_recv, c_recvfrom, c_send, c_sendto,
    c_freeaddrinfo, c_getaddrinfo, c_getnameinfo, c_memset, c_gai_strerror,
    c_setsockopt, c_getsockopt, c_getsockname) where

import Control.Monad ( when, unless )
import Control.Concurrent.MVar
import Control.Concurrent ( threadWaitRead, threadWaitWrite,
                            threadWaitReadSTM, threadWaitWriteSTM,
                            forkIO, rtsSupportsBoundThreads, killThread )
import Control.Exception ( bracketOnError, throwIO, catch, SomeException(..) )
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import GHC.Conc.Sync ( STM, atomically )
import System.Posix.Types ( Fd(..) )
import System.Socket.Internal.Socket
import System.Socket.Internal.Message
import System.Socket.Internal.Exception

#include "hs_socket.h"

waitRead :: Socket f t p -> Int -> IO ()
waitRead s _ = wait s threadWaitRead threadWaitReadSTM

waitWrite :: Socket f t p -> Int -> IO ()
waitWrite s _ = wait s threadWaitWrite threadWaitWriteSTM

waitConnected :: Socket f t p -> IO ()
waitConnected  = flip waitWrite 0

wait :: Socket f t p -> (Fd -> IO ()) -> (Fd -> IO (STM (), IO ())) -> IO ()
wait (Socket mfd) threadWait threadWaitSTM
  | rtsSupportsBoundThreads = bracketOnError
        ( withMVar mfd $ \fd -> do
            when (fd < 0) (throwIO eBadFileDescriptor)
            threadWaitSTM fd
        )
        snd ( atomically . fst ) `catch` (const (throwIO eBadFileDescriptor) :: IOError -> IO ())
  | otherwise = do
      m <- newEmptyMVar
      bracketOnError
        ( withMVar mfd $ \fd-> do
            when (fd < 0) (throwIO eBadFileDescriptor)
            forkIO $ catch
              ( threadWait fd >> putMVar m True )
              ( \(SomeException _)-> putMVar m False )
        ) killThread
        ( const $ takeMVar m >>= flip unless (throwIO eBadFileDescriptor) )

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

foreign import ccall unsafe "hs_getsockname"
  c_getsockname  :: Fd -> Ptr a -> Ptr CInt -> Ptr CInt -> IO CInt
