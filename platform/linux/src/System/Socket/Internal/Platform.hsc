module System.Socket.Internal.Platform where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import GHC.Conc (threadWaitReadSTM, threadWaitWriteSTM, atomically)

import System.Posix.Types ( Fd(..) )

import System.Socket.Internal.Msg
import System.Socket.Internal.Exception

socketWaitWrite' :: Fd -> Int -> IO (IO ())
socketWaitWrite' fd _ = do
  threadWaitWriteSTM fd >>= return . atomically . fst

socketWaitRead' :: Fd -> Int -> IO (IO ())
socketWaitRead' fd _ = do
  threadWaitReadSTM fd >>= return . atomically . fst

type CSSize
   = CInt

foreign import ccall unsafe "socket"
  c_socket  :: CInt -> CInt -> CInt -> IO Fd

foreign import ccall unsafe "close"
  c_close   :: Fd -> IO CInt

foreign import ccall unsafe "bind"
  c_bind    :: Fd -> Ptr a -> CInt -> IO CInt

foreign import ccall unsafe "connect"
  c_connect :: Fd -> Ptr a -> CInt -> IO CInt

foreign import ccall unsafe "accept"
  c_accept  :: Fd -> Ptr a -> Ptr CInt -> IO Fd

foreign import ccall unsafe "listen"
  c_listen  :: Fd -> CInt -> IO CInt

foreign import ccall unsafe "send"
  c_send    :: Fd -> Ptr a -> CSize -> MsgFlags -> IO CSSize

foreign import ccall unsafe "sendto"
  c_sendto  :: Fd -> Ptr a -> CSize -> MsgFlags -> Ptr b -> CInt -> IO CSSize

foreign import ccall unsafe "recv"
  c_recv    :: Fd -> Ptr a -> CSize -> MsgFlags -> IO CSSize

foreign import ccall unsafe "recvfrom"
  c_recvfrom :: Fd -> Ptr a -> CSize -> MsgFlags -> Ptr b -> Ptr CInt -> IO CSSize

foreign import ccall unsafe "getsockopt"
  c_getsockopt  :: Fd -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt

foreign import ccall unsafe "setsockopt"
  c_setsockopt  :: Fd -> CInt -> CInt -> Ptr a -> CInt -> IO CInt

foreign import ccall unsafe "hs_setnonblocking"
  c_setnonblocking :: Fd -> IO CInt

foreign import ccall unsafe "hs_get_last_socket_error"
  c_get_last_socket_error :: IO SocketException

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