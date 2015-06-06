{-# LANGUAGE CPP #-}
module System.Socket.Internal.FFI where

import Foreign.Ptr
import Foreign.C.Types

import System.Posix.Types ( Fd(..) )

import System.Socket.Internal.Msg
import System.Socket.Internal.Exception

type CSSize
   = CInt

foreign import ccall unsafe FFI_SOCKET
  c_socket  :: CInt -> CInt -> CInt -> IO Fd

foreign import ccall unsafe FFI_CLOSE
  c_close   :: Fd -> IO CInt

foreign import CALLCONV unsafe "bind"
  c_bind    :: Fd -> Ptr a -> CInt -> IO CInt

foreign import ccall unsafe FFI_CONNECT
  c_connect :: Fd -> Ptr a -> CInt -> IO CInt

foreign import CALLCONV unsafe "accept"
  c_accept  :: Fd -> Ptr a -> Ptr CInt -> IO Fd

foreign import CALLCONV unsafe "listen"
  c_listen  :: Fd -> CInt -> IO CInt

foreign import CALLCONV unsafe "send"
  c_send    :: Fd -> Ptr a -> CSize -> MsgFlags -> IO CSSize

foreign import CALLCONV unsafe "sendto"
  c_sendto  :: Fd -> Ptr a -> CSize -> MsgFlags -> Ptr b -> CInt -> IO CSSize

foreign import ccall unsafe "sendmsg"
  c_sendmsg :: Fd -> Ptr (Msg a t p) -> MsgFlags -> IO CSSize

foreign import CALLCONV unsafe "recv"
  c_recv    :: Fd -> Ptr a -> CSize -> MsgFlags -> IO CSSize

-- socklen_t is an int not a size_t!
foreign import CALLCONV unsafe "recvfrom"
  c_recvfrom :: Fd -> Ptr a -> CSize -> MsgFlags -> Ptr b -> Ptr CInt -> IO CSSize

foreign import ccall unsafe "recvmsg"
  c_recvmsg  :: Fd -> Ptr (Msg a t p) -> MsgFlags -> IO CSSize

foreign import CALLCONV unsafe "getsockopt"
  c_getsockopt  :: Fd -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt

foreign import CALLCONV unsafe "setsockopt"
  c_setsockopt  :: Fd -> CInt -> CInt -> Ptr a -> CInt -> IO CInt

foreign import ccall unsafe "memset"
  c_memset       :: Ptr a -> CInt -> CSize -> IO ()

foreign import ccall unsafe "setnonblocking"
  c_setnonblocking :: Fd -> IO CInt

foreign import ccall unsafe "hs_get_last_socket_error"
  c_get_last_socket_error :: IO SocketException

