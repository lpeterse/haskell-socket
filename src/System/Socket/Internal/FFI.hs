{-# LANGUAGE CPP #-}
module System.Socket.Internal.FFI where

import Foreign.Ptr
import Foreign.C.Types

import System.Posix.Types ( Fd(..) )

import System.Socket.Internal.Msg
import System.Socket.Internal.Exception

type CSSize
   = CInt

foreign import ccall FFI_SOCKET_SAFETY FFI_SOCKET
  c_socket  :: CInt -> CInt -> CInt -> IO Fd

foreign import ccall FFI_CLOSE_SAFETY FFI_CLOSE
  c_close   :: Fd -> IO CInt

foreign import ccall FFI_BIND_SAFETY FFI_BIND
  c_bind    :: Fd -> Ptr a -> CInt -> IO CInt

foreign import ccall FFI_CONNECT_SAFETY FFI_CONNECT
  c_connect :: Fd -> Ptr a -> CInt -> IO CInt

foreign import ccall FFI_ACCEPT_SAFETY FFI_ACCEPT
  c_accept  :: Fd -> Ptr a -> Ptr CInt -> IO Fd

foreign import ccall FFI_LISTEN_SAFETY FFI_LISTEN
  c_listen  :: Fd -> CInt -> IO CInt

foreign import ccall FFI_SEND_SAFETY FFI_SEND
  c_send    :: Fd -> Ptr a -> CSize -> MsgFlags -> IO CSSize

foreign import ccall FFI_SENDTO_SAFETY FFI_SENDTO
  c_sendto  :: Fd -> Ptr a -> CSize -> MsgFlags -> Ptr b -> CInt -> IO CSSize

foreign import ccall FFI_SENDMSG_SAFETY FFI_SENDMSG
  c_sendmsg :: Fd -> Ptr (Msg a t p) -> MsgFlags -> IO CSSize

foreign import ccall FFI_RECV_SAFETY FFI_RECV
  c_recv    :: Fd -> Ptr a -> CSize -> MsgFlags -> IO CSSize

foreign import ccall FFI_RECVFROM_SAFETY FFI_RECVFROM
  c_recvfrom :: Fd -> Ptr a -> CSize -> MsgFlags -> Ptr b -> Ptr CInt -> IO CSSize

foreign import ccall FFI_RECVMSG_SAFETY FFI_RECVMSG
  c_recvmsg  :: Fd -> Ptr (Msg a t p) -> MsgFlags -> IO CSSize

foreign import ccall FFI_GETSOCKOPT_SAFETY FFI_GETSOCKOPT
  c_getsockopt  :: Fd -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt

foreign import ccall FFI_SETSOCKOPT_SAFETY FFI_SETSOCKOPT
  c_setsockopt  :: Fd -> CInt -> CInt -> Ptr a -> CInt -> IO CInt

foreign import ccall unsafe "memset"
  c_memset       :: Ptr a -> CInt -> CSize -> IO ()

foreign import ccall unsafe "setnonblocking"
  c_setnonblocking :: Fd -> IO CInt

foreign import ccall unsafe "hs_get_last_socket_error"
  c_get_last_socket_error :: IO SocketException

