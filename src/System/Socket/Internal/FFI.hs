module System.Socket.Internal.FFI where

import Foreign.Ptr
import Foreign.C.Types

import System.Posix.Types ( Fd(..) )

import System.Socket.Internal.Msg
import System.Socket.Internal.MsgFlags

type CSSize
   = CInt

foreign import ccall unsafe "sys/socket.h socket"
  c_socket  :: CInt -> CInt -> CInt -> IO Fd

foreign import ccall unsafe "unistd.h close"
  c_close   :: Fd -> IO CInt

foreign import ccall unsafe "sys/socket.h bind"
  c_bind    :: Fd -> Ptr a -> CInt -> IO CInt

foreign import ccall unsafe "sys/socket.h connect"
  c_connect :: Fd -> Ptr a -> CSize -> IO CInt

foreign import ccall unsafe "sys/socket.h accept"
  c_accept  :: Fd -> Ptr a -> Ptr CInt -> IO Fd

foreign import ccall unsafe "sys/socket.h listen"
  c_listen  :: Fd -> CInt -> IO CInt

foreign import ccall unsafe "sys/socket.h send"
  c_send    :: Fd -> Ptr a -> CSize -> MsgFlags -> IO CSSize

foreign import ccall unsafe "sys/socket.h sendto"
  c_sendto  :: Fd -> Ptr a -> CSize -> MsgFlags -> Ptr b -> CInt -> IO CSSize

foreign import ccall unsafe "sys/socket.h sendmsg"
  c_sendmsg :: Fd -> Ptr (Msg a t p) -> MsgFlags -> IO CSSize

foreign import ccall unsafe "sys/socket.h recv"
  c_recv    :: Fd -> Ptr a -> CSize -> MsgFlags -> IO CSSize

-- socklen_t is an int not a size_t!
foreign import ccall unsafe "sys/socket.h recvfrom"
  c_recvfrom :: Fd -> Ptr a -> CSize -> MsgFlags -> Ptr b -> Ptr CInt -> IO CSSize

foreign import ccall unsafe "sys/socket.hs recvmsg"
  c_recvmsg  :: Fd -> Ptr (Msg a t p) -> MsgFlags -> IO CSSize

foreign import ccall unsafe "sys/socket.h getsockopt"
  c_getsockopt  :: Fd -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt

foreign import ccall unsafe "sys/socket.h setsockopt"
  c_setsockopt  :: Fd -> CInt -> CInt -> Ptr a -> CInt -> IO CInt

foreign import ccall unsafe "misc.h setnonblocking"
  c_setnonblocking :: Fd -> IO CInt

foreign import ccall unsafe "string.h memset"
  c_memset       :: Ptr a -> CInt -> CSize -> IO ()