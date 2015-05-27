module System.Socket.Internal.FFI where

import Foreign.Ptr
import Foreign.C.Types

import System.Posix.Types ( Fd(..) )

foreign import ccall unsafe "sys/socket.h socket"
  c_socket  :: CInt -> CInt -> CInt -> IO Fd

foreign import ccall unsafe "unistd.h close"
  c_close   :: Fd -> IO CInt

foreign import ccall unsafe "sys/socket.h bind"
  c_bind    :: Fd -> Ptr a -> Int -> IO CInt

foreign import ccall unsafe "sys/socket.h connect"
  c_connect :: Fd -> Ptr a -> Int -> IO CInt

foreign import ccall unsafe "sys/socket.h accept"
  c_accept  :: Fd -> Ptr a -> Ptr Int -> IO Fd

foreign import ccall unsafe "sys/socket.h listen"
  c_listen  :: Fd -> Int -> IO CInt

foreign import ccall unsafe "sys/socket.h send"
  c_send    :: Fd -> Ptr CChar -> Int -> Int -> IO Int

foreign import ccall unsafe "sys/socket.h sendto"
  c_sendto  :: Fd -> Ptr CChar -> Int -> Int -> Ptr CChar -> Int -> IO Int

foreign import ccall unsafe "sys/socket.h recv"
  c_recv    :: Fd -> Ptr CChar -> Int -> Int -> IO Int

foreign import ccall unsafe "sys/socket.h recvfrom"
  c_recvfrom :: Fd -> Ptr CChar -> Int -> Int -> Ptr CChar -> Ptr Int -> IO Int

foreign import ccall unsafe "sys/socket.h getsockopt"
  c_getsockopt  :: Fd -> CInt -> CInt -> Ptr a -> Ptr Int -> IO CInt

foreign import ccall unsafe "misc.h setnonblocking"
  c_setnonblocking :: Fd -> IO CInt